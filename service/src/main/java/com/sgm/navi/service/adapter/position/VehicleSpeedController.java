package com.sgm.navi.service.adapter.position;

import android.car.Car;
import android.car.VehiclePropertyIds;
import android.car.hardware.CarPropertyConfig;
import android.car.hardware.CarPropertyValue;
import android.car.hardware.property.CarPropertyManager;
import android.car.hardware.property.CarPropertyManager.CarPropertyEventCallback;
import android.content.Context;
import android.os.SystemClock;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.position.ISpeedCallback;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

/*车速管理类*/
public class VehicleSpeedController {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private CarPropertyManager carPropertyManager;
    private static VehicleSpeedController sInstance;
    private Context mContext;
    private Car car;
    private final ConcurrentHashMap<String, ISpeedCallback> mISpeedCallbackMap = new ConcurrentHashMap<>();
//    private ISpeedCallback mISpeedCallback;
    // 定义米每秒到千米每小时的转换因子
    public static final double MPS_TO_KPH_FACTOR = 3.6;

    private long lastTime = 0;
    private float lastSpeed = 0.0f;
    private static final long TIME = 1000;
    private final ExecutorService mSpeedUpdateExecutor = Executors.newSingleThreadExecutor();
    // 私有构造函数
    private VehicleSpeedController() {
    }

    /**
     * 获取单例实例
     * @param context 上下文
     * @return VehicleSpeedController实例
     */
    public static synchronized VehicleSpeedController getInstance(Context context) {
        if (sInstance == null) {
            sInstance = new VehicleSpeedController();
        }
        if (sInstance.mContext == null && context != null) {
            sInstance.mContext = context.getApplicationContext();
            sInstance.initCarManager();
        }
        return sInstance;
    }

    /**
     * 初始化Car管理器
     */
    private void initCarManager() {
        try {
            Logger.d(TAG, "initCarManager");
            // 初始化 Car API
            if (mContext != null) {
                car = Car.createCar(mContext);
                if (car != null) {
                    carPropertyManager = (CarPropertyManager) car.getCarManager(Car.PROPERTY_SERVICE);
                }
            }
        } catch (Exception e) {
            Logger.e(TAG, "initCarManager error: " + e);
        }
    }

    public void registerCallback() {
        if (carPropertyManager == null) {
            Logger.e(TAG, "carPropertyManager ==null: ");
            return;
        }
        // 获取车速属性配置
        CarPropertyConfig<?> carPropertyConfig = carPropertyManager.getCarPropertyConfig(VehiclePropertyIds.PERF_VEHICLE_SPEED);
        if (carPropertyConfig == null) {
            Logger.e(TAG, "speedConfig == null");
            return;
        }
        // 注册监听器
        if (carPropertyManager != null) {
            carPropertyManager.unregisterCallback(speedCallback);
            boolean result = carPropertyManager.registerCallback(speedCallback, VehiclePropertyIds.PERF_VEHICLE_SPEED, 10);
            Logger.d(TAG, "result: ", result);
        }
    }

    private final CarPropertyEventCallback speedCallback = new CarPropertyEventCallback() {

        @Override
        public void onChangeEvent(CarPropertyValue event) {
            if (event.getPropertyId() == VehiclePropertyIds.PERF_VEHICLE_SPEED) {
                // 提交到单线程池处理，避免频繁创建线程
                mSpeedUpdateExecutor.execute(() -> {
                    float speed = (float) event.getValue();
                    // 处理车速变化
                    onSpeedChanged(speed);
                });
            }
        }

        @Override
        public void onErrorEvent(int propertyId, int zone) {
            // 处理错误
            Logger.e(TAG, "Error occurred for property ID: ", propertyId);
        }
    };

    /**
     * @param speed m/s
     */
    private void onSpeedChanged(float speed) {
        // 更新 UI 或处理车速数据
        if(Logger.openLog){
            long currentTime = SystemClock.elapsedRealtime();
            if (currentTime - lastTime >= TIME && lastSpeed != speed) {
                // 只有速度发生变化时才打印日志
                Logger.d(TAG, "Current speed: %f m/s", speed);
                lastTime = currentTime;
                lastSpeed = speed;
            }
        }
        for (Map.Entry<String, ISpeedCallback> entry : mISpeedCallbackMap.entrySet()) {
            final ISpeedCallback callback = entry.getValue();
            callback.onPulseSpeedChanged((float) (speed * MPS_TO_KPH_FACTOR));
        }
    }

    public void unregisterCallback() {
        if (carPropertyManager != null) {
            carPropertyManager.unregisterCallback(speedCallback);
        }
        if (car != null) {
            car.disconnect();
        }
    }

    @Override
    protected void finalize() throws Throwable {
        super.finalize();
        Logger.i(TAG, "finalize:");
        unregisterCallback();
        // 关闭线程池
        if (mSpeedUpdateExecutor != null && !mSpeedUpdateExecutor.isShutdown()) {
            mSpeedUpdateExecutor.shutdown();
        }
    }

    /**
     * 注册 HMI 速度回调
     * @param callbackId 回调唯一标识
     * @param callback 回调接口
     */
    public void registerCallBack(final String callbackId, final ISpeedCallback callback) {
        if (callback == null || callbackId == null) {
            Logger.e(TAG, "Failed to register callback: callback or identifier is null.");
            return;
        }
        if (!mISpeedCallbackMap.containsKey(callbackId)) {
            mISpeedCallbackMap.put(callbackId, callback);
        } else {
            Logger.w(MapDefaultFinalTag.SEARCH_SERVICE_TAG, "Callback with identifier {} already registered.", callbackId);
        }
    }

    /**
     * 注销 HMI 速度回调
     *
     * @param callbackId 回调唯一标识
     */
    public void unRegisterCallBack(final String callbackId) {
        if (callbackId == null) {
            Logger.e(TAG, "Failed to unregister callback: identifier is null.");
            return;
        }
        mISpeedCallbackMap.remove(callbackId);
    }
}
