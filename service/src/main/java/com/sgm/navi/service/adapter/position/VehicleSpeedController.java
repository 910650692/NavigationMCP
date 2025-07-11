package com.sgm.navi.service.adapter.position;

import android.car.Car;
import android.car.VehiclePropertyIds;
import android.car.hardware.CarPropertyConfig;
import android.car.hardware.CarPropertyValue;
import android.car.hardware.property.CarPropertyManager;
import android.car.hardware.property.CarPropertyManager.CarPropertyEventCallback;
import android.content.Context;

import com.android.utils.log.Logger;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.position.ISpeedCallback;

/*车速管理类*/
public class VehicleSpeedController {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private CarPropertyManager carPropertyManager;
    private Car car;
    private ISpeedCallback mISpeedCallback;
    // 定义米每秒到千米每小时的转换因子
    public static final double MPS_TO_KPH_FACTOR = 3.6;

    public VehicleSpeedController(Context context, ISpeedCallback callback) {
        try {
            Logger.d(TAG, "VehicleSpeedController");
            // 初始化 Car API
            car = Car.createCar(context);
            if (car != null) {
                carPropertyManager = (CarPropertyManager) car.getCarManager(Car.PROPERTY_SERVICE);
            }
        } catch (Exception e) {
            Logger.e(TAG, "e：" + e);
        }
        mISpeedCallback = callback;
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
        boolean result = carPropertyManager.registerCallback(speedCallback, VehiclePropertyIds.PERF_VEHICLE_SPEED, 10);
        Logger.d(TAG, "result: ", result);
    }

    private final CarPropertyEventCallback speedCallback = new CarPropertyEventCallback() {

        @Override
        public void onChangeEvent(CarPropertyValue event) {
            if (event.getPropertyId() == VehiclePropertyIds.PERF_VEHICLE_SPEED) {
                float speed = (float) event.getValue();
                // 处理车速变化
                onSpeedChanged(speed);
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
        Logger.d(TAG, "Current speed: %f m/s", speed);
        if (mISpeedCallback != null) {
            mISpeedCallback.onPulseSpeedChanged((float) (speed * MPS_TO_KPH_FACTOR));
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
    }
}
