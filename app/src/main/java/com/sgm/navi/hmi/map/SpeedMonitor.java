package com.sgm.navi.hmi.map;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.adapter.position.VehicleSpeedController;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.ISpeedCallback;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * Author: QiuYaWei
 * Date: 2025/3/4
 * Description: [车速监测何时进入巡航，条件：车速大于15Km/h且超过10s]
 */
// 车速监控类
public class SpeedMonitor implements ISpeedCallback {
    private static final String TAG = "SpeedMonitor";
    // 标记是否正在计时
    private boolean isTiming = false;
    private static final float SPEED_THRESHOLD = 15;
    private static final long INTERVAL_TIME = 1;//单位：秒
    private static final long TIME_THRESHOLD = 10;//单位：秒
    private long passedTime;
    private CallBack callBack;
    private VehicleSpeedController mSpeedController;
    private ScheduledFuture scheduledFuture;
    private float currentSpeed;

    public SpeedMonitor() {

    }

    public void registerSpeedCallBack() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
            mSpeedController = new VehicleSpeedController(AppCache.getInstance().getMContext(), this);
            mSpeedController.registerCallback();
        } else {
            Logger.e(TAG, "PAD无法使用此功能！");
        }
    }

    public void registerCallBack(CallBack callBack) {
        this.callBack = callBack;
    }

    public void removeCallBack() {
        this.callBack = null;
    }

    // 处理车速更新的方法 speed单位 km/h
    private void updateSpeed(float speed) {
        if (currentSpeed == speed) {
            Logger.d(TAG, "车速没有发生变化！");
            return;
        }
        currentSpeed = speed;
        final boolean isReady = ConvertUtils.equals(NaviStatus.NaviStatusType.NO_STATUS, NaviPackage.getInstance().getCurrentNaviType());
        if (!isReady) {
            Logger.d(TAG, "当前状态无需计时");
            cancelTicket();
            return;
        }
        if (speed >= SPEED_THRESHOLD) {
            if (!isTiming) {
                startSchedule();
            } else {
                Logger.d(TAG, "正在计时当中...");
            }
        } else {
            cancelTicket();
        }
    }

    @Override
    public void onPulseSpeedChanged(float speed) {
        updateSpeed(speed);
    }

    public interface CallBack {
        void startCruise();
    }

    public void unInit() {
        cancelTicket();
        removeCallBack();
        if (!ConvertUtils.isNull(mSpeedController)) {
            mSpeedController.unregisterCallback();
        }
    }

    private void onTicketEnd() {
        if (passedTime >= TIME_THRESHOLD) {
            Logger.i(TAG, "onTicketEnd tickNum: ", passedTime);
            cancelTicket();
            if (callBack != null) {
                ThreadManager.getInstance().postUi(() -> {
                    callBack.startCruise();
                });
            } else {
                Logger.w(TAG, "回调为空，无法开始巡航！");
            }
        }
    }

    /***
     * 放在子线程执行
     */
    private void startSchedule() {
        ThreadManager.getInstance().execute(() -> {
            Logger.i(TAG, "startSchedule:", isTiming);
            try {
                isTiming = true;
                scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                    passedTime++;
                    onTicketEnd();
                }, 0, INTERVAL_TIME, TimeUnit.SECONDS);
            } catch (Exception e) {
                isTiming = false;
                Logger.e(TAG, "startSchedule failed:" + e.getMessage());
            }
        });
    }

    private void cancelTicket() {
        isTiming = false;
        passedTime = 0;
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                boolean cancelResult = scheduledFuture.cancel(true);
                Logger.i(TAG, "cancelTicket:", cancelResult);
            } else {
                Logger.d(TAG, "cancelTicket failed: scheduledFuture is null or has completed!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "cancelTicket error:" + e.getMessage());
        }
    }
}
