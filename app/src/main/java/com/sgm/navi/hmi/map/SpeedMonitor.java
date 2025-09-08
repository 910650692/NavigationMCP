package com.sgm.navi.hmi.map;

import android.os.SystemClock;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.adapter.position.VehicleSpeedController;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.ISpeedCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

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
    private long lastTime = 0;
    private static final long TIME = 1000;

    public SpeedMonitor() {

    }

    public void registerSpeedCallBack() {
        if (DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
            mSpeedController = VehicleSpeedController.getInstance(AppCache.getInstance().getMContext());
            mSpeedController.registerCallBack(TAG, this);
            mSpeedController.registerCallback();
        } else {
            Logger.e(TAG, "PAD无法使用此功能！");
        }
    }

    public void registerCallBack(CallBack callBack) {
        this.callBack = callBack;
        Logger.i(TAG, "registerCallBack");
    }

    public void removeCallBack() {
        this.callBack = null;
        Logger.i(TAG, "removeCallBack");
    }

    // 处理车速更新的方法 speed单位 km/h
    private void updateSpeed(float speed) {
        if (currentSpeed == speed) {
            return;
        }
        currentSpeed = speed;
        final boolean isReady = ConvertUtils.equals(NaviStatus.NaviStatusType.NO_STATUS, NaviStatusPackage.getInstance().getCurrentNaviStatus());
        if (!isReady) {
            cancelTicket();
            return;
        }
        if (speed >= SPEED_THRESHOLD) {
            if (!isTiming) {
                isTiming = true;
                startSchedule();
            }
        } else {
            cancelTicket();
        }
    }

    @Override
    public void onPulseSpeedChanged(float speed) {
        long currentTime = SystemClock.elapsedRealtime();
        if (currentTime - lastTime >= TIME) {
            updateSpeed(speed);
            lastTime = currentTime;
        }
    }

    public interface CallBack {
        void startCruise();
    }

    public void unInit() {
        cancelTicket();
        removeCallBack();
        if (!ConvertUtils.isNull(mSpeedController)) {
            mSpeedController.unregisterCallback();
            mSpeedController.unRegisterCallBack(TAG);
        }
    }

    private void onTicketEnd() {
        if (passedTime >= TIME_THRESHOLD) {
            Logger.i(TAG, "onTicketEnd tickNum: ", passedTime);
            cancelTicket();
            ThreadManager.getInstance().postUi(() -> {
                if (callBack != null) {
                    callBack.startCruise();
                } else {
                    Logger.w(TAG, "回调为空，无法开始巡航！");
                }
            });
        }
    }

    /***
     * 放在子线程执行
     */
    private void startSchedule() {
        try {
            scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                passedTime++;
                onTicketEnd();
            }, 0, INTERVAL_TIME, TimeUnit.SECONDS);
        } catch (Exception e) {
            isTiming = false;
            Logger.e(TAG, "startSchedule failed:" + e.getMessage());
        }
    }

    private void cancelTicket() {
        isTiming = false;
        passedTime = 0;
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                scheduledFuture.cancel(true);
            }
        } catch (Exception e) {
            Logger.d(TAG, "cancelTicket error:" + e.getMessage());
        } finally {
            scheduledFuture = null;
        }
    }
}
