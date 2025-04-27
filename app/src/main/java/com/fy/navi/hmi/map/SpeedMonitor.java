package com.fy.navi.hmi.map;

import android.os.CountDownTimer;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.position.VehicleSpeedController;
import com.fy.navi.service.define.position.ISpeedCallback;

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
    private static final long INTERVAL_TIME = 1000L;
    private static final long TOTAL_TIME = 15*1000L;
    private CallBack callBack;
    private VehicleSpeedController mSpeedController;
    private CountDownTimer countDownTimer = new CountDownTimer(TOTAL_TIME, INTERVAL_TIME) {
        @Override
        public void onTick(long millisUntilFinished) {

        }

        @Override
        public void onFinish() {
            Logger.i(TAG, "CountDownTimer --- onFinish !");
            isTiming = false;
            if (callBack != null) {
                callBack.startCruise();
            }
        }
    };

    public SpeedMonitor() {

    }

    public void registerSpeedCallBack() {
        if (DeviceUtils.isCar(AppContext.getInstance().getMContext())) {
            mSpeedController = new VehicleSpeedController(AppContext.getInstance().getMContext(), this);
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
        Logger.i(TAG, "updateSpeed:" + speed);
        if (speed >= SPEED_THRESHOLD) {
            if (!isTiming) {
                isTiming = true;
                countDownTimer.start();
            } else {
                Logger.i(TAG, "正在计时当中...");
            }
        } else {
            isTiming = false;
            countDownTimer.cancel();
        }
    }

    @Override
    public void onSpeedChanged(float speed) {
        updateSpeed(speed);
    }

    public interface CallBack {
        void startCruise();
    }

    public void unInit() {
        if (countDownTimer != null) {
            isTiming = false;
            countDownTimer.cancel();
            removeCallBack();
        }
        if (!ConvertUtils.isNull(mSpeedController)) {
            mSpeedController.unregisterCallback();
        }
    }
}
