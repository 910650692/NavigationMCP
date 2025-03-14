package com.fy.navi.hmi.map;

import android.os.CountDownTimer;

import com.android.utils.log.Logger;

/**
 * Author: QiuYaWei
 * Date: 2025/3/4
 * Description: [车速监测何时进入巡航，条件：车速大于15Km/h且超过10s]
 */
// 车速监控类
public class SpeedMonitor {
    private static final String TAG = "SpeedMonitor";
    // 标记是否正在计时
    private boolean isTiming = false;
    private static final float SPEED_THRESHOLD = 15;
    private static final long INTERVAL_TIME = 1000L;
    private static final long TOTAL_TIME = 15*1000L;
    private CallBack callBack;
    private CountDownTimer countDownTimer = new CountDownTimer(TOTAL_TIME, INTERVAL_TIME) {
        @Override
        public void onTick(long millisUntilFinished) {

        }

        @Override
        public void onFinish() {
            Logger.i(TAG, "CountDownTimer --- onFinish !");
            if (callBack != null) {
                callBack.startCruise();
            }
        }
    };

    public void registerCallBack(CallBack callBack) {
        this.callBack = callBack;
    }

    public void removeCallBack() {
        this.callBack = null;
    }

    // 处理车速更新的方法 speed单位 km/h
    public void updateSpeed(float speed) {
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

    public interface CallBack {
        void startCruise();
    }

    public void unInit() {
        if (countDownTimer != null) {
            isTiming = false;
            countDownTimer.cancel();
            removeCallBack();
        }
    }
}
