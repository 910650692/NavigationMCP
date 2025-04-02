package com.fy.navi.hmi.map;

import android.os.CountDownTimer;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.calibration.PowerType;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/25
 * Description: [油车油量低提醒帮助类]
 */
public class GasCarTipManager {
    private static final String TAG = "GasCarTipManager";
    private static final long INTERVAL = 60 * 1000;
    private static final float EDGE_DISTANCE = 50;// 单位：KM
    private boolean mMonitorOnGoing = false;
    private final CalibrationPackage mCalibrationPackage;
    private final SignalPackage mSignalPackage;
    public GasCarTipManager() {
        mCalibrationPackage = CalibrationPackage.getInstance();
        mSignalPackage = SignalPackage.getInstance();
    }

    // 导航过程中每分钟检查一下剩余油量
    private final CountDownTimer mCountDownTimer = new CountDownTimer(Long.MAX_VALUE, INTERVAL) {
        @Override
        public void onTick(final long millisUntilFinished) {
            Logger.i(TAG, "onTick set battery!");
        }

        @Override
        public void onFinish() {
            mMonitorOnGoing = false;
            Logger.i(TAG, "CountDownTimer --- onFinish !");
        }
    };

    /***
     * 获取到达目的地后剩余燃油量百分比，可以为负值
     * @param distance 距离目的地的距离,单位：km
     * @return 剩余燃油量百分比, 例如：0.55
     */
    public float getRemainGasPercent(final float distance) {
        if (isNeedTip(distance)) {
            final float remainDis = getRemainDistance();
            Logger.i(TAG, "getRemainGasPercent", "distance:" + distance, "remainDis:" + remainDis);
            if (remainDis <= 0) {
                return 0f;
            }
            return (remainDis - distance) / remainDis;
        } else {
            return 0f;
        }
    }

    /***
     * 开启油量监视器
     */
    public void openMonitor() {
        final boolean isNeedGas = isGasCar();
        Logger.i(TAG, "startMonitor", "mMonitorOnGoing:" + mMonitorOnGoing, "isNeedGas:" + isNeedGas);
        if (isNeedGas && !mMonitorOnGoing) {
            mCountDownTimer.start();
            mMonitorOnGoing = true;
        }
    }

    /***
     * 关闭油量监视器
     */
    public void closeMonitor() {
        Logger.i(TAG, "closeMonitor", "mMonitorOnGoing:" + mMonitorOnGoing);
        if (mMonitorOnGoing) {
            mCountDownTimer.cancel();
            mMonitorOnGoing = false;
        }
    }

    /***
     *
     * @return true是否需要汽油
     */
    private boolean isGasCar() {
        if (!DeviceUtils.isCar(AppContext.getInstance().getMContext())) {
            Logger.w(TAG, "请在实车上测试！");
            return false;
        }
        return mCalibrationPackage.powerType() == PowerType.E_VEHICLE_ENERGY_FUEL
                || mCalibrationPackage.powerType() == PowerType.E_VEHICLE_ENERGY_HYBRID;
    }

    /***
     *
     * @return 返回剩余油量可以跑的距离，单位：KM
     */
    private float getRemainDistance() {
        return mSignalPackage.getRangeRemaining();
    }

    /***
     *
     * @param distance
     * @return 是否提示油量不足
     */
    public boolean isNeedTip(final float distance) {
        if (!DeviceUtils.isCar(AppContext.getInstance().getMContext())) {
            Logger.w(TAG, "请在实车上测试！");
            return false;
        }
        final float canRunDistance = getRemainDistance();
        final boolean isNeedTip = canRunDistance - distance < EDGE_DISTANCE;
        return isGasCar() && isNeedTip;
    }

    public void unInit() {
        if (mMonitorOnGoing) {
            mCountDownTimer.cancel();
        }
    }
}
