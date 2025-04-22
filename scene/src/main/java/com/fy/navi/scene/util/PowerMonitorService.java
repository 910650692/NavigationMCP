package com.fy.navi.scene.util;

import android.os.CountDownTimer;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.callback.OnPowerChangeListener;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.calibration.PowerType;
import com.fy.navi.service.logicpaket.signal.SignalCallback;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description: [电量监视器]
 */
public class PowerMonitorService implements SignalCallback {
    private static final String TAG = "PowerMonitorService";
    private static final long TOTAL_TIME = Long.MAX_VALUE;
    private static final long INTERVAL = 2 * 1000 * 60L;//间隔事件2分钟
    private static final float EDGE_DISTANCE = 50f; // 续航里程小于50KM提醒
    private final boolean mDeviceIsCar;
    private final boolean mIsPureGasCar; // 是否是纯油车
    private final CopyOnWriteArrayList<OnPowerChangeListener> mPowerChangeListener;
    private final SignalPackage mSignalPackage;
    private final CalibrationPackage mCalibrationPackage;
    private final CountDownTimer mCountDownTimer = new CountDownTimer(TOTAL_TIME, INTERVAL) {
        @Override
        public void onTick(long millisUntilFinished) {
            checkRemainBattery();
        }

        @Override
        public void onFinish() {
            Logger.i(TAG, "onFinish");
        }
    };

    public PowerMonitorService() {
        mSignalPackage = SignalPackage.getInstance();
        mDeviceIsCar = DeviceUtils.isCar(AppContext.getInstance().getMContext());
        mPowerChangeListener = new CopyOnWriteArrayList<>();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mIsPureGasCar = mCalibrationPackage.powerType() == PowerType.E_VEHICLE_ENERGY_FUEL;
    }

    public void startSchedule() {
        if (mDeviceIsCar) {
            mCountDownTimer.start();
        } else {
            Logger.w(TAG, "不是汽车，无需执行定时器！");
        }
    }

    public void stopSchedule() {
        Logger.i(TAG, "stopSchedule");
        if (mCountDownTimer != null) {
            mCountDownTimer.cancel();
        }
    }

    public void registerListener(final OnPowerChangeListener listener) {
        if (!mPowerChangeListener.contains(listener)) {
            mPowerChangeListener.add(listener);
            Logger.i(TAG, "registerListener success!");
        } else {
            Logger.i(TAG, "not need repeat register again!");
        }
    }

    public void unRegisterListener(final OnPowerChangeListener listener) {
        if (!ConvertUtils.isEmpty(mPowerChangeListener) && !ConvertUtils.isNull(listener)) {
            mPowerChangeListener.remove(listener);
            Logger.i(TAG, "unRegisterListener success!");
        } else {
            Logger.i(TAG, "unRegisterListener failed!");
        }
    }

    /***
     *a) 续航里程小于等于90km且大于50km时，LBS系统需要行驶5分钟km检测一次周边可用充电站；
     * b) 续航里程小于等于50km时，LBS系统需要每行驶2分钟km检测一次周边可用充电站
     */
    private void checkRemainBattery() {
        // 剩余汽油可以跑到距离
        final float gasRemainCanRunDis = mSignalPackage.getRangeRemaining();
        // 剩余电量可以跑的距离
        final float electRemainCanRunDis = mSignalPackage.getHighVoltageBatteryPropulsionRange();
        final float totalCanRunDis = gasRemainCanRunDis + electRemainCanRunDis;
        if (mIsPureGasCar) {
            gasCarDistanceChange(totalCanRunDis);
        } else {
            electricCarDistanceChange(totalCanRunDis);
        }
    }

    /***
     * 油车
     * @param totalCanRunDis
     */
    private void gasCarDistanceChange(final float totalCanRunDis) {
        if (totalCanRunDis <= EDGE_DISTANCE) {
            Logger.i(TAG, "gasCarDistanceChange");
            mPowerChangeListener.forEach(listener -> {
                listener.onGasLowerNotify();
            });
        } else {
            Logger.i(TAG, "gasCarDistanceChange, not need callBack!", "totalCanRunDis:" + totalCanRunDis + "KM");
        }
    }

    /***
     * 电动车或者混动车
     * @param totalCanRunDis
     */
    private void electricCarDistanceChange(final float totalCanRunDis) {
        if (totalCanRunDis <= EDGE_DISTANCE) {
            Logger.i(TAG, "electricCarDistanceChange");
            mPowerChangeListener.forEach(listener -> {
                listener.onElectricLowerNotify();
            });
        } else {
            Logger.i(TAG, "electricCarDistanceChange, not need callBack!", "totalCanRunDis:" + totalCanRunDis + "KM");
        }
    }

    public void mockTest() {
        gasCarDistanceChange(EDGE_DISTANCE - 20);
    }
}
