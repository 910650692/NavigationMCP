package com.sgm.navi.scene.util;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.callback.OnPowerChangeListener;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.define.calibration.PowerType;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ScheduledFuture;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/31
 * Description: [电量监视器]
 */
public class PowerMonitorService implements SignalCallback {
    private static final String TAG = "PowerMonitorService";
    private static final long INTERVAL = 120;//间隔事件2分钟即120秒
    private static final float EDGE_DISTANCE = 48f; // 续航里程小于48KM提醒
    private final boolean mIsPureEVCar; // 是否纯电车
    private final CopyOnWriteArrayList<OnPowerChangeListener> mPowerChangeListener;
    private final SignalPackage mSignalPackage;
    private final CalibrationPackage mCalibrationPackage;
    private ScheduledFuture scheduledFuture;

    public PowerMonitorService() {
        mSignalPackage = SignalPackage.getInstance();
        mPowerChangeListener = new CopyOnWriteArrayList<>();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mIsPureEVCar = mCalibrationPackage.powerType() == PowerType.E_VEHICLE_ENERGY_ELECTRIC;
    }

    public void startSchedule() {
        ThreadManager.getInstance().execute(() -> {
            try {
                if (isOnSchedule()) {
                    Logger.i(TAG, "任务已开始，无需重复启动！");
                    return;
                }
                scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                    checkRemainBattery();
                }, 0, INTERVAL);
            } catch (Exception e) {
                Logger.e(TAG, "startSchedule failed:" + e.getMessage());
            }
        });
    }

    public void stopSchedule() {
        Logger.i(TAG, "stopSchedule");
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                final boolean cancelResult = scheduledFuture.cancel(true);
                Logger.i(TAG, "stopSchedule:" + cancelResult);
            } else {
                Logger.i(TAG, "stopSchedule failed, scheduledFuture is null or had completed");
            }
        } catch (Exception e) {
            Logger.e(TAG, "stopSchedule failed:" + e.getMessage());
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
        final long startTime = System.currentTimeMillis();
        // 剩余汽油可以跑到距离
        final float gasRemainCanRunDis = mSignalPackage.getRangeRemaining();
        // 剩余电量可以跑的距离
        final float electRemainCanRunDis = mSignalPackage.getHighVoltageBatteryPropulsionRange();
        final float totalCanRunDis = gasRemainCanRunDis + electRemainCanRunDis;
        if (mIsPureEVCar) {
            electricCarDistanceChange(electRemainCanRunDis);
        } else {
            gasCarDistanceChange(gasRemainCanRunDis);
        }
        final double costTime = (System.currentTimeMillis() - startTime) / 1000f;
        Logger.d(TAG, "checkRemainBattery", "costTime", costTime, "秒", "gasRemainCanRunDis:", gasRemainCanRunDis,
                "electRemainCanRunDis:", electRemainCanRunDis, "totalCanRunDis:", totalCanRunDis);
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

    private boolean isOnSchedule() {
        try {
            if (ConvertUtils.isNull(scheduledFuture)) return false;
            return !scheduledFuture.isDone();
        } catch (Exception e) {
            return false;
        }
    }

    public void mockTest() {
        gasCarDistanceChange(EDGE_DISTANCE - 20);
    }
}
