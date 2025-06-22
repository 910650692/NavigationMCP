package com.sgm.navi.service.utils;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.calibration.PowerType;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.concurrent.ScheduledFuture;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/3/25
 * Description: [油车油量低提醒帮助类]
 */
public class GasCarTipManager {
    private static final String TAG = "GasCarTipManager";
    private final long INTERVAL_TIME = 60;//单位：秒
    private static final float EDGE_DISTANCE = 50;// 单位：KM
    private boolean mMonitorOnGoing = false;
    private final CalibrationPackage mCalibrationPackage;
    private final SignalPackage mSignalPackage;
    private GasCarTipManager() {
        mCalibrationPackage = CalibrationPackage.getInstance();
        mSignalPackage = SignalPackage.getInstance();
    }

    // 导航过程中每分钟检查一下剩余油量
    private ScheduledFuture scheduledFuture;
    public static GasCarTipManager getInstance(){
        return Helper.gct;
    }

    /***
     * 获取到达目的地后剩余燃油量百分比，可以为负值
     * @param distance 距离目的地的距离,单位：km
     * @return 剩余燃油量百分比, 例如：0.55
     */
    public int getRemainGasPercent(final float distance) {
        if (isNeedTip(distance)) {
            final float remainDis = getRemainDistance();
            Logger.i(TAG, "getRemainGasPercent", "distance:" , distance, "remainDis:" , remainDis);
            if (remainDis <= 0) {
                return 0;
            }
            return (int) ((remainDis - distance) / remainDis);
        } else {
            return 0;
        }
    }

    /***
     * 开启油量监视器
     */
    public void openMonitor() {
        final boolean isNeedGas = isGasCar();
        Logger.i(TAG, "startMonitor", "mMonitorOnGoing:" , mMonitorOnGoing, "isNeedGas:" , isNeedGas);
        if (isNeedGas && !mMonitorOnGoing) {
            startSchedule();
            mMonitorOnGoing = true;
        }
    }

    /***
     * 关闭油量监视器
     */
    public void closeMonitor() {
        Logger.i(TAG, "closeMonitor", "mMonitorOnGoing:" , mMonitorOnGoing);
        if (mMonitorOnGoing) {
            stopSchedule();
            mMonitorOnGoing = false;
        }
    }

    /***
     *
     * @return true是否需要汽油
     */
    private boolean isGasCar() {
        if (!DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
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
        if (!DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
            Logger.w(TAG, "请在实车上测试！");
            return false;
        }
        final float canRunDistance = getRemainDistance();
        final boolean isNeedTip = canRunDistance - distance < EDGE_DISTANCE;
        return isGasCar() && isNeedTip;
    }

    public void unInit() {
        stopSchedule();
    }

    private void startSchedule() {
        try {
            scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                // TODO
            }, 0, INTERVAL_TIME);
        } catch (Exception e) {
            Logger.e(TAG, "startSchedule failed:" + e.getMessage());
        }
    }

    private void stopSchedule() {
        try {
            if (!ConvertUtils.isNull(scheduledFuture) && !scheduledFuture.isDone()) {
                final boolean cancelResult = scheduledFuture.cancel(true);
                Logger.i(TAG, "stopSchedule:" , cancelResult);
            } else {
                Logger.w(TAG, "stopSchedule not need do, scheduledFuture is null or had completed!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "stopSchedule failed:" + e.getMessage());
        }
    }

    private static final class Helper{
        private static final GasCarTipManager gct = new GasCarTipManager();
    }
}
