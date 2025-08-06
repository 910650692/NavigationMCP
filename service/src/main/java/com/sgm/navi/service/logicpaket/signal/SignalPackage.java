package com.sgm.navi.service.logicpaket.signal;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.JsonLog;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.adapter.signal.SignalAdapter;
import com.sgm.navi.service.adapter.signal.SignalAdapterCallback;
import com.sgm.navi.service.define.signal.RoadConditionGroup;
import com.sgm.navi.service.define.signal.SdNavigationStatusGroup;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;

import java.util.Hashtable;

public final class SignalPackage implements SignalAdapterCallback {
    public static final String TAG = SignalPackage.class.getSimpleName();

    private final SignalAdapter mSignalAdapter;
    private Hashtable<String, SignalCallback> mSignalCallbacks;

    public static SignalPackage getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    private static final class SInstanceHolder {
        static final SignalPackage INSTANCE = new SignalPackage();
    }

    private SignalPackage() {
        mSignalCallbacks = new Hashtable<>();
        mSignalAdapter = SignalAdapter.getInstance();
    }

    /**
     * 初始化
     * @param context
     */
    public void init(final Context context) {
        ThreadManager.getInstance().postUi(() -> mSignalAdapter.initSignal(context));
        mSignalAdapter.registerCallback(TAG, this);
    }

    /**
     * 注册回调
     * @param key 回调key
     * @param signalCallback 回调
     */
    public void registerObserver(final String key, final SignalCallback signalCallback) {
        mSignalCallbacks.put(key, signalCallback);
    }

    /**
     * 取消回调
     * @param key 回调key
     */
    public void unregisterObserver(final String key) {
        mSignalCallbacks.remove(key);
    }

    @Override
    public void onSpeedChanged(final float speed) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onSpeedChanged(speed);
                    }
                }
            }
        });
    }

    @Override
    public void onGearChanged(final int gear) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onGearChanged(gear);
                    }
                }
            }
        });
    }

    @Override
    public void onSystemStateChanged(final int state) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onSystemStateChanged(state);
                    }
                }
            }
        });
    }

    @Override
    public void onRangeRemainingSignalChanged(final float value) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onRangeRemainingSignalChanged(value);
                    }
                }
            }
        });
    }

    @Override
    public void onHighVoltageBatteryPropulsionRangeChanged(final float value) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onHighVoltageBatteryPropulsionRangeChanged(value);
                    }
                }
            }
        });
    }

    @Override
    public void onLaneCenteringWarningIndicationRequestIdcmAChanged(final int state) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onLaneCenteringWarningIndicationRequestIdcmAChanged(state);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviOnADASStateChanged(int state) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onNaviOnADASStateChanged(state);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviVolumeChanged(int volume) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onNaviVolumeChanged(volume);
                    }
                }
            }
        });
    }

    @Override
    public void onFuelLevelPercentSignalChanged(Float value) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onFuelLevelPercentSignalChanged(value);
                    }
                }
            }
        });
    }

    @Override
    public void onPredictedFuelSavingPer100km(int value) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onPredictedFuelSavingPer100km(value);
                    }
                }
            }
        });
    }

    @Override
    public void onTotalFuelSaving(int value) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isEmpty(mSignalCallbacks)) {
                for (SignalCallback signalCallback : mSignalCallbacks.values()) {
                    if (signalCallback != null) {
                        signalCallback.onTotalFuelSaving(value);
                    }
                }
            }
        });
    }

    /**
     * 车外温度
     *
     * @return 单位°C
     */
    public float getOutsideTemperature() {
        return mSignalAdapter.getOutsideTemperature();
    }

    /**
     * 车速
     *
     * @return 单位m/s
     */
    public float getSpeedOfVehicle() {
        return mSignalAdapter.getSpeedOfVehicle();
    }

    /**
     * 电池剩余电量
     *
     * @return 单位kWh
     */
    public float getBatteryEnergy() {
        return mSignalAdapter.getBatteryEnergy();
    }

    /**
     * 电池系统当前剩余电量百分比
     *
     * @return 单位%
     */
    public float getBatteryEnergyPercent() {
        return mSignalAdapter.getBatteryEnergyPercent();
    }

    /**
     * 电池系统总的电池容量
     *
     * @return 单位kwh
     */
    public float getMaxBatteryEnergy() {
        return mSignalAdapter.getMaxBatteryEnergy() / 1000;
    }

    /**
     * 充电系统状态
     *
     * @return 充电状态
     * DEFAULT = 0;
     * IDLE = 1;
     * INITIALIZING = 2;
     * ACTIVE = 3;
     * COMPLETE = 4;
     * ABORTED = 5;
     * UTILITY_OVERRIDE_ACTIVE = 6;
     * UTILITY_OVERRIDE_REDUCED_POWER = 7;
     * PAUSE_DUE_TO_UPDATE = 8;
     * CONNECTION_UNPOWERED = 9;
     * UNCONNECTED = 10;
     * OFFBOARD_ENERGY_TRANSFER_ACTIVE = 11;
     */
    public int getChargeSystemStatus() {
        return mSignalAdapter.getChargeSystemStatus();
    }

    /**
     * 空调开关状态
     *
     * @return 0:关闭 1:开启
     */
    public int getAcSwitchState() {
        return mSignalAdapter.getAcSwitchState();
    }

    /**
     * 系统状态
     * @return 系统状态
     * OFF = 0
     * ACC = 1
     * RUN = 2
     * CRANK(START) = 3
     * SLEEP = 4
     */
    public int getSystemState() {
        return mSignalAdapter.getSystemState();
    }

    /**
     * 油量续航里程信息
     *
     * @return 单位km
     */
    public float getRangeRemaining() {
        return mSignalAdapter.getRangeRemaining();
    }

    /**
     * 高压电池续航里程
     *
     * @return 单位km
     */
    public float getHighVoltageBatteryPropulsionRange() {
        return mSignalAdapter.getHighVoltageBatteryPropulsionRange();
    }

    /**
     * 设置电池预加热参数
     *
     * @param powerLevel       int
     * @param status           int
     * @param timeToArrival    int
     * @param distToArrival    int
     */
    public void setNextChargingDestination(final int powerLevel, final int status, final int timeToArrival, final int distToArrival) {
        if (!CalibrationPackage.getInstance().navigationPreConditionDataProvideEnable()) {
            Logger.i(TAG, "not configuration");
            return;
        }
        Logger.i(TAG, powerLevel + "-" + status + "-" +timeToArrival + "-" + distToArrival);
        mSignalAdapter.setNextChargingDestination(powerLevel, status, timeToArrival, distToArrival);
    }

    /**
     * l2++ nop播报开关
     *
     */
    public boolean getNavigationOnAdasTextToSpeachStatus() {
        return mSignalAdapter.getNavigationOnAdasTextToSpeachStatus();
    }

    /**
     * 设置导航音量
     *
     * @param volume 0-63
     */
    public void setNaviVolume(int volume) {
        mSignalAdapter.setNaviVolume(volume);
    }

    /**
     * 获取导航音量
     *
     * @retuen volume 0-63
     */
    public int getNaviVolume() {
        return mSignalAdapter.getNaviVolume();
    }

    /**
     * 导航状态
     * @param sdNavigationStatusGroup
     */
    public void setSdNavigationStatus(SdNavigationStatusGroup sdNavigationStatusGroup) {
        mSignalAdapter.setSdNavigationStatus(sdNavigationStatusGroup);
        JsonLog.saveJsonToCacheV2(AppCache.getInstance().getMContext(), sdNavigationStatusGroup, "cleacan.json", "NavigationStatus");
    }

    /**
     * 距离拥堵路段的行驶距离
     * @param value
     */
    public void setDistanceToTrafficJamRoad(int value) {
        mSignalAdapter.setDistanceToTrafficJamRoad(value);
    }

    /**
     * 距离拥堵路段的行驶距离的可用性
     * @param value
     */
    public void setDistanceToTrafficJamRoadAvailability(int value) {
        mSignalAdapter.setDistanceToTrafficJamRoadAvailability(value);
    }

    /**
     * 在拥堵路段行驶的距离
     * @param value
     */
    public void setDistanceOnTrafficJamRoad(float value) {
        mSignalAdapter.setDistanceOnTrafficJamRoad(value);
    }

    /**
     * 在拥堵路段行驶的距离的可用性
     * @param value
     */
    public void setDistanceOnTrafficJamRoadAvailability(int value) {
        mSignalAdapter.setDistanceOnTrafficJamRoadAvailability(value);
    }

    /**
     * 拥堵路段的平均速度
     * @param value
     */
    public void setTrafficJamRoadAverageSpeed(int value) {
        if (value > 255) {
            value = 255;
        }
        mSignalAdapter.setTrafficJamRoadAverageSpeed(value);
    }

    /**
     * 拥堵路段的平均速度的可用性
     * @param value
     */
    public void setTrafficJamRoadAverageSpeedAvailability(int value) {
        mSignalAdapter.setTrafficJamRoadAverageSpeedAvailability(value);
    }

    /**
     * 设置道路状态
     * @param roadConditionGroup
     */
    public void setRoadConditionGroup(RoadConditionGroup roadConditionGroup) {
        mSignalAdapter.setRoadConditionGroup(roadConditionGroup);
        JsonLog.saveJsonToCacheV2(AppCache.getInstance().getMContext(), roadConditionGroup, "cleacan.json", "RoadConditionGroup");
    }

    /**
     * 总距离导航到目的地
     * @param value
     */
    public void setTotalDistanceFromStartToDestinationOnNavigation(int value) {
        if (value > 65535) {
            value = 65535;
        }
        mSignalAdapter.setTotalDistanceFromStartToDestinationOnNavigation(value);
        JsonLog.saveJsonToCacheV2(AppCache.getInstance().getMContext(), value, "cleacan.json", "TotalDistance");
    }

    /**
     * 总时间导航到目的地
     * @param value
     */
    public void setTotalPredictedTimeFromStartToDestinationOnNavigation(int value) {
        if (value > 65535) {
            value = 65535;
        }
        mSignalAdapter.setTotalPredictedTimeFromStartToDestinationOnNavigation(value);
        JsonLog.saveJsonToCacheV2(AppCache.getInstance().getMContext(), value, "cleacan.json", "TotalPredictedTime");
    }

    /**
     * 剩余充电站距离
     * @param value
     */
    public void setRemainDistanceToChargingStation(int value) {
        if (value > 65535) {
            value = 65535;
        }
        mSignalAdapter.setRemainDistanceToChargingStation(value);
    }

    /**
     * 剩余充电站时间
     * @param value
     */
    public void setRemainTimeToChargingStationy(int value) {
        if (value > 65535) {
            value = 65535;
        }
        mSignalAdapter.setRemainTimeToChargingStationy(value);
    }

    public void setVcuSpeedLimitArbitrationResults(int value) {
        mSignalAdapter.setVcuSpeedLimitArbitrationResults(value);
    }

    public void setVcuSpeedLimitArbitrationResultsAssured(int value) {
        mSignalAdapter.setVcuSpeedLimitArbitrationResultsAssured(value);
    }

    public int getPredictedFuelSavingPer100km() {
        return mSignalAdapter.getPredictedFuelSavingPer100km();
    }

    public int getTotalFuelSaving() {
        return mSignalAdapter.getTotalFuelSaving();
    }
}