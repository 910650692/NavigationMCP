package com.fy.navi.service.logicpaket.signal;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.adapter.signal.SignalAdapter;
import com.fy.navi.service.adapter.signal.SignalAdapterCallback;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;

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
        mSignalAdapter.initSignal(context);
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
     * 电池剩余电量百分比
     *
     * @return 单位%
     */
    public float getBatteryEnergyPercent() {
        return mSignalAdapter.getBatteryEnergyPercent();
    }

    /**
     * 电池最大电量
     *
     * @return 单位kWh
     */
    public float getMaxBatteryEnergy() {
        return mSignalAdapter.getMaxBatteryEnergy();
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
     * 续航里程
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
     * 智慧领航播报开关
     *
     * @return 0:关闭 1:开启
     */
    public int getNavigationOnAdasTextToSpeachStatus() {
        return mSignalAdapter.getNavigationOnAdasTextToSpeachStatus();
    }
}