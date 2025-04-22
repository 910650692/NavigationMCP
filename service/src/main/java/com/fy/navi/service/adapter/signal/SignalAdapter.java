package com.fy.navi.service.adapter.signal;

import android.content.Context;

import com.fy.navi.service.AdapterConfig;

import java.util.Objects;

/**
 * 车辆信号
 */
public final class SignalAdapter {
    private static final String SIGNAL_API_PKG = Objects.requireNonNull(SignalAdapter.class.getPackage()).getName();
    private static final String SIGNAL_API_CLS = "SignalAdapterImpl";
    private final SignalApi mSignalApi;

    private static final class SInstanceHolder {
        static final SignalAdapter INSTANCE = new SignalAdapter();
    }

    private SignalAdapter() {
        mSignalApi = (SignalApi) AdapterConfig.getObject(SIGNAL_API_PKG, SIGNAL_API_CLS);
    }

    /**
     * 初始化信号
     * @param context
     */
    public void initSignal(final Context context) {
        mSignalApi.initSignal(context);
    }

    /**
     * 注册回调
     * @param key
     * @param resultCallback
     */
    public void registerCallback(final String key, final SignalAdapterCallback resultCallback) {
        mSignalApi.registerCallback(key, resultCallback);
    }

    public static SignalAdapter getInstance() {
        return SignalAdapter.SInstanceHolder.INSTANCE;
    }

    public int getChargeSystemStatus() {
        return mSignalApi.getChargeSystemStatus();
    }

    public float getBatteryEnergyPercent() {
        return mSignalApi.getBatteryEnergyPercent();
    }

    public float getMaxBatteryEnergy() {
        return mSignalApi.getMaxBatteryEnergy();
    }

    public float getBatteryEnergy() {
        return mSignalApi.getBatteryEnergy();
    }

    public float getOutsideTemperature() {
        return mSignalApi.getOutsideTemperature();
    }

    public float getSpeedOfVehicle() {
        return mSignalApi.getSpeedOfVehicle();
    }

    public int getAcSwitchState() {
        return mSignalApi.getAcSwitchState();
    }

    public int getSystemState() {
        return mSignalApi.getSystemState();
    }

    public float getRangeRemaining() {
        return mSignalApi.getRangeRemaining();
    }

    public float getHighVoltageBatteryPropulsionRange() {
        return mSignalApi.getHighVoltageBatteryPropulsionRange();
    }

    public void setNextChargingDestination(int powerLevel, int status, int timeToArrival, int distToArrival) {
        mSignalApi.setNextChargingDestination(powerLevel, status, timeToArrival, distToArrival);
    }

    public int getNavigationOnAdasTextToSpeachStatus() {
        return mSignalApi.getNavigationOnAdasTextToSpeachStatus();
    }
}
