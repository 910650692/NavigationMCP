package com.fy.navi.service.adapter.signal;

import android.content.Context;

import com.fy.navi.service.AdapterConfig;

import java.util.Objects;

public class SignalAdapter {
    private static final String SIGNAL_API_PKG = Objects.requireNonNull(SignalAdapter.class.getPackage()).getName();
    private static final String SIGNAL_API_CLS = "SignalAdapterImpl";
    private final SignalApi mSignalApi;

    private static final class SInstanceHolder {
        static final SignalAdapter sInstance = new SignalAdapter();
    }

    private SignalAdapter() {
        mSignalApi = (SignalApi) AdapterConfig.getObject(SIGNAL_API_PKG, SIGNAL_API_CLS);
    }

    public void initSignal(Context context) {
        mSignalApi.initSignal(context);
    }

    public void registerCallback(String key, SignalAdapterCallback resultCallback) {
        mSignalApi.registerCallback(key, resultCallback);
    }

    public static SignalAdapter getInstance() {
        return SignalAdapter.SInstanceHolder.sInstance;
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
}
