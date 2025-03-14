package com.fy.navi.service.adapter.signal;

import android.content.Context;

public interface SignalApi {
    void initSignal(Context context);

    void registerCallback(String key, SignalAdapterCallback resultCallback);

    float getOutsideTemperature();

    float getSpeedOfVehicle();

    int getAcSwitchState();

    int getChargeSystemStatus();

    float getBatteryEnergyPercent();

    float getMaxBatteryEnergy();

    float getBatteryEnergy();

    int getSystemState();
}
