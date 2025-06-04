package com.fy.navi.service.adapter.signal;

import android.content.Context;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.signal.RoadConditionGroupFirst;
import com.fy.navi.service.define.signal.RoadConditionGroupSecond;
import com.fy.navi.service.define.signal.SdNavigationStatusGroup;

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

    public void setNaviVolume(int volume) {
        mSignalApi.setNaviVolume(volume);
    }

    public int getNaviVolume() {
        return mSignalApi.getNaviVolume();
    }

    public void setSdNavigationStatus(SdNavigationStatusGroup sdNavigationStatusGroup) {
        mSignalApi.setSdNavigationStatus(sdNavigationStatusGroup);
    }

    public void setNavigationOnAdasButtonSettingRequest(int value) {
        mSignalApi.setNavigationOnAdasButtonSettingRequest(value);
    }

    public void setNavigationOnAdasInfoNavigationStatus(int value) {
        mSignalApi.setNavigationOnAdasInfoNavigationStatus(value);
    }

    public void setDistanceToTrafficJamRoad(int value) {
        mSignalApi.setDistanceToTrafficJamRoad(value);
    }

    public void setDistanceToTrafficJamRoadAvailability(int value) {
        mSignalApi.setDistanceToTrafficJamRoadAvailability(value);
    }

    public void setDistanceOnTrafficJamRoad(int value) {
        mSignalApi.setDistanceOnTrafficJamRoad(value);
    }

    public void setDistanceOnTrafficJamRoadAvailability(int value) {
        mSignalApi.setDistanceOnTrafficJamRoadAvailability(value);
    }

    public void setTrafficJamRoadAverageSpeed(int value) {
        mSignalApi.setTrafficJamRoadAverageSpeed(value);
    }

    public void setTrafficJamRoadAverageSpeedAvailability(int value) {
        mSignalApi.setTrafficJamRoadAverageSpeedAvailability(value);
    }

    public void setRoadConditionGroupFirst(RoadConditionGroupFirst roadConditionGroupFirst) {
        mSignalApi.setRoadConditionGroupFirst(roadConditionGroupFirst);
    }

    public void setRoadConditionGroupSecond(RoadConditionGroupSecond roadConditionGroupSecond) {
        mSignalApi.setRoadConditionGroupSecond(roadConditionGroupSecond);
    }

    public void setTotalDistanceFromStartToDestinationOnNavigation(int value) {
        mSignalApi.setTotalDistanceFromStartToDestinationOnNavigation(value);
    }

    public void setTotalPredictedTimeFromStartToDestinationOnNavigation(int value) {
        mSignalApi.setTotalPredictedTimeFromStartToDestinationOnNavigation(value);
    }

    public void setRemainDistanceToChargingStation(int value) {
        mSignalApi.setRemainDistanceToChargingStation(value);
    }

    public void setRemainTimeToChargingStationy(int value) {
        mSignalApi.setRemainTimeToChargingStationy(value);
    }
}
