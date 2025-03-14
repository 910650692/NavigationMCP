package com.fy.navi.service.adapter.setting;



import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.setting.SimpleFavoriteItemBean;

import java.util.ArrayList;
import java.util.Objects;

public class SettingAdapter {
    private static final String SETTING_API_PKG = Objects.requireNonNull(SettingAdapter.class.getPackage()).getName();
    private static final String SETTING_API_CLS = "SettingAdapterImpl";
    private final SettingApi mSettingApi;

    private SettingAdapter() {
        mSettingApi = (SettingApi) AdapterConfig.getObject(SETTING_API_PKG, SETTING_API_CLS);
    }

    public void initSetting() {
        mSettingApi.initSetting();
    }

    public void registerCallback(String key, SettingAdapterCallback resultCallback) {
        mSettingApi.registerCallback(key, resultCallback);
    }

    public ArrayList<SimpleFavoriteItemBean> getSimpleFavoriteList(int type, boolean sorted) {
        return mSettingApi.getSimpleFavoriteList(type, sorted);
    }

    public int setConfigKeyPlanPref(RoutePreferenceID routePreferenceID) {
        return mSettingApi.setConfigKeyPlanPref(routePreferenceID);
    }

    public RoutePreferenceID getConfigKeyPlanPref() {
        return mSettingApi.getConfigKeyPlanPref();
    }

    public int setConfigKeyAvoidLimit(boolean avoidLimit) {
        return mSettingApi.setConfigKeyAvoidLimit(avoidLimit);
    }
    public boolean getConfigKeyAvoidLimit() {
        return mSettingApi.getConfigKeyAvoidLimit();
    }

    public int setConfigKeyPlateNumber(String carNumber) {
        return mSettingApi.setConfigKeyPlateNumber(carNumber);
    }
    public String getConfigKeyPlateNumber() {
        return mSettingApi.getConfigKeyPlateNumber();
    }
    public int setConfigKeyOftenArrived(boolean oftenArrived) {
        return mSettingApi.setConfigKeyOftenArrived(oftenArrived);
    }

    public boolean getConfigKeyOftenArrived() {
        return mSettingApi.getConfigKeyOftenArrived();
    }

    public int setConfigKeyAutoExitPreview(boolean autoExitPreview) {
        return mSettingApi.setConfigKeyAutoExitPreview(autoExitPreview);
    }

    public boolean getConfigKeyAutoExitPreview() {
        return mSettingApi.getConfigKeyAutoExitPreview();
    }

    public int setConfigKeyBroadcastVolume(int broadcastVolume) {
        return mSettingApi.setConfigKeyBroadcastVolume(broadcastVolume);
    }

    public int getConfigKeyBroadcastVolume() {
        return mSettingApi.getConfigKeyBroadcastVolume();
    }

    public int setConfigKeyBroadcastMode(int broadcastMode) {
        return mSettingApi.setConfigKeyBroadcastMode(broadcastMode);
    }

    public int getConfigKeyBroadcastMode() {
        return mSettingApi.getConfigKeyBroadcastMode();
    }

    public int setConfigKeyRoadWarn(boolean roadWarn) {
        return mSettingApi.setConfigKeyRoadWarn(roadWarn);
    }

    public boolean getConfigKeyRoadWarn() {
        return mSettingApi.getConfigKeyRoadWarn();
    }

    public int setConfigKeySafeBroadcast(boolean safeBroadcast) {
        return mSettingApi.setConfigKeySafeBroadcast(safeBroadcast);
    }

    public boolean getConfigKeySafeBroadcast() {
        return mSettingApi.getConfigKeySafeBroadcast();
    }

    public int setConfigKeyDriveWarn(boolean driveWarn) {
        return mSettingApi.setConfigKeyDriveWarn(driveWarn);
    }

    public boolean getConfigKeyDriveWarn() {
        return mSettingApi.getConfigKeyDriveWarn();
    }

    public int setConfigKeyMapviewMode(int mapViewMode) {
        return mSettingApi.setConfigKeyMapviewMode(mapViewMode);
    }

    public int getConfigKeyMapviewMode() {
        return mSettingApi.getConfigKeyMapviewMode();
    }

    public int setConfigKeyRoadEvent(boolean roadEvent) {
        return mSettingApi.setConfigKeyRoadEvent(roadEvent);
    }

    public boolean getConfigKeyRoadEvent() {
        return mSettingApi.getConfigKeyRoadEvent();
    }

    public int getConfigKeyPowerType() {
        return mSettingApi.getConfigKeyPowerType();
    }

    public int getConfigKeyAudioMixMode() {
        return mSettingApi.getConfigKeyAudioMixMode();
    }

    public int setConfigKeyAudioMixMode(int audioMixMode) {
        return mSettingApi.setConfigKeyAudioMixMode(audioMixMode);
    }

    public int setConfigKeyMute(int mute) {
        return mSettingApi.setConfigKeyMute(mute);
    }

    public int getConfigKeyMute() {
        return mSettingApi.getConfigKeyMute();
    }

    public int setConfigKeyDayNightMode(int dayNightMode) {
        return mSettingApi.setConfigKeyDayNightMode(dayNightMode);
    }

    public int getConfigKeyDayNightMode() {
        return mSettingApi.getConfigKeyDayNightMode();
    }

    public static SettingAdapter getInstance() {
        return SInstanceHolder.sInstance;
    }

    private static final class SInstanceHolder {
        static final SettingAdapter sInstance = new SettingAdapter();
    }
}