package com.sgm.navi.service.adapter.setting.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.behavior.BehaviorService;
import com.autonavi.gbl.user.behavior.model.BehaviorServiceParam;
import com.autonavi.gbl.user.behavior.model.ConfigKey;
import com.autonavi.gbl.user.behavior.model.ConfigValue;
import com.autonavi.gbl.user.behavior.observer.IBehaviorServiceObserver;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.setting.SettingAdapterCallback;
import com.sgm.navi.service.adapter.setting.SettingApi;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.route.RoutePreferenceID;
import com.sgm.navi.service.define.setting.SettingController;

import java.util.Hashtable;


public class SettingAdapterImpl implements SettingApi, IBehaviorServiceObserver {
    private static final String TAG = MapDefaultFinalTag.SETTING_SERVICE_TAG;
    private final Hashtable<String, SettingAdapterCallback> mSettingHashtable;
    private BehaviorService mBehaviorService;

    public SettingAdapterImpl() {
        mSettingHashtable = new Hashtable<>();
        mBehaviorService = (BehaviorService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BehaviorSingleServiceID);
        Logger.i(TAG, "lvww", mBehaviorService);
    }

    @Override
    public void initSetting() {
        final BehaviorServiceParam behaviorServiceParam = new BehaviorServiceParam();
        if(null == mBehaviorService)
            mBehaviorService = (BehaviorService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BehaviorSingleServiceID);

        mBehaviorService.init(behaviorServiceParam);
        mBehaviorService.addObserver(this);
    }

    @Override
    public void registerCallback(final String key, final SettingAdapterCallback resultCallback) {
        mSettingHashtable.put(key, resultCallback);
    }

    /**
     * 获取路线偏好
     * 默认0：高德推荐，默认态； 2：躲避拥堵； 4：避免收费； 8：不走高速； 16：高速优先 32：速度最快  64：大路优先
     */
    @Override
    public RoutePreferenceID getConfigKeyPlanPref() {
        final ConfigValue mConfigValue = mBehaviorService.getConfig(ConfigKey.ConfigKeyPlanPref);
        if (mConfigValue == null || "".equals(mConfigValue.strValue)) {
            return RoutePreferenceID.PREFERENCE_RECOMMEND;
        }
        Logger.d(TAG, "getConfigKeyPlanPref strValue: " + mConfigValue.strValue);
        return getConfigKeyPlanPref(mConfigValue.strValue);
    }

    /**
     * 路线偏好转换
     * @param planPref 路线偏好
     * @return 路线偏好ID
     */
    private RoutePreferenceID getConfigKeyPlanPref(final String planPref) {
        return switch (planPref) {
            case "2" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION;
            case "4" -> RoutePreferenceID.PREFERENCE_LESSCHARGE;
            case "8" -> RoutePreferenceID.PREFERENCE_NOTHIGHWAY;
            case "16" -> RoutePreferenceID.PREFERENCE_FIRSTHIGHWAY;
            case "32" -> RoutePreferenceID.PREFERENCE_FIRSTMAINROAD;
            case "64" -> RoutePreferenceID.PREFERENCE_FASTESTSPEED;
            case "2|4" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE;
            case "2|8" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY;
            case "2|16" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY;
            case "4|8" -> RoutePreferenceID.PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY;
            case "2|4|8" ->
                    RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY;
            case "2|32" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD;
            case "2|64" -> RoutePreferenceID.PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED;
            default -> RoutePreferenceID.PREFERENCE_RECOMMEND;
        };
    }

    /**
     * 设置路线偏好
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyPlanPref(final RoutePreferenceID routePreferenceID) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.strValue = getRoutePreference(routePreferenceID);
        Logger.d(TAG, "setConfigKeyPlanPref planPrefString: " + routePreferenceID);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyPlanPref, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 路线偏好转换
     * @param routePreferenceID 路线偏好ID
     * @return 路线偏好
     */
    private String getRoutePreference(final RoutePreferenceID routePreferenceID) {
        return switch (routePreferenceID) {
            case PREFERENCE_AVOIDCONGESTION -> "2";
            case PREFERENCE_LESSCHARGE -> "4";
            case PREFERENCE_NOTHIGHWAY -> "8";
            case PREFERENCE_FIRSTHIGHWAY -> "16";
            case PREFERENCE_FIRSTMAINROAD -> "32";
            case PREFERENCE_FASTESTSPEED -> "64";
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE -> "2|4";
            case PREFERENCE_AVOIDCONGESTION_AND_NOTHIGHWAY -> "2|8";
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTHIGHWAY -> "2|16";
            case PREFERENCE_LESSCHARGE_AND_NOTHIGHWAY -> "4|8";
            case PREFERENCE_AVOIDCONGESTION_AND_LESSCHARGE_AND_NOTHIGHWAY -> "2|4|8";
            case PREFERENCE_AVOIDCONGESTION_AND_FIRSTMAINROAD -> "2|32";
            case PREFERENCE_AVOIDCONGESTION_AND_FASTESTSPEED -> "2|64";
            default -> "0";
        };
    }

    /**
     * 设置避开限行状态  0关闭 1打开
     * return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyAvoidLimit(final boolean avoidLimit) {

        Logger.d(TAG, "setConfigKeyAvoidLimit avoidLimit: " + avoidLimit);

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = avoidLimit ? 1 : 0;

        return mBehaviorService.setConfig(ConfigKey.ConfigKeyAvoidLimit, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取避开限行状态   0关闭 1打开
     */
    @Override
    public boolean getConfigKeyAvoidLimit() {
        final ConfigValue configKeyAvoidLimit = mBehaviorService.getConfig(ConfigKey.ConfigKeyAvoidLimit);
        if (configKeyAvoidLimit == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyAvoidLimit intValue: " + configKeyAvoidLimit.intValue);
            return configKeyAvoidLimit.intValue != 0;
        }
    }

    /**
     * 获取车牌号
     * @return 车牌号
     */
    public String getConfigKeyPlateNumber() {
        //车牌号
        final ConfigValue configKeyPlateNumber = mBehaviorService.getConfig(ConfigKey.ConfigKeyPlateNumber);
        if (configKeyPlateNumber == null) {
            return null;
        }
        Logger.d(TAG, "getConfigKeyPlateNumber strValue: " + configKeyPlateNumber.strValue);
        return configKeyPlateNumber.strValue;
    }

    /**
     * 设置车牌号
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyPlateNumber(final String carNumber) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.strValue = carNumber;
        Logger.d(TAG, "setConfigKeyPlateNumber carNumber: " + carNumber);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyPlateNumber, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取常去地点  0:关闭，默认态; 1:开启
     */
    @Override
    public boolean getConfigKeyOftenArrived() {
        final ConfigValue configKeyOftenArrived = mBehaviorService.getConfig(ConfigKey.ConfigKeyOftenArrived);
        if (configKeyOftenArrived == null) {
            return false;
        } else {
            Logger.d(TAG, "getConfigKeyOftenArrived intValue: " + configKeyOftenArrived.intValue);
            return configKeyOftenArrived.intValue != 0;
        }
    }

    /**
     * 设置常去地点  0:关闭，默认态; 1:开启
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyOftenArrived(final boolean oftenArrived) {

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = oftenArrived ? 1 : 0;

        Logger.d(TAG, "setConfigKeyOftenArrived oftenArrived: " + oftenArrived);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyOftenArrived, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取自动退出全览  0:关闭; 1:开启, 默认
     */
    @Override
    public boolean getConfigKeyAutoExitPreview() {
        final ConfigValue configKeyAutoExitPreview = mBehaviorService.getConfig(ConfigKey.ConfigKeyAutoExitPreview);
        if (configKeyAutoExitPreview == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyAutoExitPreview intValue: " + configKeyAutoExitPreview.intValue);
            return configKeyAutoExitPreview.intValue != 0;
        }
    }

    /**
     * 设置自动退出全览  0:关闭; 1:开启, 默认
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyAutoExitPreview(final boolean autoExitPreview) {

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = autoExitPreview ? 1 : 0;

        Logger.d(TAG, "setConfigKeyAutoExitPreview autoExitPreview: " + autoExitPreview);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyAutoExitPreview, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取导航播报音量
     */
    @Override
    public int getConfigKeyBroadcastVolume() {
        final ConfigValue configKeyBroadcastVolume = mBehaviorService.getConfig(ConfigKey.ConfigKeyBroadcastVolume);
        if (configKeyBroadcastVolume == null) {
            return 0;
        }
        Logger.d(TAG, "getConfigKeyBroadcastVolume intValue: " + configKeyBroadcastVolume.intValue);
        return configKeyBroadcastVolume.intValue;
    }

    /**
     * 设置导航播报音量
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyBroadcastVolume(final int broadcastVolume) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = broadcastVolume;
        Logger.d(TAG, "setConfigKeyBroadcastVolume broadcastVolume: " + broadcastVolume);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyBroadcastVolume, castSimple, SyncMode.SyncModeNow);
    }


    /**
     * 获取导航播报模式  1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    @Override
    public int getConfigKeyBroadcastMode() {
        final ConfigValue configKeyBroadcastMode = mBehaviorService.getConfig(ConfigKey.ConfigKeyBroadcastMode);
        if (configKeyBroadcastMode == null) {
            return 2;
        }
        Logger.d(TAG, "getConfigKeyBroadcastMode intValue: " + configKeyBroadcastMode.intValue);
        return configKeyBroadcastMode.intValue == 0 ? 2 : configKeyBroadcastMode.intValue;
    }

    /**
     * 设置导航播报模式  1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    @Override
    public int setConfigKeyBroadcastMode(final int broadcastMode) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = broadcastMode;
        Logger.d(TAG, "setConfigKeyBroadcastMode broadcastMode: " + broadcastMode);
        final int res = mBehaviorService.setConfig(ConfigKey.ConfigKeyBroadcastMode, castSimple, SyncMode.SyncModeNow);
        if (res == 0) {
            switch (broadcastMode) {
                case 1:
                    for (final SettingAdapterCallback settingAdapterCallback : mSettingHashtable.values()) {
                        settingAdapterCallback.onSettingChanged(
                                SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                    }
                    break;
                case 2:
                    for (final SettingAdapterCallback settingAdapterCallback : mSettingHashtable.values()) {
                        settingAdapterCallback.onSettingChanged(
                                SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                    }
                    break;
                case 3:
                    for (final SettingAdapterCallback settingAdapterCallback : mSettingHashtable.values()) {
                        settingAdapterCallback.onSettingChanged(
                                SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                    }
                    break;
                default:
                    break;
            }

        }
        return res;
    }

    /**
     * 获取巡航播报前方路况  0：off； 1：on
     */
    @Override
    public boolean getConfigKeyRoadWarn() {
        final ConfigValue configKeyRoadWarn = mBehaviorService.getConfig(ConfigKey.ConfigKeyRoadWarn);
        if (configKeyRoadWarn == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyRoadWarn intValue: " + configKeyRoadWarn.intValue);
            return configKeyRoadWarn.intValue != 0;
        }
    }

    /**
     * 设置巡航播报前方路况 0：off； 1：on
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyRoadWarn(final boolean roadWarn) {

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = roadWarn ? 1 : 0;

        Logger.d(TAG, "setConfigKeyRoadWarn roadWarn: " + roadWarn);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyRoadWarn, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取巡航播报电子眼播报  0：off； 1：on
     */
    @Override
    public boolean getConfigKeySafeBroadcast() {
        final ConfigValue configKeySafeBroadcast = mBehaviorService.getConfig(ConfigKey.ConfigKeySafeBroadcast);
        if (configKeySafeBroadcast == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeySafeBroadcast intValue: " + configKeySafeBroadcast.intValue);
            return configKeySafeBroadcast.intValue != 0;
        }
    }

    /**
     * 设置巡航播报电子眼播报  0：off； 1：on
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeySafeBroadcast(final boolean safeBroadcast) {

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = safeBroadcast ? 1 : 0;

        Logger.d(TAG, "setConfigKeySafeBroadcast safeBroadcast: " + safeBroadcast);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeySafeBroadcast, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取巡航播报安全提醒  0：off； 1：on
     */
    @Override
    public boolean getConfigKeyDriveWarn() {
        final ConfigValue configKeyDriveWarn = mBehaviorService.getConfig(ConfigKey.ConfigKeyDriveWarn);
        if (configKeyDriveWarn == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyDriveWarn intValue: " + configKeyDriveWarn.intValue);
            return configKeyDriveWarn.intValue != 0;
        }
    }

    /**
     * 设置巡航播报安全提醒  0：off； 1：on
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyDriveWarn(final boolean driveWarn) {

        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = driveWarn ? 1 : 0;

        Logger.d(TAG, "setConfigKeyDriveWarn driveWarn: " + driveWarn);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyDriveWarn, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取地图视角  0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    @Override
    public int getConfigKeyMapviewMode() {
        final ConfigValue configMapviewModeValue = mBehaviorService.getConfig(ConfigKey.ConfigKeyMapviewMode);
        if (configMapviewModeValue == null) {
            return 0;
        }
        Logger.d(TAG, "getConfigKeyMapviewMode intValue: " + configMapviewModeValue.intValue);
        return configMapviewModeValue.intValue;
    }

    /**
     * 设置地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyMapviewMode(final int mapViewMode) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = mapViewMode;
        Logger.d(TAG, "setConfigKeyMapviewMode mapViewMode: " + mapViewMode);
        final int res = mBehaviorService.setConfig(ConfigKey.ConfigKeyMapviewMode, castSimple, SyncMode.SyncModeNow);
        if (res == 0) {
            switch (mapViewMode) {
                case 0:
                    for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                        resultCallback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    }
                    break;
                case 1:
                    for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                        resultCallback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    }
                    break;
                case 2:
                    for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                        resultCallback.onSettingChanged(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    }
                    break;
                default:
                    break;
            }
        }
        return res;
    }

    /**
     * 获取路况开关   1：on  0：off
     */
    @Override
    public boolean getConfigKeyRoadEvent() {
        final ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyRoadEvent);
        if (configKeyRoadEvent == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyRoadEvent intValue: " + configKeyRoadEvent.intValue);
            return configKeyRoadEvent.intValue != 0;
        }
    }

    /**
     * 设置路况开关   1：on  0：off
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyRoadEvent(final boolean roadEvent) {

        final ConfigValue configValue = new ConfigValue();
        configValue.intValue = roadEvent ? 1 : 0;

        Logger.d(TAG, "setConfigKeyRoadEvent roadEvent: " + roadEvent);
        final int res = mBehaviorService.setConfig(ConfigKey.ConfigKeyRoadEvent, configValue, SyncMode.SyncModeNow);
        if (res == 0) {
            for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                resultCallback.onSettingChanged(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
            }
        }
        return res;
    }

    /**
     * 获取车辆动力类型
     * @return 1:无,未设置车牌号默认值 0:燃油车,已设置车牌号默认值 1:纯电动 2:插电式混动 3:油气两用
     */
    @Override
    public int getConfigKeyPowerType() {
        final ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyPowerType);
        if (configKeyRoadEvent == null) {
            return 1;
        } else {
            Logger.d(TAG, "getConfigKeyPowerType intValue: " + configKeyRoadEvent.intValue);
            return configKeyRoadEvent.intValue;
        }
    }

    /**
     * 获取混音方式
     * @return 混音方式 2：暂停音乐；  3：压低音乐，默认
     */
    @Override
    public int getConfigKeyAudioMixMode() {
        final ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyAudioMixMode);
        if (configKeyRoadEvent == null) {
            return 1;
        } else {
            Logger.d(TAG, "getConfigKeyAudioMixMode intValue: " + configKeyRoadEvent.intValue);
            return configKeyRoadEvent.intValue;
        }
    }

    /**
     * 设置混音方式
     * @param audioMixMode 混音方式 2：暂停音乐；  3：压低音乐，默认
     * @return 0:成功 其他：失败
     */
    @Override
    public int setConfigKeyAudioMixMode(final int audioMixMode) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = audioMixMode;
        Logger.d(TAG, "setConfigKeyAudioMixMode audioMixMode: " + audioMixMode);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyAudioMixMode, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 设置静音状态
     * @param mute 静音状态 0：不静音，默认态； 1：静音
     * @return 返回错误码
     */
    @Override
    public int setConfigKeyMute(final int mute) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = mute;
        Logger.d(TAG, "setConfigKeyMute mute: " + mute);
        final int res = mBehaviorService.setConfig(ConfigKey.ConfigKeyMute, castSimple, SyncMode.SyncModeNow);
        if (res == 0) {
            switch (mute) {
                case 0:
                    for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                        resultCallback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_ON);
                    }
                    break;
                case 1:
                    for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
                        resultCallback.onSettingChanged(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_OFF);
                    }
                    break;
                default:
                    break;
            }

        }
        return res;
    }

    /**
     * 获取静音状态
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    @Override
    public int getConfigKeyMute() {
        final ConfigValue configKeyMute = mBehaviorService.getConfig(ConfigKey.ConfigKeyMute);
        if (configKeyMute == null) {
            return 0;
        } else {
            Logger.d(TAG, "getConfigKeyMute intValue: " + configKeyMute.intValue);
            return configKeyMute.intValue;
        }
    }

    /**
     * 设置白天黑夜模式
     * @param dayNightMode 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     * @return 返回错误码
     */
    @Override
    public int setConfigKeyDayNightMode(final ThemeType dayNightMode) {
        final ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = ThemeType.getThemeValueByType(dayNightMode);
        Logger.d(TAG, "setConfigKeyDayNightMode dayNightMode: " + dayNightMode);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyDayNightMode, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取白天黑夜
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    @Override
    public int getConfigKeyDayNightMode() {
        final ConfigValue configKeyMute = mBehaviorService.getConfig(ConfigKey.ConfigKeyDayNightMode);
        if (configKeyMute == null) {
            return 0;
        } else {
            Logger.d(TAG, "getConfigKeyDayNightMode intValue: " + configKeyMute.intValue);
            return configKeyMute.intValue;
        }
    }

    /**
     * 获取行为数据回调通知
     * @param eventType 同步SDK回调事件类型
     * @param exCode 同步SDK返回值
     */
    @Override
    public void notify(final int eventType, final int exCode) {
        Logger.d(TAG, "notify eventType: " + eventType + " exCode: " + exCode);
        for (SettingAdapterCallback resultCallback : mSettingHashtable.values()) {
            resultCallback.notify(eventType, exCode);
        }
    }
}