package com.fy.navi.service.adapter.setting.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.user.behavior.BehaviorService;
import com.autonavi.gbl.user.behavior.model.BehaviorServiceParam;
import com.autonavi.gbl.user.behavior.model.ConfigKey;
import com.autonavi.gbl.user.behavior.model.ConfigValue;
import com.autonavi.gbl.user.behavior.model.SimpleFavoriteItem;
import com.autonavi.gbl.user.behavior.observer.IBehaviorServiceObserver;
import com.autonavi.gbl.user.syncsdk.model.SyncMode;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.setting.SettingAdapterCallback;
import com.fy.navi.service.adapter.setting.SettingApi;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.setting.SettingConstant;
import com.fy.navi.service.define.setting.SimpleFavoriteItemBean;

import java.util.ArrayList;
import java.util.Hashtable;

/**
 * Setting数据服务.
 * @Description Impl类只做SDK的原子能力封装，不做对象及数据转换
 * @Author fh
 * @date 2024/02/19
 */
public class SettingAdapterImpl implements SettingApi, IBehaviorServiceObserver {
    private static final String TAG = MapDefaultFinalTag.SETTING_SERVICE_TAG;
    private final Hashtable<String, SettingAdapterCallback> settingHashtable;
    private final BehaviorService mBehaviorService;

    public SettingAdapterImpl() {
        settingHashtable = new Hashtable<>();
        mBehaviorService = (BehaviorService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BehaviorSingleServiceID);
    }

    @Override
    public void initSetting() {
        BehaviorServiceParam behaviorServiceParam = new BehaviorServiceParam();
        mBehaviorService.init(behaviorServiceParam);
        mBehaviorService.addObserver(this);
    }

    @Override
    public void registerCallback(String key, SettingAdapterCallback resultCallback) {
        settingHashtable.put(key, resultCallback);
    }

    /**
     * 获取路线偏好
     * 默认0：高德推荐，默认态； 2：躲避拥堵； 4：避免收费； 8：不走高速； 16：高速优先 32：速度最快  64：大路优先
     */
    @Override
    public RoutePreferenceID getConfigKeyPlanPref() {
        ConfigValue mConfigValue = mBehaviorService.getConfig(ConfigKey.ConfigKeyPlanPref);
        if (mConfigValue == null || "".equals(mConfigValue.strValue)) {
            return RoutePreferenceID.PREFERENCE_RECOMMEND;
        }
        Logger.d(TAG, "getConfigKeyPlanPref strValue: " + mConfigValue.strValue);
        return getConfigKeyPlanPref(mConfigValue.strValue);
    }

    private RoutePreferenceID getConfigKeyPlanPref(String planPref) {
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
    public int setConfigKeyPlanPref(RoutePreferenceID routePreferenceID) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.strValue = getRoutePreference(routePreferenceID);
        Logger.d(TAG, "setConfigKeyPlanPref planPrefString: " + routePreferenceID);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyPlanPref, castSimple, SyncMode.SyncModeNow);
    }

    private String getRoutePreference(RoutePreferenceID routePreferenceID) {
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
    public int setConfigKeyAvoidLimit(boolean avoidLimit) {

        Logger.d(TAG, "setConfigKeyAvoidLimit avoidLimit: " + avoidLimit);

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = avoidLimit ? 1 : 0;

        return mBehaviorService.setConfig(ConfigKey.ConfigKeyAvoidLimit, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取避开限行状态   0关闭 1打开
     */
    @Override
    public boolean getConfigKeyAvoidLimit() {
        ConfigValue configKeyAvoidLimit = mBehaviorService.getConfig(ConfigKey.ConfigKeyAvoidLimit);
        if (configKeyAvoidLimit == null) {
            return true;
        } else {
            Logger.d(TAG, "getConfigKeyAvoidLimit intValue: " + configKeyAvoidLimit.intValue);
            return configKeyAvoidLimit.intValue != 0;
        }
    }

    /**
     * 获取车牌号
     */
    public String getConfigKeyPlateNumber() {
        //车牌号
        ConfigValue configKeyPlateNumber = mBehaviorService.getConfig(ConfigKey.ConfigKeyPlateNumber);
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
    public int setConfigKeyPlateNumber(String carNumber) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.strValue = carNumber;
        Logger.d(TAG, "setConfigKeyPlateNumber carNumber: " + carNumber);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyPlateNumber, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取常去地点  0:关闭，默认态; 1:开启
     */
    @Override
    public boolean getConfigKeyOftenArrived() {
        ConfigValue configKeyOftenArrived = mBehaviorService.getConfig(ConfigKey.ConfigKeyOftenArrived);
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
    public int setConfigKeyOftenArrived(boolean oftenArrived) {

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = oftenArrived ? 1 : 0;

        Logger.d(TAG, "setConfigKeyOftenArrived oftenArrived: " + oftenArrived);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyOftenArrived, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取自动退出全览  0:关闭; 1:开启, 默认
     */
    @Override
    public boolean getConfigKeyAutoExitPreview() {
        ConfigValue configKeyAutoExitPreview = mBehaviorService.getConfig(ConfigKey.ConfigKeyAutoExitPreview);
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
    public int setConfigKeyAutoExitPreview(boolean autoExitPreview) {

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = autoExitPreview ? 1 : 0;

        Logger.d(TAG, "setConfigKeyAutoExitPreview autoExitPreview: " + autoExitPreview);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyAutoExitPreview, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取导航播报音量
     */
    @Override
    public int getConfigKeyBroadcastVolume() {
        ConfigValue configKeyBroadcastVolume = mBehaviorService.getConfig(ConfigKey.ConfigKeyBroadcastVolume);
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
    public int setConfigKeyBroadcastVolume(int broadcastVolume) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = broadcastVolume;
        Logger.d(TAG, "setConfigKeyBroadcastVolume broadcastVolume: " + broadcastVolume);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyBroadcastVolume, castSimple, SyncMode.SyncModeNow);
    }


    /**
     * 获取导航播报模式  1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    @Override
    public int getConfigKeyBroadcastMode() {
        ConfigValue configKeyBroadcastMode = mBehaviorService.getConfig(ConfigKey.ConfigKeyBroadcastMode);
        if (configKeyBroadcastMode == null) {
            return SettingConstant.BROADCAST_DETAIL;
        }
        Logger.d(TAG, "getConfigKeyBroadcastMode intValue: " + configKeyBroadcastMode.intValue);
        return configKeyBroadcastMode.intValue == 0 ? SettingConstant.BROADCAST_DETAIL : configKeyBroadcastMode.intValue;
    }

    /**
     * 设置导航播报模式  1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    @Override
    public int setConfigKeyBroadcastMode(int broadcastMode) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = broadcastMode;
        Logger.d(TAG, "setConfigKeyBroadcastMode broadcastMode: " + broadcastMode);
        int res = mBehaviorService.setConfig(ConfigKey.ConfigKeyBroadcastMode, castSimple, SyncMode.SyncModeNow);
        return res;
    }

    /**
     * 获取巡航播报前方路况  0：off； 1：on
     */
    @Override
    public boolean getConfigKeyRoadWarn() {
        ConfigValue configKeyRoadWarn = mBehaviorService.getConfig(ConfigKey.ConfigKeyRoadWarn);
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
    public int setConfigKeyRoadWarn(boolean roadWarn) {

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = roadWarn ? 1 : 0;

        Logger.d(TAG, "setConfigKeyRoadWarn roadWarn: " + roadWarn);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyRoadWarn, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取巡航播报电子眼播报  0：off； 1：on
     */
    @Override
    public boolean getConfigKeySafeBroadcast() {
        ConfigValue configKeySafeBroadcast = mBehaviorService.getConfig(ConfigKey.ConfigKeySafeBroadcast);
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
    public int setConfigKeySafeBroadcast(boolean safeBroadcast) {

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = safeBroadcast ? 1 : 0;

        Logger.d(TAG, "setConfigKeySafeBroadcast safeBroadcast: " + safeBroadcast);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeySafeBroadcast, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取巡航播报安全提醒  0：off； 1：on
     */
    @Override
    public boolean getConfigKeyDriveWarn() {
        ConfigValue configKeyDriveWarn = mBehaviorService.getConfig(ConfigKey.ConfigKeyDriveWarn);
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
    public int setConfigKeyDriveWarn(boolean driveWarn) {

        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = driveWarn ? 1 : 0;

        Logger.d(TAG, "setConfigKeyDriveWarn driveWarn: " + driveWarn);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyDriveWarn, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取地图视角  0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    @Override
    public int getConfigKeyMapviewMode() {
        ConfigValue configMapviewModeValue = mBehaviorService.getConfig(ConfigKey.ConfigKeyMapviewMode);
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
    public int setConfigKeyMapviewMode(int mapViewMode) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = mapViewMode;
        Logger.d(TAG, "setConfigKeyMapviewMode mapViewMode: " + mapViewMode);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyMapviewMode, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取路况开关   1：on  0：off
     */
    @Override
    public boolean getConfigKeyRoadEvent() {
        ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyRoadEvent);
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
    public int setConfigKeyRoadEvent(boolean roadEvent) {

        ConfigValue configValue = new ConfigValue();
        configValue.intValue = roadEvent ? 1 : 0;

        Logger.d(TAG, "setConfigKeyRoadEvent roadEvent: " + roadEvent);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyRoadEvent, configValue, SyncMode.SyncModeNow);
    }

    /**
     * 获取收藏点列表
     * @param type 收藏类别
     * @param sorted 是否排序
     * @return 收藏点列表
     */
    @Override
    public ArrayList<SimpleFavoriteItemBean> getSimpleFavoriteList(int type, boolean sorted) {
        if (mBehaviorService != null) {
            return formatSimpleFavoriteList(mBehaviorService.getSimpleFavoriteList(type, sorted));
        }
        return null;
    }

    /**
     * 获取车辆动力类型
     * @return 1:无,未设置车牌号默认值 0:燃油车,已设置车牌号默认值 1:纯电动 2:插电式混动 3:油气两用
     */
    @Override
    public int getConfigKeyPowerType() {
        ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyPowerType);
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
        ConfigValue configKeyRoadEvent = mBehaviorService.getConfig(ConfigKey.ConfigKeyAudioMixMode);
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
    public int setConfigKeyAudioMixMode(int audioMixMode) {
        ConfigValue castSimple = new ConfigValue();
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
    public int setConfigKeyMute(int mute) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = mute;
        Logger.d(TAG, "setConfigKeyMute mute: " + mute);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyMute, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取静音状态
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    @Override
    public int getConfigKeyMute() {
        ConfigValue configKeyMute = mBehaviorService.getConfig(ConfigKey.ConfigKeyMute);
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
    public int setConfigKeyDayNightMode(int dayNightMode) {
        ConfigValue castSimple = new ConfigValue();
        castSimple.intValue = dayNightMode;
        Logger.d(TAG, "setConfigKeyDayNightMode dayNightMode: " + dayNightMode);
        return mBehaviorService.setConfig(ConfigKey.ConfigKeyDayNightMode, castSimple, SyncMode.SyncModeNow);
    }

    /**
     * 获取白天黑夜
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    @Override
    public int getConfigKeyDayNightMode() {
        ConfigValue configKeyMute = mBehaviorService.getConfig(ConfigKey.ConfigKeyDayNightMode);
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
    public void notify(int eventType, int exCode) {
        Logger.d(TAG, "notify eventType: " + eventType + " exCode: " + exCode);
        for (SettingAdapterCallback resultCallback : settingHashtable.values()) {
            resultCallback.notify(eventType, exCode);
        }
    }

    private boolean checkSettingService(){
        int serviceInitResult = mBehaviorService.isInit();
        if(ConvertUtils.equals(serviceInitResult, 0) && ConvertUtils.equals(serviceInitResult, 1)){
           return true;
        }
        return false;
    }

    private ArrayList<SimpleFavoriteItemBean> formatSimpleFavoriteList(ArrayList<SimpleFavoriteItem> list) {
        ArrayList<SimpleFavoriteItemBean> simpleFavoriteList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(list)) {
            for (SimpleFavoriteItem item : list) {
                SimpleFavoriteItemBean simpleFavoriteItemBean = new SimpleFavoriteItemBean();
                simpleFavoriteItemBean.id = item.id;
                simpleFavoriteItemBean.item_id = item.item_id;
                simpleFavoriteItemBean.name = item.name;
                simpleFavoriteItemBean.common_name = item.common_name;
                simpleFavoriteItemBean.point_x = item.point_x;
                simpleFavoriteItemBean.point_y = item.point_y;
                simpleFavoriteItemBean.point_x_arrive = item.point_x_arrive;
                simpleFavoriteItemBean.point_y_arrive = item.point_y_arrive;
                simpleFavoriteItemBean.city_name = item.city_name;
                simpleFavoriteItemBean.city_code = item.city_code;
                simpleFavoriteItemBean.phone_numbers = item.phone_numbers;
                simpleFavoriteItemBean.tag = item.tag;
                simpleFavoriteItemBean.type = item.type;
                simpleFavoriteItemBean.newType = item.newType;
                simpleFavoriteItemBean.custom_name = item.custom_name;
                simpleFavoriteItemBean.address = item.address;
                simpleFavoriteItemBean.classification = item.classification;
                simpleFavoriteItemBean.top_time = item.top_time;
                simpleFavoriteList.add(simpleFavoriteItemBean);
            }
        }
        return simpleFavoriteList;
    }
}