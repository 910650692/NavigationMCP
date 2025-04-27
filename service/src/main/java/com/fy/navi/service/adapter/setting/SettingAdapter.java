package com.fy.navi.service.adapter.setting;


import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.setting.SettingManager;

import java.util.Objects;

public final class SettingAdapter {
    private static final String SETTING_API_PKG = Objects.requireNonNull(SettingAdapter.class.getPackage()).getName();
    private static final String SETTING_API_CLS = "SettingAdapterImpl";
    private final SettingApi mSettingApi;

    private final SettingManager mSettingManager;

    private SettingAdapter() {
        mSettingApi = (SettingApi) AdapterConfig.getObject(SETTING_API_PKG, SETTING_API_CLS);
        mSettingManager = SettingManager.getInstance();
        mSettingManager.init();
    }

    /**
     * 初始化设置
     */
    public void initSetting() {
        mSettingApi.initSetting();
    }

    /**
     * 注册回调
     * @param key 设置项
     * @param resultCallback 回调
     */
    public void registerCallback(final String key, final SettingAdapterCallback resultCallback) {
        mSettingApi.registerCallback(key, resultCallback);
    }

    /**
     * 获取路线偏好
     * @param routePreferenceID 路线偏好
     * @return 路线偏好
     */
    public int setConfigKeyPlanPref(final RoutePreferenceID routePreferenceID) {
        return mSettingApi.setConfigKeyPlanPref(routePreferenceID);
    }

    /**
     * 获取路线偏好
     * @return 路线偏好
     */
    public RoutePreferenceID getConfigKeyPlanPref() {
        return mSettingApi.getConfigKeyPlanPref();
    }

    /**
     * 设置避开限行状态
     * @param avoidLimit 限行状态 true：开启限行 false：关闭限行
     * @return 0:成功 其他：失败
     */
    public int setConfigKeyAvoidLimit(final boolean avoidLimit) {
        return mSettingApi.setConfigKeyAvoidLimit(avoidLimit);
    }

    /**
     * 获取避开限行状态
     * @return 限行状态 true：开启限行 false：关闭限行
     */
    public boolean getConfigKeyAvoidLimit() {
        return mSettingApi.getConfigKeyAvoidLimit();
    }

    /**
     * 设置车牌号
     * @param carNumber 车牌号
     * @return 返回错误码
     */
    public int setConfigKeyPlateNumber(final String carNumber) {
        return mSettingApi.setConfigKeyPlateNumber(carNumber);
    }

    /**
     * 获取车牌号
     * @return 车牌号
     */
    public String getConfigKeyPlateNumber() {
        return mSettingApi.getConfigKeyPlateNumber();
    }

    /**
     * 设置常去地点
     * @param oftenArrived 设置常去地点状态 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeyOftenArrived(final boolean oftenArrived) {
        return mSettingApi.setConfigKeyOftenArrived(oftenArrived);
    }

    /**
     * 获取常去地点
     * @return 常去地点状态 true：开启 false：关闭
     */
    public boolean getConfigKeyOftenArrived() {
        return mSettingApi.getConfigKeyOftenArrived();
    }

    /**
     * 设置自动退出全览
     * @param autoExitPreview 自动退出全览状态 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeyAutoExitPreview(final boolean autoExitPreview) {
        return mSettingApi.setConfigKeyAutoExitPreview(autoExitPreview);
    }

    /**
     * 获取自动退出全览
     * @return 自动退出全览状态 true：开启 false：关闭
     */
    public boolean getConfigKeyAutoExitPreview() {
        return mSettingApi.getConfigKeyAutoExitPreview();
    }

    /**
     * 设置导航播报音量
     * @param broadcastVolume 导航播报音量
     * @return 返回错误码
     */
    public int setConfigKeyBroadcastVolume(final int broadcastVolume) {
        return mSettingApi.setConfigKeyBroadcastVolume(broadcastVolume);
    }

    /**
     * 获取导航播报音量
     * @return 导航播报音量
     */
    public int getConfigKeyBroadcastVolume() {
        return mSettingApi.getConfigKeyBroadcastVolume();
    }

    /**
     * 设置导航播报模式
     * @param broadcastMode 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     * @return 0:成功 其他：失败
     */
    public int setConfigKeyBroadcastMode(final int broadcastMode) {
        final int code = mSettingApi.setConfigKeyBroadcastMode(broadcastMode);
        if (code == 0) {
            switch (broadcastMode) {
                case 1:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_CONCISE);
                    break;
                case 2:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_DETAIL);
                    break;
                case 3:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_NAVI_BROADCAST, SettingController.VALUE_NAVI_BROADCAST_SIMPLE);
                    break;
                default:
                    break;
            }
        }
        return code;
    }

    /**
     * 获取导航播报模式
     * @return 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    public int getConfigKeyBroadcastMode() {
        return mSettingApi.getConfigKeyBroadcastMode();
    }

    /**
     * 设置巡航播报前方路况
     * @param roadWarn 巡航播报前方路况 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeyRoadWarn(final boolean roadWarn) {
        return mSettingApi.setConfigKeyRoadWarn(roadWarn);
    }

    /**
     * 获取巡航播报前方路况
     * @return 巡航播报前方路况 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadWarn() {
        return mSettingApi.getConfigKeyRoadWarn();
    }

    /**
     * 设置巡航播报电子眼播报
     * @param safeBroadcast 巡航播报电子眼播报 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeySafeBroadcast(final boolean safeBroadcast) {
        return mSettingApi.setConfigKeySafeBroadcast(safeBroadcast);
    }

    /**
     *  获取巡航播报电子眼播报
     * @return 巡航播报电子眼播报 true：开启 false：关闭
     */
    public boolean getConfigKeySafeBroadcast() {
        return mSettingApi.getConfigKeySafeBroadcast();
    }

    /**
     * 设置巡航播报安全提醒
     * @param driveWarn 巡航播报安全提醒 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeyDriveWarn(final boolean driveWarn) {
        return mSettingApi.setConfigKeyDriveWarn(driveWarn);
    }

    /**
     * 获取巡航播报安全提醒
     * @return 巡航播报安全提醒 true：开启 false：关闭
     */
    public boolean getConfigKeyDriveWarn() {
        return mSettingApi.getConfigKeyDriveWarn();
    }

    /**
     * 设置地图视角
     * @param mapViewMode 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     * @return 返回错误码
     */
    public int setConfigKeyMapviewMode(final int mapViewMode) {
        final int code = mSettingApi.setConfigKeyMapviewMode(mapViewMode);
        if (code == 0) {
            switch (mapViewMode) {
                case 0:
                    mSettingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_2D);
                    break;
                case 1:
                    mSettingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_NORTH_2D);
                    break;
                case 2:
                    mSettingManager.insertOrReplace(SettingController.SETTING_GUIDE_MAP_MODE, SettingController.VALUE_MAP_MODE_CAR_3D);
                    break;
                default:
                    break;
            }
        }
        return code;
    }

    /**
     * 获取地图视角
     * @return 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    public int getConfigKeyMapviewMode() {
        return mSettingApi.getConfigKeyMapviewMode();
    }

    /**
     * 设置路况开关
     * @param roadEvent 路况开关 true：开启 false：关闭
     * @return 返回错误码
     */
    public int setConfigKeyRoadEvent(final boolean roadEvent) {
        final int code = mSettingApi.setConfigKeyRoadEvent(roadEvent);
        if (code == 0) {
            mSettingManager.insertOrReplace(SettingController.KEY_SETTING_ROAD_CONDITION, String.valueOf(roadEvent));
        }
        return code;
    }

    /**
     * 获取路况开关
     * @return 路况开关 true：开启 false：关闭
     */
    public boolean getConfigKeyRoadEvent() {
        return mSettingApi.getConfigKeyRoadEvent();
    }

    /**
     * 获取车辆动力类型
     * @return 1:无,未设置车牌号默认值 0:燃油车,已设置车牌号默认值 1:纯电动 2:插电式混动 3:油气两用
     */
    public int getConfigKeyPowerType() {
        return mSettingApi.getConfigKeyPowerType();
    }

    /**
     * 获取混音方式
     * @return 混音方式 2：暂停音乐；  3：压低音乐，默认
     */
    public int getConfigKeyAudioMixMode() {
        return mSettingApi.getConfigKeyAudioMixMode();
    }

    /**
     * 设置混音方式
     * @param audioMixMode 混音方式 2：暂停音乐；  3：压低音乐，默认
     * @return 返回错误码
     */
    public int setConfigKeyAudioMixMode(final int audioMixMode) {
        return mSettingApi.setConfigKeyAudioMixMode(audioMixMode);
    }

    /**
     * 设置静音状态
     * @param mute 静音状态 0：不静音，默认态； 1：静音
     * @return 返回错误码
     */
    public int setConfigKeyMute(final int mute) {
        final int code = mSettingApi.setConfigKeyMute(mute);
        if (code == 0) {
            switch (mute) {
                case 0:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_ON);
                    break;
                case 1:
                    mSettingManager.insertOrReplace(SettingController.KEY_SETTING_VOICE_MUTE, SettingController.VALUE_VOICE_MUTE_OFF);
                    break;
                default:
                    break;
            }
        }
        return code;
    }

    /**
     * 获取静音状态
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    public int getConfigKeyMute() {
        return mSettingApi.getConfigKeyMute();
    }

    /**
     * 设置白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     * @param dayNightMode 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     * @return 返回错误码
     */
    public int setConfigKeyDayNightMode(final ThemeType dayNightMode) {
        return mSettingApi.setConfigKeyDayNightMode(dayNightMode);
    }

    /**
     * 获取白天黑夜
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    public int getConfigKeyDayNightMode() {
        return mSettingApi.getConfigKeyDayNightMode();
    }

    /**
     * 获取单例
     * @return 单例
     */
    public static SettingAdapter getInstance() {
        return SInstanceHolder.INSTANCE;
    }

    /**
     * 单例Holder
     */
    private static final class SInstanceHolder {
        static final SettingAdapter INSTANCE = new SettingAdapter();
    }
}