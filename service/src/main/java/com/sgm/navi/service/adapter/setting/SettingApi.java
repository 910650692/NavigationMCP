package com.sgm.navi.service.adapter.setting;

import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.route.RoutePreferenceID;


public interface SettingApi {

    /**
     * 初始化设置
     */
    void initSetting();

    /**
     * 注册回调
     * @param key 设置项
     * @param resultCallback 回调
     */
    void registerCallback(String key, SettingAdapterCallback resultCallback);

    /**
     * 获取路线偏好
     * @return 路线偏好
     */
    RoutePreferenceID getConfigKeyPlanPref();

    /**
     * 设置路线偏好
     * @param routePreferenceID 路线偏好
     * @return 返回错误码
     */
    int setConfigKeyPlanPref(RoutePreferenceID routePreferenceID);

    /**
     * 设置避开限行状态
     * @param avoidLimit 限行状态 true：开启限行 false：关闭限行
     * @return 0:成功 其他：失败
     */
    int setConfigKeyAvoidLimit(boolean avoidLimit);

    /**
     * 获取避开限行状态
     * @return 限行状态 true：开启限行 false：关闭限行
     */
    boolean getConfigKeyAvoidLimit();

    /**
     * 获取车牌号
     * @return 车牌号
     */
    String getConfigKeyPlateNumber();

    /**
     * 设置车牌号
     * @param carNumber 车牌号
     * @return 返回错误码
     */
    int setConfigKeyPlateNumber(String carNumber);

    /**
     * 获取常去地点
     * @return 常去地点状态 true：开启 false：关闭
     */
    boolean getConfigKeyOftenArrived();

    /**
     * 设置常去地点
     * @param oftenArrived 设置常去地点状态 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeyOftenArrived(boolean oftenArrived);

    /**
     * 获取自动退出全览
     * @return 自动退出全览状态 true：开启 false：关闭
     */
    boolean getConfigKeyAutoExitPreview();

    /**
     * 设置自动退出全览
     * @param autoExitPreview 自动退出全览状态 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeyAutoExitPreview(boolean autoExitPreview);

    /**
     * 获取导航播报音量
     * @return 导航播报音量
     */
    int getConfigKeyBroadcastVolume();

    /**
     * 设置导航播报音量
     * @param broadcastVolume 导航播报音量
     * @return 返回错误码
     */
    int setConfigKeyBroadcastVolume(int broadcastVolume);

    /**
     * 获取导航播报模式
     * @return 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     */
    int getConfigKeyBroadcastMode();

    /**
     * 设置导航播报模式
     * @param broadcastMode 导航播报模式 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报 ；默认值2
     * @return 0:成功 其他：失败
     */
    int setConfigKeyBroadcastMode(int broadcastMode);

    /**
     * 获取巡航播报前方路况
     * @return 巡航播报前方路况 true：开启 false：关闭
     */
    boolean getConfigKeyRoadWarn();

    /**
     * 设置巡航播报前方路况
     * @param roadWarn 巡航播报前方路况 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeyRoadWarn(boolean roadWarn);

    /**
     *  获取巡航播报电子眼播报
     * @return 巡航播报电子眼播报 true：开启 false：关闭
     */
    boolean getConfigKeySafeBroadcast();

    /**
     * 设置巡航播报电子眼播报
     * @param safeBroadcast 巡航播报电子眼播报 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeySafeBroadcast(boolean safeBroadcast);

    /**
     * 获取巡航播报安全提醒
     * @return 巡航播报安全提醒 true：开启 false：关闭
     */
    boolean getConfigKeyDriveWarn();

    /**
     * 设置巡航播报安全提醒
     * @param driveWarn 巡航播报安全提醒 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeyDriveWarn(boolean driveWarn);

    /**
     * 获取地图视角
     * @return 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     */
    int getConfigKeyMapviewMode();

    /**
     * 设置地图视角
     * @param mapViewMode 地图视角 0: 2D车首上，默认态; 1: 2D北上; 2: 3D车首上
     * @return 返回错误码
     */
    int setConfigKeyMapviewMode(int mapViewMode);

    /**
     * 获取路况开关
     * @return 路况开关 true：开启 false：关闭
     */
    boolean getConfigKeyRoadEvent();

    /**
     * 设置路况开关
     * @param roadEvent 路况开关 true：开启 false：关闭
     * @return 返回错误码
     */
    int setConfigKeyRoadEvent(boolean roadEvent);

    /**
     * 获取车辆动力类型
     * @return 1:无,未设置车牌号默认值 0:燃油车,已设置车牌号默认值 1:纯电动 2:插电式混动 3:油气两用
     */
    int getConfigKeyPowerType();

    /**
     * 获取混音方式
     * @return 混音方式 2：暂停音乐；  3：压低音乐，默认
     */
    int getConfigKeyAudioMixMode();

    /**
     * 设置混音方式
     * @param audioMixMode 混音方式 2：暂停音乐；  3：压低音乐，默认
     * @return 返回错误码
     */
    int setConfigKeyAudioMixMode(int audioMixMode);

    /**
     * 设置静音状态
     * @param mute 静音状态 0：不静音，默认态； 1：静音
     * @return 返回错误码
     */
    int setConfigKeyMute(int mute);

    /**
     * 获取静音状态
     * @return 静音状态 0：不静音，默认态； 1：静音
     */
    int getConfigKeyMute();

    /**
     * 设置白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     * @param dayNightMode 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     * @return 返回错误码
     */
    int setConfigKeyDayNightMode(ThemeType dayNightMode);

    /**
     * 获取白天黑夜
     * @return 白天黑夜 16：自动模式，默认态； 17：日间模式； 18：夜间模式
     */
    int getConfigKeyDayNightMode();
}