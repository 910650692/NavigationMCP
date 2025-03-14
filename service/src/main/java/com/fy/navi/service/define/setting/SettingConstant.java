package com.fy.navi.service.define.setting;

import com.fy.navi.service.define.route.RoutePreferenceID;

public interface SettingConstant {
    /*** 内聚图层开关，工程面板中增加控制按钮 **/
    boolean isInnerLayerStyle = true;
    /*** 多屏是否同步 该参数非常重要 **/
    boolean isDisplaySync = true;
    /*** 是否深色模式 **/
    boolean isNightMode = false;
    // TODO: 2024/12/22 这个导航类型由客户端决定，不予存储，后续需要删除
    boolean isSimulateMode = true;

    /***北京天安门 **/
    double DEFAULT_LAT_BJ = 39.908696;
    double DEFAULT_LON_BJ = 116.397496;
    double DEFAULT_ALT_BJ = 0.0;
    /*** 上海东方明珠 **/
     double DEFAULT_LAT_SH = 31.239668;
     double DEFAULT_LON_SH = 121.499779;
     double DEFAULT_ALT_SH = 0.0;

     // ------------------------------------ map ----------------------------------------------

     interface RealTimeTrafficCondition {
     String KEY = "setting_real_time_traffic_condition";
     String VALUE_ON = "on";
     String VALUE_OFF = "off";
     }

     interface DayNightMode {
     String KEY = "setting_day_night_mode";
     String VALUE_DAY = "day";
     String VALUE_NIGHT = "night";
     String VALUE_AUTO = "auto";
     }

     // ------------------------------------ broadcast ----------------------------------------------
     /**
     * 1 简洁播报
     */
    int BROADCAST_EASY = 1;
    /**
     * 2 详细播报
     */
    int BROADCAST_DETAIL = 2;

    /**
     * 3 极简播报
     */
    int BROADCAST_MINIMALISM = 3;

    interface NavigationBroadcast {
        String KEY = "setting_navigation_broadcast";
        String VALUE_DETAILS = "details";
        String VALUE_SUCCINCT = "succinct";
        String VALUE_PROMPT_TONE_ONLY = "prompt_tone_only";
    }

    interface CruiseMode {
        String KEY = "setting_cruise_announcer";
        String VALUE_ELECTRONIC_EYE = "electronic_eye";
        String VALUE_ROAD_CONDITION = "road_condition";
        String VALUE_SAFETY_REMINDER = "safety_reminder";
    }

    // ------------------------------------ navigation ---------------------------------------------

    interface RouteCriteria {
        String KEY = "setting_route_preference";
        String VALUE_INTELLIGENT_RECOMMEN_DATION = "intelligent_recommen_dation";
        String VALUE_AVOID_CONGESTION = "avoid_congestion";
        String VALUE_TIME_PRIORITY = "time_priority";
        String VALUE_FEE_AVOIDANCE = "fee_avoidance";
        String VALUE_AVOID_THE_HIGHWAY = "avoid_the_highway";
        String VALUE_HIGH_SPEED_PRIORITY = "high_speed_priority";

        String KEY_AVOID_TOLL = "avoid_toll";
        String KEY_USING_FERRY = "using_ferry";
        String KEY_ROUTE_MODEL = "route_model";

        String VALUE_TRUE = "true";
        String VALUE_FALSE = "false";
        String VALUE_ROUTE_MODEL_FAST = "fast";
        String VALUE_ROUTE_MODEL_SHORT = "short";
    }
}
