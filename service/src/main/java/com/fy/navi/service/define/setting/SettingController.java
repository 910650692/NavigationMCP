package com.fy.navi.service.define.setting;

public interface SettingController {
    String SETTING_KEY_PREFIX = "setting_";
    String VALUE_GENERIC_TRUE = "true";
    String VALUE_GENERIC_FALSE = "false";
    // 限行开关
    String KEY_SETTING_GUIDE_AVOID_LIMIT = "setting_guide_avoid_limit";
    // 车牌号
    String KEY_SETTING_GUIDE_VEHICLE_NUMBER = "setting_guide_vehicle_number";
    // 白天黑夜
    String KEY_SETTING_DISPLAY_MODE = "automatic_daytime";
    String VALUE_DISPLAY_MODE_DAYTIME = "daytime";
    String VALUE_DISPLAY_MODE_NIGHT = "night";
    String VALUE_DISPLAY_MODE_AUTO = "auto";
    // 车道级导航开关
    String KEY_SETTING_GUIDE_VEHICLE_GUIDE = "setting_guide_vehicle_guide";
    // 补能计划
    String KEY_SETTING_GUIDE_CHARGING_PLAN = "setting_guide_charging_plan";
    // 视角切换模式
    String SETTING_GUIDE_MAP_MODE = "setting_guide_map_mode";
    String VALUE_MAP_MODE_NORTH_2D = "navi_map_mode_north_2d"; // 2D正北向上
    String VALUE_MAP_MODE_CAR_2D = "navi_map_mode_car_2d"; // 2D车头向上
    String VALUE_MAP_MODE_CAR_3D = "navi_map_mode_car_3d"; // 3D车头向上
    // 路线偏好
    String KEY_SETTING_GUIDE_ROUTE_PREFERENCE = "setting_guide_route_preference";
    String VALUE_ROUTE_PREFERENCE_RECOMMEND = "recommend"; //高德推荐
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION = "avoid_congestion";//躲避拥堵
    String VALUE_ROUTE_PREFERENCE_LESS_CHARGE = "less_charge";//少收费
    String VALUE_ROUTE_PREFERENCE_NOT_HIGHWAY = "not_highway";//不走高速
    String VALUE_ROUTE_PREFERENCE_FIRST_HIGHWAY = "first_highway";//高速优先
    String VALUE_ROUTE_PREFERENCE_FIRST_MAIN_ROAD = "first_main_road";//大路优先
    String VALUE_ROUTE_PREFERENCE_FASTEST_SPEED = "fastest_speed";//速度最快
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE = "avoid_congestion_and_less_charge";//躲避拥堵+少收费
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_NOT_HIGHWAY = "avoid_congestion_and_not_highway";//躲避拥堵+不走高速
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_HIGHWAY = "avoid_congestion_and_first_highway";//躲避拥堵+高速优先
    String VALUE_ROUTE_PREFERENCE_LESS_CHARGE_AND_NOT_HIGHWAY = "less_charge_and_not_highway";//少收费+不走高速
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY = "avoid_congestion_and_less_charge_not_highway";//躲避拥堵+少收费+不走高速
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_MAIN_ROAD = "avoid_congestion_and_first_main_road";//躲避拥堵+大路优先
    String VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FASTEST_SPEED = "avoid_congestion_and_fastest_speed"; //躲避拥堵+速度最快
    // 巡航播报
    String KEY_SETTING_CRUISE_BROADCAST = "setting_cruise_broadcast"; //巡航播报开关
    String KEY_SETTING_BROADCAST_ELECTRONIC_EYE = "electronic_eye";// 电子眼播报
    String KEY_SETTING_BROADCAST_ROAD_CONDITIONS = "road_conditions";// 前方路况
    String KEY_SETTING_BROADCAST_SAFE_REMINDER = "safe_reminder";// 安全提醒
    // 导航播报
    String KEY_SETTING_NAVI_BROADCAST = "setting_navi_broadcast";
    String VALUE_NAVI_BROADCAST_DETAIL = "detail"; // 详细
    String VALUE_NAVI_BROADCAST_CONCISE = "concise";// 简洁
    String VALUE_NAVI_BROADCAST_SIMPLE = "simple";// 极简
    // 实时路况
    String KEY_SETTING_ROAD_CONDITION = "setting_road_condition";
    // 收藏点
    String KEY_SETTING_FAVORITE_POINT = "setting_favorite_point";
    // 充电桩
    String KEY_SETTING_CHARGING_STATION = "setting_charging_station";
    // 自动比例尺
    String KEY_SETTING_AUTO_SCALE = "setting_auto_scale";
    // 地图文字大小
    String KEY_SETTING_TEXT_SIZE = "setting_text_size";
    String VALUE_NAVI_TEXT_SIZE_STANDARD = "setting_text_size_standard";
    String VALUE_NAVI_TEXT_SIZE_LARGE = "setting_text_size_large";
    // 个性化车标
    String KEY_SETTING_CAR_LOGO = "setting_car_logo";
    String VALUE_NAVI_CAR_LOGO_DEFAULT = "setting_car_logo_default";
    String VALUE_NAVI_CAR_LOGO_BRAND = "setting_car_logo_brand";
    String VALUE_NAVI_CAR_LOGO_SPEED = "setting_car_logo_speed";
    // 第一次进入设置
    String KEY_SETTING_FIRST_IN_SETTING = "setting_first_in_setting";
    // 3D建筑
    String KEY_SETTING_3D_BUILDING = "setting_3d_building";
    // 授权时间
    String KEY_SETTING_PRIVACY_STATUS = "setting_privacy_status";
    String VALUE_PRIVACY_ONE_YEAR = "setting_privacy_one_year";
    String VALUE_PRIVACY_NEVER = "setting_privacy_never";
    String KEY_SETTING_PRIVACY_END_DATE = "setting_privacy_end_date";
    // 收藏夹同步时间
    String KEY_SETTING_SYNC_TIME = "setting_sync_time";
    // 是否主图显示家或者公司
    String KEY_SETTING_HOME_COMPANY_DISPLAYED = "setting_home_company_displayed";
    // 静音状态
    String KEY_SETTING_VOICE_MUTE = "setting_voice_mute";
    //声音打开
    String VALUE_VOICE_MUTE_ON = "setting_voice_mute_on";
    //声音关闭
    String VALUE_VOICE_MUTE_OFF = "setting_voice_mute_off";
    // 是否是电车
    String KEY_SETTING_IS_EV_CAR = "setting_is_ev_car";
    // 是否是混动
    String KEY_SETTING_IS_PHEV_CAR = "setting_is_phev_car";
    // 是否绑定微信
    String KEY_SETTING_IS_WE_CHAT_BIND = "setting_is_we_chat_bind";
    // 是否发送目的地最后一公里
    String KEY_SETTING_IS_SEND_DESTINATION_LAST_MILE = "setting_is_send_destination_last_mile";
    // 是否自动记录行程
    String KEY_SETTING_IS_AUTO_RECORD = "setting_is_auto_record";
    // 当前选中的语音包图标
    String KEY_SETTING_VOICE_ICON = "setting_voice_icon";
    // 当前选中的语音包名称
    String KEY_SETTING_VOICE_NAME = "setting_voice_name";
    // 当前选中的语音包
    String KEY_SETTING_VOICE_PACKAGE = "setting_voice_package";
    // 当前选中的语音包地址
    String KEY_SETTING_VOICE_PATH = "setting_voice_path";

    // 渠道号
    String KEY_SETTING_CHANNEL_ID = "setting_channel_id";

    String GUIDE_LOGIN_IS_CANCEL = "GUIDE_LOGIN_IS_CANCEL";
}
