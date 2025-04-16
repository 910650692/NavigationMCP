package com.fy.navi.service.adapter.navi;

import androidx.annotation.IntDef;

import com.android.utils.ScreenUtils;

public interface NaviConstant {
    boolean IS_SYSTEM_AUDIO = true;
    long NAVI_ID = 0;//真实导航ID
    long NAVI_SIM_ID = 1;//模拟导航ID
    long NAVI_CRUISE_ID = 2;//巡航ID
    String KEY_NAVI_MODEL = "NaviGuidanceModel";
    String NAVI_CONTROL = "NAVI_CONTROL";
    // 导航转向图标宽度信息
    int TURN_ICON_SIZE = ScreenUtils.Companion.getInstance().dp2px(155);
    int NEXT_TURN_ICON_SIZE = ScreenUtils.Companion.getInstance().dp2px(100);
    //离线转向图标，本地图片名
    String HUD_RES_PREFIX = "global_image_hud_";
    String ICON_RES_NAME = "sou";
    String NIGHT = "_night";
    // 1000公里
    int THOUSANDKILOMETRE = 1000 * 1000;
    /**
     * 道路等级 高速路
     */
    byte ROAD_CLASS_HIGH_SPEED = 0;
    /**
     * 道路等级 城市快速路 Urban rapid
     */
    byte ROAD_CLASS_URBAN_RAPID = 6;
    /**
     * 隧道内分叉
     */
    byte CROSS_NAV_TUNNEL_INNER = 1;
    /**
     * 隧道外分叉
     */
    byte CROSS_NAV_TUNNEL_OUT = 2;
    int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
    int LANE_TYPE_NULL = 255;
    int LANE_TYPE_NORMAL = 0;
    int LANE_TYPE_BUS = 1;
    int LANE_TYPE_OTHER = 2;
    int LANE_TYPE_TIDAL = 3;
    int LANE_TYPE_VARIABLE = 4;

    // 小号车道线最多条数
    int LANE_SMALL_MAX = 8;
    // 距离出口最小提示距离2000米
    int MIN_EXIT_DIST = 2000;
    /**
     * 出入口编号最大长度
     */
    int EXIT_STRING_MAX_NUMBER = 5;
    long ONE_KILOMETER = 1000;
    long START_SEARCH_PARK_DISTANCE = 1500;

    interface ManeuverDataType {
        int MANEUVER = 1; //转向图标图片数据
        int MANEUVER_ICON = 2;    //转向图标信息
        int EXIT_DIRECTION = 3;  //导航过程中传出出入口的编号、方向信息
    }

    interface BizRouteType {
        int BIZ_ROUTE_TYPE_ARROW = 6017;
    }

    interface ChangeNaviPathResult {
        /**
         * 切换成功
         */
        int CHANGE_NAVI_PATH_RESULT_SUCCESS = 1;
        /**
         * PathID无效切换失败
         */
        int CHANGE_NAVI_PATH_RESULT_ERROR_PATH_ID_INVALID = 2;
        /**
         * 与主路线PathID切换失败
         */
        int CHANGE_NAVI_PATH_RESULT_ERROR_SAME_PATH_ID = 3;
    }

    @IntDef({GpsStrengthState.GPS_NONE, GpsStrengthState.GPS_STRONG,
            GpsStrengthState.GPS_MEDIUM, GpsStrengthState.GPS_WEAK})
    @interface GpsStrengthState {//Strong, medium, weak
        int GPS_NONE = -1;
        int GPS_STRONG = 0;
        int GPS_MEDIUM = 1;
        int GPS_WEAK = 2;
    }

    @IntDef({SapaItemsType.AUTO_UNKNOWN_ERROR, SapaItemsType.SPAS_LIST,
            SapaItemsType.TOLL_STATION_LIST, SapaItemsType.TOLL_STATION_AND_SPAS})
    @interface SapaItemsType {
        int AUTO_UNKNOWN_ERROR = -1;
        int SPAS_LIST = 0;                // 服务区 无收费站
        int TOLL_STATION_LIST = 1;        // 收费站 无服务区
        int TOLL_STATION_AND_SPAS = 3;    // 1个服务区 1个收费站
    }

    @IntDef({OverviewType.OVERVIEW_DEFAULT, OverviewType.OVERVIEW_SELECT, OverviewType.OVERVIEW_FIXED})
    @interface OverviewType {
        int OVERVIEW_DEFAULT = 0;            //全览默认状态
        int OVERVIEW_SELECT = 1;            //全览选择状态
        int OVERVIEW_FIXED = 2;            //全览固定状态
    }

    @IntDef({VariationType.VARIATION_MUTE, VariationType.VARIATION_BROADCAST, VariationType.VARIATION_SELECT})
    @interface VariationType {
        int VARIATION_BROADCAST = 0;       //播报中
        int VARIATION_MUTE = 1;            //静音
        int VARIATION_SELECT = 2;           //全览固定
    }

    @IntDef({BroadcastType.BROADCAST_CONCISE, BroadcastType.BROADCAST_DETAIL, BroadcastType.BROADCAST_MINIMALISM})
    @interface BroadcastType {
        int BROADCAST_CONCISE = 1;       //经典简洁播报
        int BROADCAST_DETAIL = 2;        //详细播报
        int BROADCAST_MINIMALISM = 3;    //极简
    }

    @IntDef({NaviSceneType.SCENE_TBT, NaviSceneType.SCENE_LANES, NaviSceneType.SCENE_VIA_LIST,
            NaviSceneType.SCENE_PARK_LIST, NaviSceneType.SCENE_VIA_INFO, NaviSceneType.SCENE_LAST_MILE,
            NaviSceneType.SCENE_PARALLEL, NaviSceneType.SCENE_PREFERENCE, NaviSceneType.SCENE_SPEED,
            NaviSceneType.SCENE_SAPA, NaviSceneType.SCENE_CROSS_IMAGE, NaviSceneType.SCENE_CONTROL})
    @interface NaviSceneType {
        int SCENE_TBT = 0;
        int SCENE_LANES = 1;
        int SCENE_VIA_LIST = 2;
        int SCENE_PARK_LIST = 3;
        int SCENE_VIA_INFO = 4;
        int SCENE_LAST_MILE = 5;
        int SCENE_PARALLEL = 6;
        int SCENE_PREFERENCE = 7;
        int SCENE_SPEED = 8;
        int SCENE_SAPA = 9;
        int SCENE_CROSS_IMAGE = 10;
        int SCENE_CONTROL = 11;
    }

    @IntDef({CrossType.CROSS_TYPE_GRID, CrossType.CROSS_TYPE_VECTOR,
            CrossType.CROSS_TYPE_3_D})
    @interface CrossType {
        int CROSS_TYPE_GRID = 1;//栅格图, 背景为JPG，箭头为PNG
        int CROSS_TYPE_VECTOR = 3;//矢量大图
        int CROSS_TYPE_3_D = 4;//三维放大图
    }

    @IntDef({SpeedType.SPEED_OVERALL, SpeedType.SPEED_GREEN_WAVE, SpeedType.SPEED_CAMERA,
            SpeedType.SPEED})
    @interface SpeedType {
        int SPEED_OVERALL = 1;//区间测速
        int SPEED_GREEN_WAVE = 2;//绿波车速
        int SPEED_CAMERA = 3;//摄像头限速
        int SPEED = 4;//车速
    }

    interface LocSwitchRoadType {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int LOC_SWITCH_NULL = -1;
        int LOC_SWITCH_MAIN_TO_SIDE = 0;
        int LOC_SWITCH_SIDE_TO_MAIN = 1;
        int LOC_SWITCH_UP_BRIDGE_TO_DOWN_BRIDGE = 2;
        int LOC_SWITCH_DOWN_BRIDGE_TO_UP_BRIDGE = 3;
    }

    interface TMCTrafficStatus {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int TRAFFIC_STATUS_UNKONW = 0;
        int TRAFFIC_STATUS_OPEN = 1;
        int TRAFFIC_STATUS_SLOW = 2;
        int TRAFFIC_STATUS_JAM = 3;
        int TRAFFIC_STATUS_CONGESTED = 4;
        int TRAFFIC_STATUS_EXTREMELY_OPEN = 5;
    }

    interface VectorCrossImageType {
        int VECTOR_CROSS_IMAGE_TYPE_INVALID = -1;//非法值
        int VECTOR_CROSS_IMAGE_TYPE_COMMON = 0;//普通矢量路口大图
        int VECTOR_CROSS_IMAGE_TYPE_ROUNDABOUT = 1;//环岛矢量路口大图
        int VECTOR_CROSS_IMAGE_TYPE_CONFUSION = 2;//混淆矢量路口大图
        int VECTOR_CROSS_IMAGE_TYPE_NEAR = 3;//近接矢量路口大图
        int VECTOR_CROSS_IMAGE_TYPE_DOUBLE_LIGHT = 4;//跨红绿灯矢量路口大图
        int VECTOR_CROSS_IMAGE_TYPE_SOLID_LINE = 6;//长实线大图
        int VECTOR_CROSS_IMAGE_TYPE_SOLID_NEAR = 7;//长实线近接大图
        int VECTOR_CROSS_IMAGE_TYPE_MIX_REVERSE = 10;//混淆型逆行大图
    }

    interface GuideParamType {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int GUIDE_PARAM_INVALID = 0;
        int GUIDE_PARAM_COMMON = 1;
        int GUIDE_PARAM_TMC = 2;
        int GUIDE_PARAM_TR = 3;
        int GUIDE_PARAM_SAPA = 4;
        int GUIDE_PARAM_LANE = 5;
        int GUIDE_PARAM_CRUISE = 6;
        int GUIDE_PARAM_NAVI = 7;
        int GUIDE_PARAM_CAMERA = 8;
        int GUIDE_PARAM_EXIT = 9;
        int GUIDE_PARAM_CROSSING = 10;
        int GUIDE_PARAM_TTS_PLAY = 11;
        int GUIDE_PARAM_VEHICLE = 12;
        int GUIDE_PARAM_NETWORK = 13;
        int GUIDE_PARAM_EASY_3_D = 14;
        int GUIDE_PARAM_EMULATOR = 15;
        int GUIDE_PARAM_JILI_AUTO = 16;
        int GUIDE_PARAM_NAVI_WEATHER = 17;
    }

    interface TMCViaIndex {
        int VIA_0 = 0;
        int VIA_1 = 1;
        int VIA_2 = 2;
        int VIA_3 = 3;
        int VIA_4 = 4;
        int VIA_5 = 5;
        int VIA_6 = 6;
        int VIA_7 = 7;
        int VIA_8 = 8;
        int VIA_9 = 9;
        int VIA_10 = 10;
        int VIA_11 = 11;
        int VIA_12 = 12;
        int VIA_13 = 13;
        int VIA_14 = 14;
        int VIA_15 = 15;
        int VIA_16 = 16;
        int VIA_17 = 17;
        int VIA_18 = 18;
        int VIA_19 = 19;
    }

    //车道线
    interface LaneActionConstants {
        int LANE_ACTION_AHEAD_AND_N_ULL = 0;
        int LANE_ACTION_AHEAD_AND_N_ULL_NO_ACTION = 1;
        int LANE_ACTION_LEFT_AND_N_ULL = 2;
        int LANE_ACTION_LEFT_AND_N_ULL_NO_ACTION = 3;
        int LANE_ACTION_AHEAD_LEFT_AND_AHEAD = 4;
        int LANE_ACTION_AHEAD_LEFT_AND_LEFT = 5;
        int LANE_ACTION_AHEAD_LEFT_NO_ACTION = 6;
        int LANE_ACTION_RIGHT_AND_N_ULL = 7;
        int LANE_ACTION_RIGHT_AND_N_ULL_NO_ACTION = 8;
        int LANE_ACTION_AHEAD_RIGHT_AND_AHEAD = 9;
        int LANE_ACTION_AHEAD_RIGHT_AND_RIGHT = 10;
        int LANE_ACTION_AHEAD_RIGHT_NO_ACTION = 11;
        int LANE_ACTION_LU_TURN_AND_N_ULL = 12;
        int LANE_ACTION_LU_TURN_AND_N_ULL_NO_ACTION = 13;
        int LANE_ACTION_LEFT_RIGHT_AND_LEFT = 14;
        int LANE_ACTION_LEFT_RIGHT_AND_RIGHT = 15;
        int LANE_ACTION_LEFT_RIGHT_NO_ACTION = 16;
        int LANE_ACTION_AHEAD_LEFT_RIGHT_AND_AHEAD = 17;
        int LANE_ACTION_AHEAD_LEFT_RIGHT_AND_LEFT = 18;
        int LANE_ACTION_AHEAD_LEFT_RIGHT_AND_RIGHT = 19;
        int LANE_ACTION_AHEAD_LEFT_RIGHT_NO_ACTION = 20;
        int LANE_ACTION_RU_TURN_AND_N_ULL = 21;
        int LANE_ACTION_RU_TURN_AND_N_ULL_NO_ACTION = 22;
        int LANE_ACTION_AHEAD_LU_TURN_AND_AHEAD = 23;
        int LANE_ACTION_AHEAD_LU_TURN_AND_LU_TURN = 24;
        int LANE_ACTION_AHEAD_LU_TURN_NO_ACTION = 25;
        int LANE_ACTION_AHEAD_RU_TURN_AND_AHEAD = 26;
        int LANE_ACTION_AHEAD_RU_TURN_AND_RU_TURN = 27;
        int LANE_ACTION_AHEAD_RU_TURN_NO_ACTION = 28;
        int LANE_ACTION_LEFT_LU_TURN_AND_LEFT = 29;
        int LANE_ACTION_LEFT_LU_TURN_AND_LU_TURN = 30;
        int LANE_ACTION_LEFT_LU_TURN_NO_ACTION = 31;
        int LANE_ACTION_RIGHT_RU_TURN_AND_RIGHT = 32;
        int LANE_ACTION_RIGHT_RU_TURN_AND_RU_TURN = 33;
        int LANE_ACTION_RIGHT_RU_TURN_NO_ACTION = 34;
        int LANE_ACTION_LEFT_IN_AHEAD_AND_N_ULL = 35;
        int LANE_ACTION_LEFT_IN_AHEAD_AND_N_ULL_NO_ACTION = 36;
        int LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_AHEAD = 37;
        int LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_LEFT = 38;
        int LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_LU_TRUN = 39;
        int LANE_ACTION_AHEAD_LEFT_LU_TURN_NO_ACTION = 40;
        int LANE_ACTION_RIGHT_LU_TURN_AND_RIGHT = 41;
        int LANE_ACTION_RIGHT_LU_TURN_AND_LU_TURN = 42;
        int LANE_ACTION_RIGHT_LU_TURN_NO_ACTION = 43;
        int LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_LEFT = 44;
        int LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_RIGHT = 45;
        int LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_LU_TURN = 46;
        int LANE_ACTION_LEFT_RIGHT_LU_TURN_NO_ACTION = 47;
        int LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_AHEAD = 48;
        int LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_RIGHT = 49;
        int LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_LU_TURN = 50;
        int LANE_ACTION_AHEAD_RIGHT_LU_TURN_NO_ACTION = 51;
        int LANE_ACTION_LEFT_RU_TURN_AND_LEFT = 52;
        int LANE_ACTION_LEFT_RU_TURN_AND_RU_TURN = 53;
        int LANE_ACTION_LEFT_RU_TURN_NO_ACTION = 54;
        int LANE_ACTION_BUS_AND_BUS = 55;
        int LANE_ACTION_BUS_NO_ACTION = 56;
        int LANE_ACTION_VARIABLE_AND_VARIABLE = 57;
        int LANE_ACTION_VARIABLE_NO_ACTION = 58;
        int LANE_ACTION_DEDICATED = 149;
        int LANE_ACTION_TIDAL = 150;
        int LANE_ACTION_EMPTY_NO_ACTION = 151;
        int LANE_ACTION_EMPTY = 152;


        // 分时车道底部公交/专用/潮汐/可变图标
//    private enum TimeLaneBottomAction
        int BACK_LANE_BUS_WORKABLE = 1;
        int BACK_LANE_SPECIAL_WORKABLE = 2;
        int BACK_LANE_TIDAL_WORKABLE = 3;
        int BACK_LANE_REVERSIBLE_WORKABLE = 4;
        int BACK_LANE_BUS_NO_WORKABLE = 5;
        int BACK_LANE_SPECIAL_NO_WORKABLE = 6;
        int BACK_LANE_TIDAL_NO_WORKABLE = 7;
        int BACK_LANE_REVERSIBLE_NO_WORKABLE = 8;

        int[][] NAVI_LANE_MAP = {
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_AND_N_ULL_NO_ACTION},// 直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_AND_N_ULL},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_AND_N_ULL_NO_ACTION},// 左转
                {NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_LEFT_AND_N_ULL},

                {NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_RIGHT_AND_N_ULL_NO_ACTION},// 右转
                {NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_RIGHT_AND_N_ULL},

                {NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_LU_TURN_AND_N_ULL},// 左U-Turn
                {NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LU_TURN_AND_N_ULL_NO_ACTION},

                {NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_RU_TURN_AND_N_ULL_NO_ACTION},// 右U-Turn
                {NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        LaneActionConstants.LANE_ACTION_RU_TURN_AND_N_ULL},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT_IN_AHEAD,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_IN_AHEAD_AND_N_ULL_NO_ACTION},// 左侧扩展车道 直行
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_IN_AHEAD,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT_IN_AHEAD,
                        LaneActionConstants.LANE_ACTION_LEFT_IN_AHEAD_AND_N_ULL},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_NO_ACTION},// 左转+直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_AND_LEFT},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_NO_ACTION},// 右转+直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_AND_RIGHT},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_NO_ACTION},// 左转+右转
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_AND_RIGHT},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_RIGHT_NO_ACTION},// 左转+直行+右转
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_RIGHT_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_RIGHT_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_RIGHT,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_RIGHT_AND_RIGHT},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_LU_TURN_NO_ACTION},// 左U-Turn + 直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_LU_TURN_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_AHEAD_LU_TURN_AND_LU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_RU_TURN_NO_ACTION},// 右U-Turn + 直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_RU_TURN_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        LaneActionConstants.LANE_ACTION_AHEAD_RU_TURN_AND_RU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_LU_TURN_NO_ACTION},// 左U-Turn + 左转
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_LEFT_LU_TURN_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_LEFT_LU_TURN_AND_LU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_RIGHT_RU_TURN_NO_ACTION},// 右U-Turn + 右转
                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_RIGHT_RU_TURN_AND_RIGHT},
                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        LaneActionConstants.LANE_ACTION_RIGHT_RU_TURN_AND_RU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_LU_TURN_NO_ACTION},// 左U-Turn + 左转 + 直行
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_LEFT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_AHEAD_LEFT_LU_TURN_AND_LU_TRUN},

                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_RIGHT_LU_TURN_NO_ACTION},// 左U-Turn + 右转
                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_RIGHT_LU_TURN_AND_RIGHT},
                {NaviConstant.LaneAction.LANE_ACTION_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_RIGHT_LU_TURN_AND_LU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_LU_TURN_NO_ACTION},// 左U-Turn + 左转 + 右转
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_RIGHT},
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_LEFT_RIGHT_LU_TURN_AND_LU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_LU_TURN_NO_ACTION},// 左U-Turn + 直行 + 右转
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_AHEAD,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_AHEAD},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RIGHT,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_RIGHT},
                {NaviConstant.LaneAction.LANE_ACTION_AHEAD_RIGHT_LU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LU_TURN,
                        LaneActionConstants.LANE_ACTION_AHEAD_RIGHT_LU_TURN_AND_LU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_LEFT_RU_TURN_NO_ACTION},// 左转 + 右U-Turn
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_LEFT,
                        LaneActionConstants.LANE_ACTION_LEFT_RU_TURN_AND_LEFT},
                {NaviConstant.LaneAction.LANE_ACTION_LEFT_RU_TURN,
                        NaviConstant.LaneAction.LANE_ACTION_RU_TURN,
                        LaneActionConstants.LANE_ACTION_LEFT_RU_TURN_AND_RU_TURN},

                {NaviConstant.LaneAction.LANE_ACTION_BUS,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_BUS_NO_ACTION},// 公交车道
                {NaviConstant.LaneAction.LANE_ACTION_BUS,
                        NaviConstant.LaneAction.LANE_ACTION_BUS,
                        LaneActionConstants.LANE_ACTION_BUS_AND_BUS},

                {NaviConstant.LaneAction.LANE_ACTION_VARIABLE,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_VARIABLE_NO_ACTION},// 可变车道
                {NaviConstant.LaneAction.LANE_ACTION_VARIABLE,
                        NaviConstant.LaneAction.LANE_ACTION_VARIABLE,
                        LaneActionConstants.LANE_ACTION_VARIABLE_AND_VARIABLE},

                {NaviConstant.LaneAction.LANE_ACTION_DEDICATED,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_DEDICATED},// 专用车道
                {NaviConstant.LaneAction.LANE_ACTION_DEDICATED,
                        NaviConstant.LaneAction.LANE_ACTION_DEDICATED,
                        LaneActionConstants.LANE_ACTION_DEDICATED},

                {NaviConstant.LaneAction.LANE_ACTION_TIDAL,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_TIDAL},// 潮汐车道
                {NaviConstant.LaneAction.LANE_ACTION_TIDAL,
                        NaviConstant.LaneAction.LANE_ACTION_TIDAL,
                        LaneActionConstants.LANE_ACTION_TIDAL},

                {NaviConstant.LaneAction.LANE_ACTION_EMPTY,
                        NaviConstant.LaneAction.LANE_ACTION_NULL,
                        LaneActionConstants.LANE_ACTION_EMPTY_NO_ACTION}, // 空车道
                {NaviConstant.LaneAction.LANE_ACTION_EMPTY,
                        NaviConstant.LaneAction.LANE_ACTION_EMPTY,
                        LaneActionConstants.LANE_ACTION_EMPTY},// 可行空车道
        };
    }

    interface LaneAction {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int LANE_ACTION_NULL = 255;
        int LANE_ACTION_AHEAD = 0;
        int LANE_ACTION_LEFT = 1;
        int LANE_ACTION_AHEAD_LEFT = 2;
        int LANE_ACTION_RIGHT = 3;
        int LANE_ACTION_AHEAD_RIGHT = 4;
        int LANE_ACTION_LU_TURN = 5;
        int LANE_ACTION_LEFT_RIGHT = 6;
        int LANE_ACTION_AHEAD_LEFT_RIGHT = 7;
        int LANE_ACTION_RU_TURN = 8;
        int LANE_ACTION_AHEAD_LU_TURN = 9;
        int LANE_ACTION_AHEAD_RU_TURN = 10;
        int LANE_ACTION_LEFT_LU_TURN = 11;
        int LANE_ACTION_RIGHT_RU_TURN = 12;
        int LANE_ACTION_LEFT_IN_AHEAD = 13;
        int LANE_ACTION_LEFT_L_UTURN = 14;
        int LANE_ACTION_RESERVED = 15;
        int LANE_ACTION_AHEAD_LEFT_LU_TURN = 16;
        int LANE_ACTION_RIGHT_LU_TURN = 17;
        int LANE_ACTION_LEFT_RIGHT_LU_TURN = 18;
        int LANE_ACTION_AHEAD_RIGHT_LU_TURN = 19;
        int LANE_ACTION_LEFT_RU_TURN = 20;
        int LANE_ACTION_BUS = 21;
        int LANE_ACTION_EMPTY = 22;
        int LANE_ACTION_VARIABLE = 23;
        int LANE_ACTION_DEDICATED = 24;
        int LANE_ACTION_TIDAL = 25;
    }
}
