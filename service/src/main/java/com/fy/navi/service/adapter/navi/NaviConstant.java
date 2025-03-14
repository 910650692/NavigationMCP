package com.fy.navi.service.adapter.navi;

import androidx.annotation.IntDef;

import com.android.utils.ScreenUtils;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

public interface NaviConstant {
    boolean isSystemAudio = true;
    long NAVI_ID = 0;//真实导航ID
    long NAVI_SIM_ID = 1;//模拟导航ID
    long NAVI_CRUISE_ID = 2;//巡航ID
    String KEY_NAVI_MODEL = "NaviGuidanceModel";
    // 导航转向图标宽度信息
    int turnIconSize = ScreenUtils.Companion.getInstance().dp2px(155);
    int nextTurnIconSize = ScreenUtils.Companion.getInstance().dp2px(100);
    //离线转向图标，本地图片名
    String hudResPrefix = "global_image_hud_";
    String iconResName = "sou";
    String night = "_night";
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
    int LaneTypeNULL = 255;
    int LaneTypeNormal = 0;
    int LaneTypeBus = 1;
    int LaneTypeOther = 2;
    int LaneTypeTidal = 3;
    int LaneTypeVariable = 4;

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
        int Maneuver = 1; //转向图标图片数据
        int ManeuverIcon = 2;    //转向图标信息
        int ExitDirection = 3;  //导航过程中传出出入口的编号、方向信息
    }

    interface BizRouteType {
        int BizRouteTypeArrow = 6017;
    }

    interface ChangeNaviPathResult {
        /**
         * 切换成功
         */
        int ChangeNaviPathResultSuccess = 1;
        /**
         * PathID无效切换失败
         */
        int ChangeNaviPathResultErrorPathIDInvalid = 2;
        /**
         * 与主路线PathID切换失败
         */
        int ChangeNaviPathResultErrorSamePathID = 3;
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
            NaviSceneType.SCENE_PARK_LIST, NaviSceneType.Scene_VIA_INFO, NaviSceneType.SCENE_LAST_MILE,
            NaviSceneType.SCENE_PARALLEL, NaviSceneType.SCENE_PREFERENCE, NaviSceneType.SCENE_SPEED,
            NaviSceneType.SCENE_SAPA, NaviSceneType.SCENE_CROSS_IMAGE, NaviSceneType.SCENE_CONTROL})
    @interface NaviSceneType {
        int SCENE_TBT = 0;
        int SCENE_LANES = 1;
        int SCENE_VIA_LIST = 2;
        int SCENE_PARK_LIST = 3;
        int Scene_VIA_INFO = 4;
        int SCENE_LAST_MILE = 5;
        int SCENE_PARALLEL = 6;
        int SCENE_PREFERENCE = 7;
        int SCENE_SPEED = 8;
        int SCENE_SAPA = 9;
        int SCENE_CROSS_IMAGE = 10;
        int SCENE_CONTROL = 11;
    }

    @IntDef({CrossType.CrossTypeGrid, CrossType.CrossTypeVector,
            CrossType.CrossType3D})
    @interface CrossType {
        int CrossTypeGrid = 1;//栅格图, 背景为JPG，箭头为PNG
        int CrossTypeVector = 3;//矢量大图
        int CrossType3D = 4;//三维放大图
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
        int LocSwitchNull = -1;
        int LocSwitchMainToSide = 1;
        int LocSwitchSideToMain = 0;
        int LocSwitchUpBridgeToDownBridge = 3;
        int LocSwitchDownBridgeToUpBridge = 2;
    }

    interface TMCTrafficStatus {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int TrafficStatusUnkonw = 0;
        int TrafficStatusOpen = 1;
        int TrafficStatusSlow = 2;
        int TrafficStatusJam = 3;
        int TrafficStatusCongested = 4;
        int TrafficStatusExtremelyOpen = 5;
    }

    interface VectorCrossImageType {
        int VectorCrossImageTypeInvalid = -1;//非法值
        int VectorCrossImageTypeCommon = 0;//普通矢量路口大图
        int VectorCrossImageTypeRoundabout = 1;//环岛矢量路口大图
        int VectorCrossImageTypeConfusion = 2;//混淆矢量路口大图
        int VectorCrossImageTypeNear = 3;//近接矢量路口大图
        int VectorCrossImageTypeDoubleLight = 4;//跨红绿灯矢量路口大图
        int VectorCrossImageTypeSolidLine = 6;//长实线大图
        int VectorCrossImageTypeSolidNear = 7;//长实线近接大图
        int VectorCrossImageTypeMixReverse = 10;//混淆型逆行大图
    }

    interface GuideParamType {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int GuideParamInvalid = 0;
        int GuideParamCommon = 1;
        int GuideParamTMC = 2;
        int GuideParamTR = 3;
        int GuideParamSAPA = 4;
        int GuideParamLane = 5;
        int GuideParamCruise = 6;
        int GuideParamNavi = 7;
        int GuideParamCamera = 8;
        int GuideParamExit = 9;
        int GuideParamCrossing = 10;
        int GuideParamTTSPlay = 11;
        int GuideParamVehicle = 12;
        int GuideParamNetwork = 13;
        int GuideParamEasy3d = 14;
        int GuideParamEmulator = 15;
        int GuideParamJiliAuto = 16;
        int GuideParamNaviWeather = 17;
    }

    interface TMCViaIndex {
        int Via0 = 0;
        int Via1 = 1;
        int Via2 = 2;
        int Via3 = 3;
        int Via4 = 4;
        int Via5 = 5;
        int Via6 = 6;
        int Via7 = 7;
        int Via8 = 8;
        int Via9 = 9;
        int Via10 = 10;
        int Via11 = 11;
        int Via12 = 12;
        int Via13 = 13;
        int Via14 = 14;
        int Via15 = 15;
        int Via16 = 16;
        int Via17 = 17;
        int Via18 = 18;
        int Via19 = 19;
    }

    //车道线
    interface LaneActionConstants {
        int LaneActionAheadAndNUll = 0;
        int LaneActionAheadAndNUllNoAction = 1;
        int LaneActionLeftAndNUll = 2;
        int LaneActionLeftAndNUllNoAction = 3;
        int LaneActionAheadLeftAndAhead = 4;
        int LaneActionAheadLeftAndLeft = 5;
        int LaneActionAheadLeftNoAction = 6;
        int LaneActionRightAndNUll = 7;
        int LaneActionRightAndNUllNoAction = 8;
        int LaneActionAheadRightAndAhead = 9;
        int LaneActionAheadRightAndRight = 10;
        int LaneActionAheadRightNoAction = 11;
        int LaneActionLUTurnAndNUll = 12;
        int LaneActionLUTurnAndNUllNoAction = 13;
        int LaneActionLeftRightAndLeft = 14;
        int LaneActionLeftRightAndRight = 15;
        int LaneActionLeftRightNoAction = 16;
        int LaneActionAheadLeftRightAndAhead = 17;
        int LaneActionAheadLeftRightAndLeft = 18;
        int LaneActionAheadLeftRightAndRight = 19;
        int LaneActionAheadLeftRightNoAction = 20;
        int LaneActionRUTurnAndNUll = 21;
        int LaneActionRUTurnAndNUllNoAction = 22;
        int LaneActionAheadLUTurnAndAhead = 23;
        int LaneActionAheadLUTurnAndLUTurn = 24;
        int LaneActionAheadLUTurnNoAction = 25;
        int LaneActionAheadRUTurnAndAhead = 26;
        int LaneActionAheadRUTurnAndRUTurn = 27;
        int LaneActionAheadRUTurnNoAction = 28;
        int LaneActionLeftLUTurnAndLeft = 29;
        int LaneActionLeftLUTurnAndLUTurn = 30;
        int LaneActionLeftLUTurnNoAction = 31;
        int LaneActionRightRUTurnAndRight = 32;
        int LaneActionRightRUTurnAndRUTurn = 33;
        int LaneActionRightRUTurnNoAction = 34;
        int LaneActionLeftInAheadAndNUll = 35;
        int LaneActionLeftInAheadAndNUllNoAction = 36;
        int LaneActionAheadLeftLUTurnAndAhead = 37;
        int LaneActionAheadLeftLUTurnAndLeft = 38;
        int LaneActionAheadLeftLUTurnAndLUTrun = 39;
        int LaneActionAheadLeftLUTurnNoAction = 40;
        int LaneActionRightLUTurnAndRight = 41;
        int LaneActionRightLUTurnAndLUTurn = 42;
        int LaneActionRightLUTurnNoAction = 43;
        int LaneActionLeftRightLUTurnAndLeft = 44;
        int LaneActionLeftRightLUTurnAndRight = 45;
        int LaneActionLeftRightLUTurnAndLUTurn = 46;
        int LaneActionLeftRightLUTurnNoAction = 47;
        int LaneActionAheadRightLUTurnAndAhead = 48;
        int LaneActionAheadRightLUTurnAndRight = 49;
        int LaneActionAheadRightLUTurnAndLUTurn = 50;
        int LaneActionAheadRightLUTurnNoAction = 51;
        int LaneActionLeftRUTurnAndLeft = 52;
        int LaneActionLeftRUTurnAndRUTurn = 53;
        int LaneActionLeftRUTurnNoAction = 54;
        int LaneActionBusAndBus = 55;
        int LaneActionBusNoAction = 56;
        int LaneActionVariableAndVariable = 57;
        int LaneActionVariableNoAction = 58;
        int LaneActionDedicated = 149;
        int LaneActionTidal = 150;
        int LaneActionEmptyNoAction = 151;
        int LaneActionEmpty = 152;


        // 分时车道底部公交/专用/潮汐/可变图标
//    private enum TimeLaneBottomAction
        int BackLaneBusWorkable = 1;
        int BackLaneSpecialWorkable = 2;
        int BackLaneTidalWorkable = 3;
        int BackLaneReversibleWorkable = 4;
        int BackLaneBusNoWorkable = 5;
        int BackLaneSpecialNoWorkable = 6;
        int BackLaneTidalNoWorkable = 7;
        int BackLaneReversibleNoWorkable = 8;

        int[][] naviLaneMap = {
                {NaviConstant.LaneAction.LaneActionAhead, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadAndNUllNoAction},// 直行
                {NaviConstant.LaneAction.LaneActionAhead, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadAndNUll},

                {NaviConstant.LaneAction.LaneActionLeft, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftAndNUllNoAction},// 左转
                {NaviConstant.LaneAction.LaneActionLeft, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionLeftAndNUll},

                {NaviConstant.LaneAction.LaneActionRight, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionRightAndNUllNoAction},// 右转
                {NaviConstant.LaneAction.LaneActionRight, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionRightAndNUll},

                {NaviConstant.LaneAction.LaneActionLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionLUTurnAndNUll},// 左U-Turn
                {NaviConstant.LaneAction.LaneActionLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLUTurnAndNUllNoAction},

                {NaviConstant.LaneAction.LaneActionRUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionRUTurnAndNUllNoAction},// 右U-Turn
                {NaviConstant.LaneAction.LaneActionRUTurn, NaviConstant.LaneAction.LaneActionRUTurn, LaneActionConstants.LaneActionRUTurnAndNUll},

                {NaviConstant.LaneAction.LaneActionLeftInAhead, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftInAheadAndNUllNoAction},// 左侧扩展车道 直行
                {NaviConstant.LaneAction.LaneActionLeftInAhead, NaviConstant.LaneAction.LaneActionLeftInAhead, LaneActionConstants.LaneActionLeftInAheadAndNUll},

                {NaviConstant.LaneAction.LaneActionAheadLeft, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadLeftNoAction},// 左转+直行
                {NaviConstant.LaneAction.LaneActionAheadLeft, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadLeftAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadLeft, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionAheadLeftAndLeft},

                {NaviConstant.LaneAction.LaneActionAheadRight, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadRightNoAction},// 右转+直行
                {NaviConstant.LaneAction.LaneActionAheadRight, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadRightAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadRight, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionAheadRightAndRight},

                {NaviConstant.LaneAction.LaneActionLeftRight, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftRightNoAction},// 左转+右转
                {NaviConstant.LaneAction.LaneActionLeftRight, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionLeftRightAndLeft},
                {NaviConstant.LaneAction.LaneActionLeftRight, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionLeftRightAndRight},

                {NaviConstant.LaneAction.LaneActionAheadLeftRight, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadLeftRightNoAction},// 左转+直行+右转
                {NaviConstant.LaneAction.LaneActionAheadLeftRight, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadLeftRightAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadLeftRight, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionAheadLeftRightAndLeft},
                {NaviConstant.LaneAction.LaneActionAheadLeftRight, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionAheadLeftRightAndRight},

                {NaviConstant.LaneAction.LaneActionAheadLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadLUTurnNoAction},// 左U-Turn + 直行
                {NaviConstant.LaneAction.LaneActionAheadLUTurn, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadLUTurnAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionAheadLUTurnAndLUTurn},

                {NaviConstant.LaneAction.LaneActionAheadRUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadRUTurnNoAction},// 右U-Turn + 直行
                {NaviConstant.LaneAction.LaneActionAheadRUTurn, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadRUTurnAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadRUTurn, NaviConstant.LaneAction.LaneActionRUTurn, LaneActionConstants.LaneActionAheadRUTurnAndRUTurn},

                {NaviConstant.LaneAction.LaneActionLeftLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftLUTurnNoAction},// 左U-Turn + 左转
                {NaviConstant.LaneAction.LaneActionLeftLUTurn, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionLeftLUTurnAndLeft},
                {NaviConstant.LaneAction.LaneActionLeftLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionLeftLUTurnAndLUTurn},

                {NaviConstant.LaneAction.LaneActionRightRUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionRightRUTurnNoAction},// 右U-Turn + 右转
                {NaviConstant.LaneAction.LaneActionRightRUTurn, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionRightRUTurnAndRight},
                {NaviConstant.LaneAction.LaneActionRightRUTurn, NaviConstant.LaneAction.LaneActionRUTurn, LaneActionConstants.LaneActionRightRUTurnAndRUTurn},

                {NaviConstant.LaneAction.LaneActionAheadLeftLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadLeftLUTurnNoAction},// 左U-Turn + 左转 + 直行
                {NaviConstant.LaneAction.LaneActionAheadLeftLUTurn, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadLeftLUTurnAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadLeftLUTurn, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionAheadLeftLUTurnAndLeft},
                {NaviConstant.LaneAction.LaneActionAheadLeftLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionAheadLeftLUTurnAndLUTrun},

                {NaviConstant.LaneAction.LaneActionRightLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionRightLUTurnNoAction},// 左U-Turn + 右转
                {NaviConstant.LaneAction.LaneActionRightLUTurn, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionRightLUTurnAndRight},
                {NaviConstant.LaneAction.LaneActionRightLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionRightLUTurnAndLUTurn},

                {NaviConstant.LaneAction.LaneActionLeftRightLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftRightLUTurnNoAction},// 左U-Turn + 左转 + 右转
                {NaviConstant.LaneAction.LaneActionLeftRightLUTurn, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionLeftRightLUTurnAndLeft},
                {NaviConstant.LaneAction.LaneActionLeftRightLUTurn, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionLeftRightLUTurnAndRight},
                {NaviConstant.LaneAction.LaneActionLeftRightLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionLeftRightLUTurnAndLUTurn},

                {NaviConstant.LaneAction.LaneActionAheadRightLUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionAheadRightLUTurnNoAction},// 左U-Turn + 直行 + 右转
                {NaviConstant.LaneAction.LaneActionAheadRightLUTurn, NaviConstant.LaneAction.LaneActionAhead, LaneActionConstants.LaneActionAheadRightLUTurnAndAhead},
                {NaviConstant.LaneAction.LaneActionAheadRightLUTurn, NaviConstant.LaneAction.LaneActionRight, LaneActionConstants.LaneActionAheadRightLUTurnAndRight},
                {NaviConstant.LaneAction.LaneActionAheadRightLUTurn, NaviConstant.LaneAction.LaneActionLUTurn, LaneActionConstants.LaneActionAheadRightLUTurnAndLUTurn},

                {NaviConstant.LaneAction.LaneActionLeftRUTurn, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionLeftRUTurnNoAction},// 左转 + 右U-Turn
                {NaviConstant.LaneAction.LaneActionLeftRUTurn, NaviConstant.LaneAction.LaneActionLeft, LaneActionConstants.LaneActionLeftRUTurnAndLeft},
                {NaviConstant.LaneAction.LaneActionLeftRUTurn, NaviConstant.LaneAction.LaneActionRUTurn, LaneActionConstants.LaneActionLeftRUTurnAndRUTurn},

                {NaviConstant.LaneAction.LaneActionBus, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionBusNoAction},// 公交车道
                {NaviConstant.LaneAction.LaneActionBus, NaviConstant.LaneAction.LaneActionBus, LaneActionConstants.LaneActionBusAndBus},

                {NaviConstant.LaneAction.LaneActionVariable, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionVariableNoAction},// 可变车道
                {NaviConstant.LaneAction.LaneActionVariable, NaviConstant.LaneAction.LaneActionVariable, LaneActionConstants.LaneActionVariableAndVariable},

                {NaviConstant.LaneAction.LaneActionDedicated, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionDedicated},// 专用车道
                {NaviConstant.LaneAction.LaneActionDedicated, NaviConstant.LaneAction.LaneActionDedicated, LaneActionConstants.LaneActionDedicated},

                {NaviConstant.LaneAction.LaneActionTidal, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionTidal},// 潮汐车道
                {NaviConstant.LaneAction.LaneActionTidal, NaviConstant.LaneAction.LaneActionTidal, LaneActionConstants.LaneActionTidal},

                {NaviConstant.LaneAction.LaneActionEmpty, NaviConstant.LaneAction.LaneActionNULL, LaneActionConstants.LaneActionEmptyNoAction}, // 空车道
                {NaviConstant.LaneAction.LaneActionEmpty, NaviConstant.LaneAction.LaneActionEmpty, LaneActionConstants.LaneActionEmpty},// 可行空车道
        };
    }

    interface LaneAction {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int LaneActionNULL = 255;
        int LaneActionAhead = 0;
        int LaneActionLeft = 1;
        int LaneActionAheadLeft = 2;
        int LaneActionRight = 3;
        int LaneActionAheadRight = 4;
        int LaneActionLUTurn = 5;
        int LaneActionLeftRight = 6;
        int LaneActionAheadLeftRight = 7;
        int LaneActionRUTurn = 8;
        int LaneActionAheadLUTurn = 9;
        int LaneActionAheadRUTurn = 10;
        int LaneActionLeftLUTurn = 11;
        int LaneActionRightRUTurn = 12;
        int LaneActionLeftInAhead = 13;
        int LaneActionLeftLUturn = 14;
        int LaneActionReserved = 15;
        int LaneActionAheadLeftLUTurn = 16;
        int LaneActionRightLUTurn = 17;
        int LaneActionLeftRightLUTurn = 18;
        int LaneActionAheadRightLUTurn = 19;
        int LaneActionLeftRUTurn = 20;
        int LaneActionBus = 21;
        int LaneActionEmpty = 22;
        int LaneActionVariable = 23;
        int LaneActionDedicated = 24;
        int LaneActionTidal = 25;
    }
}
