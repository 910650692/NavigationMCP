package com.fy.navi.mapservice.bean;

public interface INaviConstant {

    String NAVI_COMMON_TAG = "NaviAutoApi";
    String PAGE_EXTRA = "targetPage";
    String SEARCH_KEYWORD_EXTRA = "keyword";
    String POI_INFO_EXTRA = "poi_info_entity";
    String ROUTE_END_POI = "route_end_point";

    //底图类型
    interface MapType {
        int UNKNOWN = 0;
        int MAIN = 1; //主图
        int LAUNCHER_DESK = 2; //桌面底图
        int LAUNCHER_WIDGET = 3; //桌面卡片
        int HUD_WIDGET = 4; //HUD
        int CLUSTER_WIDGET = 5; //HUD
    }

    //用于定义BaseRouteLine里的type.
    interface PathType {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int NULL = 0;
        int DRIVE = 1;
        int RIDE = 2;
        int WALK = 3;
        int CUSTOM = 255;
    }

    //用于定义BaseTurnInfo里的type.
    interface NaviType {
        int UNKNOWN_ERROR = Integer.MIN_VALUE;
        int GPS = 0;
        int SIMULATION = 1;
        int CRUISE = 2;
        int HEALTH_RUN = 3;
        int HEALTH_RIDE = 4;
        int HEALTH_SHARE_BIKE = 5;
    }

    //用于定义BaseTurnInfo里的curRoadClass.
    interface RoadClass {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int NULL = -1;
        int FREE_WAY = 0; //高速公路
        int NATIONAL_ROAD = 1; //国道
        int PROVINCE_ROAD = 2; //省道
        int COUNTY_ROAD = 3; //县道
        int RURAL_ROAD = 4; //乡道
        int IN_COUNTY_ROAD = 5; //县乡村内部道路
        int CITY_SPEED_WAY = 6; //城市快速路
        int MAIN_ROAD = 7; //主要道路
        int SECONDARY_ROAD = 8; //次要道路
        int COMMON_ROAD = 9; //普通道路
        int NON_NAVI_ROAD = 10; //非导航道路
    }

    interface ManeuverId {
        int TURN_LEFT = 2;
        int TURN_RIGHT = 3;
        //轻微左转图标
        int SLIGHT_LEFT = 4;
        //轻微右转图标
        int SLIGHT_RIGHT = 5;
        int TURN_HARD_LEFT = 6;
        int TURN_HARD_RIGHT = 7;
        int U_TURN = 8;
        //继续执行图标
        int CONTINUE = 9;
        int WAY = 10;
        int ENTRY_RING = 11;
        int LEAVE_RING = 12;
        int SAPA = 13;
        int TOLLGATE = 14;
        int DESTINATION = 15;
        int TUNNEL = 16;
        int ENTRY_LEFT_RING = 17;
        int LEAVE_LEFT_RING = 18;
        int U_TURN_RIGHT  = 19;
        int SPECIAL_CONTINUE = 20;
        //进入环岛后左转图标
        int ENTRY_RING_LEFT = 21;
        //进入环岛后右转图标
        int ENTRY_RING_RIGHT = 22;
        //进入环岛后继续直行图标
        int ENTRY_RING_CONTINUE = 23;
        //进入环岛后掉头图标
        int ENTRY_RING_U_TURN = 24;
        int ENTRY_LEFT_RING_LEFT= 25;
        int ENTRY_LEFT_RING_RIGHT = 26;
        //进入左侧环岛后继续直行图标
        int ENTRY_LEFT_RING_CONTINUE = 27;
        //进入左侧环岛后掉头图标
        int ENTRY_LEFT_RING_UTURN = 28;
        int ENTRY_RING_1 = 68;
        int ENTRY_RING_2 = 69;
        int ENTRY_RING_3 = 70;
        int ENTRY_RING_4 = 71;
        int ENTRY_RING_5 = 72;
        int ENTRY_RING_6 = 73;
        int ENTRY_RING_7 = 74;
        int ENTRY_RING_8 = 75;
        int ENTRY_RING_9 = 76;
        int ENTRY_RING_10 = 77;
        int LEAVE_RING_1 = 79;
        int LEAVE_RING_2 = 80;
        int LEAVE_RING_3 = 81;
        int LEAVE_RING_4 = 82;
        int LEAVE_RING_5 = 83;
        int LEAVE_RING_6 = 84;
        int LEAVE_RING_7 = 85;
        int LEAVE_RING_8 = 86;
        int LEAVE_RING_9 = 87;
        int LEAVE_RING_10 = 88;
        int MERGE_LEFT = 65;
        int MERGE_RIGHT = 66;
    }

    //地图状态
    interface NaviStatusType {
        String NO_STATUS = "NO_STATUS";
        String CRUISE = "CRUISE";
        String ROUTING = "ROUTING";
        String SELECT_ROUTE = "SELECT_ROUTE";
        String NAVING = "NAVING";
        String LIGHT_NAVING = "LIGHT_NAVING";
    }

    //打开地图意图
    interface OpenIntentPage {
        int NONE = -1;
        int SEARCH_PAGE = 1; //搜索界面
        int ROUTE_PAGE = 2; //路线规划界面
        int POI_DETAIL_PAGE = 3; // poi详情页面
        int GO_HOME = 4;
        int GO_COMPANY = 5;
        int START_NAVIGATION = 6;
    }

    interface GuidePanelStatus {
        int NOT_IN_NAVIGATION = 0; //不在导航态
        int COMMON_NAVIGATION = 1; //正常导航，普通面板
        int IN_YAW = 3; //偏航状态
        int UN_NAME_ROAD = 6; //无数据道路
    }

}
