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
