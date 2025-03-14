package com.fy.navi.mapservice.bean;

public interface INaviConstant {

    String PAGE_EXTRA = "targetPage";
    String SEARCH_KEYWORD_EXTRA = "keyword";
    String POI_INFO_EXTRA = "poi_info_entity";
    String ROUTE_END_POI = "route_end_point";

    //底图类型
    interface MapType {
        int UNKNOWN = 0;
        int Main = 1; //主图
        int LauncherDesk = 2; //桌面底图
        int LauncherWidget = 3; //桌面卡片
    }

    //用于定义BaseRouteLine里的type.
    interface PathType {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int PathTypeNULL = 0;
        int PathTypeDrive = 1;
        int PathTypeRide = 2;
        int PathTypeWalk = 3;
        int PathTypeCustom = 255;
    }

    //用于定义BaseTurnInfo里的type.
    interface NaviType {
        int UNKNOWN_ERROR = Integer.MIN_VALUE;
        int NaviTypeGPS = 0;
        int NaviTypeSimulation = 1;
        int NaviTypeCruise = 2;
        int NaviTypeHealthRun = 3;
        int NaviTypeHealthRide = 4;
        int NaviTypeHealthShareBike = 5;
    }

    //用于定义BaseTurnInfo里的curRoadClass.
    interface RoadClass {
        int AUTO_UNKNOWN_ERROR = Integer.MIN_VALUE;
        int RoadClassNULL = -1;
        int RoadClassFreeway = 0;
        int RoadClassNationalRoad = 1;
        int RoadClassProvinceRoad = 2;
        int RoadClassCountyRoad = 3;
        int RoadClassRuralRoad = 4;
        int RoadClassInCountyRoad = 5;
        int RoadClassCitySpeedway = 6;
        int RoadClassMainRoad = 7;
        int RoadClassSecondaryRoad = 8;
        int RoadClassCommonRoad = 9;
        int RoadClassNonNaviRoad = 10;
        int RoadClassCount = 11;
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
