package com.fy.navi.fsa;

public interface FsaConstant {

    public static final String FSA_TAG = "FsaService";


    interface INET_CONFIG {
        int SERVICE_PORT = 9903;
        int SERVICE_ID = 3003;
        String SERVICE_IP = "172.16.4.100";
    }

    interface FSA_METHOD {
        int ID_REQUEST_MSG = 9201;
    }

    interface FSA_EVENT_PAYLOAD {
        //是否有有效路网数据
        String ROAD_NETWORK_MODE = "0";
        //获取TBT信息
        String TBT_INFO = "1";
        //导航态区间测速信息
        String INTERVAL_SPEED_INFO = "2";
        //巡航态前方限速信息
        String CRUISE_SPEED_LIMIT = "3";
        //车道线信息
        String LANE_INFO = "4";
        //导航剩余时间和距离
        String REMAIN_TIME_DISTANCE = "5";
        //导航目的地名称和坐标
        String DESTINATION_INFO = "6";
        //当前道路名称
        String CURRENT_ROAD = "7";
        //高速服务区信息
        String SERVICE_AREA = "8";
        //获取隧道信息
        String TUNNEL_INFO = "9";
        //获取放大图信息
        String ENLARGE_ICON = "10";
        //获取续航里程信息
        String RANGE_ON_ROUTE = "11";
        //导航态前方限速摄像头信息
        String FORWARD_CAMERA = "12";
        //路况拥堵信息
        String CONGESTION_INFO = "13";
        //当前路况显示模式
        String TRAFFIC_MAP_MODE = "14";
        //当前路况数据
        String ROAD_CONDITION_INFO = "15";
        //获取当前车标位置(已行驶里程占路线总长度百分比)
        String PASSED_PERCENT = "16";
        //是否处于专业导航状态
        String IN_NAVIGATION = "17";
        //是否处于轻导航状态
        String IN_LIGHT_NAVIGATION = "18";
        //是否处于巡航状态
        String IN_CRUISE = "19";
        //是否处于NOP导航
        String IN_NOP_NAVIGATION = "20";
        //导航状态下诱导面板状态信息
        String PANEL_STATUS = "21";
        //获取放大图信息(HUD专用)
        String HUD_ENLARGE_MAP = "22";
        //开启仪表多屏地图渲染(无返回值，不需要functionId)
        String OPEN_HUD_MAP = "23";
        //关闭仪表多屏地图渲染(无返回值，不需要functionId)
        String CLOSE_HUD_MAP = "24";
        //开启HUD视频流服务(无返回值，不需要functionId)
        String OPEN_HUD_VIDEO = "25";
        //关闭HUD视频流服务(无返回值，不需要functionId)
        String CLOSE_HUD_VIDEO = "26";
        //开启扶手屏RSTP(无返回值，不需要functionId)
        String OPEN_RSTP = "27";
        //关闭扶手屏RSTP(无返回值，不需要functionId)
        String CLOSE_RSTP = "28";
        //HUD视频服务初始化
        String HUD_SERVICE_INIT = "30";
        //HUD视频服务反初始化(无返回值，不需要functionId)
        String HUD_SERVICE_UNINIT = "29";
        //日夜模式切换
        String THEME_CHANGED = "31";
        //当前地图状态
        String NAVIGATION_STATUS = "32";
        //获取整体限速状态
        String WHOLE_SPEED_LIMIT = "33";
        //获取NOP导航下个路段和下下个路段
        String NOP_NEXT_ROAD = "34";
    }

    interface FSA_FUNCTION {
        //是否有有效路网数据
        int ID_ROAD_NETWORK_MODE = 9250;
        //获取TBT信息
        int ID_TBT_INFO = 9251;
        //导航态区间测速信息
        int ID_INTERVAL_SPEED_INFO = 9252;
        //巡航态前方限速信息
        int ID_CRUISE_SPEED_LIMIT = 9253;
        //车道线信息
        int ID_LANE_INFO = 9254;
        //导航剩余时间和距离
        int ID_REMAIN_TIME_DISTANCE = 9255;
        //导航目的地名称和坐标
        int ID_DESTINATION_INFO = 9256;
        //当前道路名称
        int ID_CURRENT_ROAD = 9257;
        //高速服务区信息
        int ID_SERVICE_AREA = 9258;
        //获取隧道信息
        int ID_TUNNEL_INFO = 92599;
        //获取放大图信息
        int ID_ENLARGE_ICON = 9260;
        //获取续航里程信息
        int ID_RANGE_ON_ROUTE = 9261;
        //导航态前方限速摄像头信息
        int ID_FORWARD_CAMERA = 9262;
        //路况拥堵信息
        int ID_CONGESTION_INFO = 9263;
        //当前路况显示模式
        int ID_TRAFFIC_MAP_MODE = 9264;
        //当前路况数据
        int ID_ROAD_CONDITION_INFO = 9265;
        //获取当前车标位置(已行驶里程占路线总长度百分比)
        int ID_PASSED_PERCENT = 9266;
        //是否处于专业导航状态
        int ID_IN_NAVIGATION = 9267;
        //是否处于轻导航状态
        int ID_IN_LIGHT_NAVIGATION = 9268;
        //是否处于巡航状态
        int ID_IN_CRUISE = 9269;
        //是否处于NOP导航
        int ID_IN_NOP_NAVIGATION = 9270;
        //导航状态下诱导面板状态信息
        int ID_PANEL_STATUS = 9271;
        //获取放大图信息(HUD专用)
        int ID_HUD_ENLARGE_MAP = 9272;
        //响应三指飞屏及仪表发送的信号
        int ID_FINGER_FLYING_HUD = 9273;
        //HUD视频服务初始化
        int ID_HUD_SERVICE_INIT = 9274;
        //日夜模式切换
        int ID_THEME_CHANGED = 9276;
        //当前地图状态
        int ID_NAVIGATION_STATUS = 9277;
        //获取整体限速状态
        int ID_WHOLE_SPEED_LIMIT = 9278;
        //获取NOP导航下个路段和下下个路段
        int ID_NOP_NEXT_ROAD = 9279;
        //导航中目的地变更信息（名称、经纬度等）
        int ID_CHANGE_DESTINATION = 9280;
        //导航中自车位置
        int ID_SELF_DRIVING_POSITION = 9281;
        //低电量周边充电站POI透出（最近5个）
        int ID_CHARGING_STATIONS_POI = 9282;
        //端导停功周边停车场POI透出（最近5个）
        int ID_PARKING_LOT_POI = 9283;
        //高速服务区POI透出（最近5个）
        int ID_SERVICE_POI = 9284;
    }

    interface FSA_VALUE {
        String STRING_ZERO = "0";
        String STRING_ONE = "1";
        String TRUE = "ture";
        String FALSE = "false";
        int ZERO = 0;
        int ONE = 1;
        int TWO = 2;
    }

    //定位类型
    interface FSA_LOCATION_TYPE {
        int INVALID = -1; //无效值
        int GPS = 0; //卫星定位
        int WIFI = 1; //基于WIFI网络定位
        int CELL = 2; //基于基站的定位
        int BLUETOOTH = 3; //基于蓝牙的定位
        int MAGNETIC_FIELD = 4; //基于地磁的定位
        int UNKNOWN = 5; //未知
    }

    //定位来源
    interface FSA_LOCATION_SOURCE {
        int INVALID = 0;
        int GPS = 1;
        int BDS = 2;
        int GLONASS = 4;
        int GALILEO = 8;
        int DR = 16;
        int WIFI_CORRECTED = 32;
        int BASE_STATION_CORRECTED = 64;
    }

    //道路等级
    interface FSA_ROAD_LEVEL {
        int UNKNOWN_ERROR = Integer.MIN_VALUE; //非法操作错误
        int RoadClassNULL = -1; //无效值
        int RoadClassFreeway = 0; //高速公路
        int RoadClassNationalRoad = 1;//国道
        int RoadClassProvinceRoad = 2;//省道
        int RoadClassCountyRoad = 3;//县道
        int RoadClassRuralRoad = 4;//乡公路
        int RoadClassInCountyRoad = 5;//县乡村内部道路
        int RoadClassCitySpeedway = 6;//主要大街、城市快速道
        int RoadClassMainRoad = 7;//主要道路
        int RoadClassSecondaryRoad = 8;//次要道路
        int RoadClassCommonRoad = 9;//普通道路
        int RoadClassNonNaviRoad = 10;//非导航道路
    }

    //turnKind转向类型
    interface FSA_TURN_KIND {
        int IconContinue = 9; //直行对应的iconId
    }


    //Camera type
    interface FSA_CAMERA_TYPE {
        int NULL = 0;
        int IllegalUseLight = 1; //违规用灯
        int IllegalUseSafetyBelt = 2; //不系安全带
        int DoNotFollowLane = 3; //违规占车道
        int IllegalPassCross = 4; //违规过路口
        int DialPhoneWhenDriving = 5; //开车用手机
        int LaneLimitSpeed = 6; //分车道限速
        int UltrahighSpeed = 7; //超高速
        int VeryLowSpeed = 8; //超低速
        int VariableSpeed = 9; //可变限速
        int TrafficLight = 10; //闯红灯
        int EndNumberLimit = 11; //尾号限行
        int EnvironmentalLimit = 12; //环保限行
        int BreachProhibitionSign = 13; //违反禁令标志
        int ViolateProhibitedMarkings = 14; //违反禁止标线
        int CourtesyCrossing = 15; //礼让行人
        int ReverseDriving = 16; //逆向行驶
        int IllegalParking = 17; //违章停车
        int BicycleLane = 18; //占用非机动车道
        int BusWay = 19; //公交专用车道
        int EmergencyLane = 20; //占用应急车道
        int Honk = 21; //禁止鸣笛
        int FlowSpeed = 22; //流动测速
        int CourtesyCarCrossing = 23; //路口不让行
        int RailwayCrossing = 24; //违规过铁路
        int IntervalVelocityStart = 25; //区间测速起点
        int IntervalVelocityEnd = 26; //区间测速终点
        int IntervalVelocityStartEnd = 27; //区间测速起终点
        int CarSpacing = 28; //车间距抓拍
        int HOVLane = 29; //HOV车道
        int OccupiedLine = 30; //压线抓拍
        int ETC = 99; //ETC拍照
        int BreakRule = 100; //无细类违章
        int Surveillance = 101; //视频监控
        int Consequent = 255; //后备
    }

    interface FSA_LANE_TYPE {
        int INVALID_VALUE = 0;
        int NORMAL = 1;
        int BUS_LANE = 2;
        int TEXT_BUS_LANE = 3;
        int VARIABLE_LANE = 4;
        int MULTI_MEMBER_LANE = 5;
        int TEXT_TIDAL_LANE = 6;
        int FORWARD_ARROWS_CHANGE_LANE = 7;
        int CROSS_MARKED_VARIABLE_LANE = 8;
    }

    interface FSA_LANE_DIRECTION {
        int INVALID_VALUE = 0;
        int GO_STRAIGHT = 1;
        int TURN_LEFT = 2;
        int TURN_RIGHT = 3;
        int TURN_AROUND = 4;
    }


}
