package com.fy.navi.fsa;

public interface FsaConstant {
    String FSA_TAG = "FsaService";

    //显示
    int SHOW = 0;
    //修改
    int UPDATE = 1;
    //隐藏
    int HIDE = 2;

    int NO_CHANGE = 0;
    int INCREASE = 1;
    int REDUCE = 2;

    /**
     * 在FSA协议中，分客户端和服务端的概念（
     * 端口信息：FSA_SERVICE_PORT = 9903；
     * FSA_SERVICE_ID = 3003；
     * FSA_SERVICE_IP = "172.16.4.100"），这里统一明确，
     */
    interface InetConfig {
        int SERVICE_PORT = 9903;
        int SERVICE_ID = 3003;
        String SERVICE_IP = "172.16.4.100";
    }

    interface FsaMethod {
        int ID_REQUEST_MSG = 9201;
    }

    interface FsaEventPayload {
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

    interface FsaFunction {
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

    interface FsaValue {
        String STRING_ZERO = "0";
        String STRING_ONE = "1";
        String TRUE = "ture";
        String FALSE = "false";
        int ZERO = 0;
        int ONE = 1;
        int TWO = 2;
    }

    //turnKind转向类型
    interface FsaTurnKind {
        int ICON_CONTINUE = 9; //直行对应的iconId
    }


    //Camera type
    interface FsaCameraType {
        int INTERVAL_VELOCITY_START = 25; //区间测速起点
        int INTERVAL_VELOCITY_END = 26; //区间测速终点
        int INTERVAL_VELOCITY_START_END = 27; //区间测速起终点
    }

    interface FsaLaneType {
        int INVALID_VALUE = 0; // 无效值
        int NORMAL = 1; // 普通
        int BUS_LANE = 2; // 公交车道（可能允许社会车辆通行）
        int TEXT_BUS_LANE = 3; // 文字公交车道（只允许公交车通行）
        int VARIABLE_LANE = 4; // 可变车道
        int MULTI_MEMBER_LANE = 5; // 多成员车道
        int TEXT_TIDAL_LANE = 6; // 文字潮汐车道
        int FORWARD_ARROWS_CHANGE_LANE = 7; // 前行箭头可变车道
        int CROSS_MARKED_VARIABLE_LANE = 8; // 叉号可变车道
    }

    interface FsaLaneDirection {
        int INVALID_VALUE = 0;
        int GO_STRAIGHT = 1;
        int TURN_LEFT = 2;
        int TURN_RIGHT = 3;
        int TURN_AROUND = 4;
    }


}
