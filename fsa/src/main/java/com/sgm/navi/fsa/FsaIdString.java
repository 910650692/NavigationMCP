package com.sgm.navi.fsa;

import java.util.HashMap;
import java.util.Map;

public final class FsaIdString {

    private FsaIdString() {
    }

    public final static Map<String, String> FSA_EVENT_PAYLOAD_MAP = new HashMap<>();
    public final static Map<Integer, String> FSA_FUNCTION_PAYLOAD_MAP = new HashMap<>();

    /**
     * eventid转功能描述
     * @param code eventid
     * @return 功能描述
     */
    public static String event2String(final String code) {
        if (FSA_EVENT_PAYLOAD_MAP.isEmpty()) {
            FSA_EVENT_PAYLOAD_MAP.put("0", "是否有有效路网数据"); // ROAD_NETWORK_MODE
            FSA_EVENT_PAYLOAD_MAP.put("1", "获取TBT信息"); // TBT_INFO
            FSA_EVENT_PAYLOAD_MAP.put("2", "导航态区间测速信息"); // INTERVAL_SPEED_INFO
            FSA_EVENT_PAYLOAD_MAP.put("3", "巡航态前方限速信息"); // CRUISE_SPEED_LIMIT
            FSA_EVENT_PAYLOAD_MAP.put("4", "车道线信息"); // LANE_INFO
            FSA_EVENT_PAYLOAD_MAP.put("5", "导航剩余时间和距离"); // REMAIN_TIME_DISTANCE
            FSA_EVENT_PAYLOAD_MAP.put("6", "导航目的地名称和坐标"); // DESTINATION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("7", "当前道路名称"); // CURRENT_ROAD
            FSA_EVENT_PAYLOAD_MAP.put("8", "高速服务区信息"); // SERVICE_AREA
            FSA_EVENT_PAYLOAD_MAP.put("9", "获取隧道信息"); // TUNNEL_INFO
            FSA_EVENT_PAYLOAD_MAP.put("10", "获取放大图信息"); // ENLARGE_ICON
            FSA_EVENT_PAYLOAD_MAP.put("11", "获取续航里程信息"); // RANGE_ON_ROUTE
            FSA_EVENT_PAYLOAD_MAP.put("12", "导航态前方限速摄像头信息"); // FORWARD_CAMERA
            FSA_EVENT_PAYLOAD_MAP.put("13", "路况拥堵信息"); // CONGESTION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("14", "当前路况显示模式"); // TRAFFIC_MAP_MODE
            FSA_EVENT_PAYLOAD_MAP.put("15", "当前路况数据"); // ROAD_CONDITION_INFO
            FSA_EVENT_PAYLOAD_MAP.put("16", "获取当前车标位置(已行驶里程占路线总长度百分比)"); // PASSED_PERCENT
            FSA_EVENT_PAYLOAD_MAP.put("17", "是否处于专业导航状态"); // IN_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("18", "是否处于轻导航状态"); // IN_LIGHT_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("19", "是否处于巡航状态"); // IN_CRUISE
            FSA_EVENT_PAYLOAD_MAP.put("20", "是否处于NOP导航"); // IN_NOP_NAVIGATION
            FSA_EVENT_PAYLOAD_MAP.put("21", "导航状态下诱导面板状态信息"); // PANEL_STATUS
            FSA_EVENT_PAYLOAD_MAP.put("22", "获取放大图信息(HUD专用)"); // HUD_ENLARGE_MAP
            FSA_EVENT_PAYLOAD_MAP.put("23", "开启仪表多屏地图渲染"); // OPEN_HUD_MAP
            FSA_EVENT_PAYLOAD_MAP.put("24", "关闭仪表多屏地图渲染"); // CLOSE_HUD_MAP
            FSA_EVENT_PAYLOAD_MAP.put("25", "开启HUD视频流服务"); // OPEN_HUD_VIDEO
            FSA_EVENT_PAYLOAD_MAP.put("26", "关闭HUD视频流服务"); // CLOSE_HUD_VIDEO
            FSA_EVENT_PAYLOAD_MAP.put("27", "开启扶手屏RSTP"); // OPEN_RSTP
            FSA_EVENT_PAYLOAD_MAP.put("28", "关闭扶手屏RSTP"); // CLOSE_RSTP
            FSA_EVENT_PAYLOAD_MAP.put("30", "HUD视频服务初始化"); // HUD_SERVICE_INIT
            FSA_EVENT_PAYLOAD_MAP.put("29", "HUD视频服务反初始化"); // HUD_SERVICE_UNINIT
            FSA_EVENT_PAYLOAD_MAP.put("31", "日夜模式切换"); // THEME_CHANGED
            FSA_EVENT_PAYLOAD_MAP.put("32", "当前地图状态"); // NAVIGATION_STATUS
            FSA_EVENT_PAYLOAD_MAP.put("33", "获取整体限速状态"); // WHOLE_SPEED_LIMIT
            FSA_EVENT_PAYLOAD_MAP.put("34", "获取NOP导航下个路段和下下个路段"); // NOP_NEXT_ROAD
        }
        return FSA_EVENT_PAYLOAD_MAP.get(code);
    }

    /**
     * functionid转功能描述
     * @param code functionid
     * @return 功能描述
     */
    public static String function2String(final int code) {
        if (FSA_FUNCTION_PAYLOAD_MAP.isEmpty()) {
            FSA_FUNCTION_PAYLOAD_MAP.put(9250, "是否有有效路网数据");
            FSA_FUNCTION_PAYLOAD_MAP.put(9251, "获取TBT信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9252, "导航态区间测速信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9253, "巡航态前方限速信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9254, "车道线信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9255, "导航剩余时间和距离");
            FSA_FUNCTION_PAYLOAD_MAP.put(9256, "导航目的地名称和坐标");
            FSA_FUNCTION_PAYLOAD_MAP.put(9257, "当前道路名称");
            FSA_FUNCTION_PAYLOAD_MAP.put(9258, "高速服务区信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9259, "获取隧道信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9260, "获取放大图信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9261, "获取续航里程信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9262, "导航态前方限速摄像头信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9263, "路况拥堵信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9264, "当前路况显示模式");
            FSA_FUNCTION_PAYLOAD_MAP.put(9265, "当前路况数据");
            FSA_FUNCTION_PAYLOAD_MAP.put(9266, "获取当前车标位置(已行驶里程占路线总长度百分比)");
            FSA_FUNCTION_PAYLOAD_MAP.put(9267, "是否处于专业导航状态");
            FSA_FUNCTION_PAYLOAD_MAP.put(9268, "是否处于轻导航状态");
            FSA_FUNCTION_PAYLOAD_MAP.put(9269, "是否处于巡航状态");
            FSA_FUNCTION_PAYLOAD_MAP.put(9270, "是否处于NOP导航");
            FSA_FUNCTION_PAYLOAD_MAP.put(9271, "导航状态下诱导面板状态信息");
            FSA_FUNCTION_PAYLOAD_MAP.put(9272, "获取放大图信息(HUD专用)");
            FSA_FUNCTION_PAYLOAD_MAP.put(9273, "响应三指飞屏及仪表发送的信号");
            FSA_FUNCTION_PAYLOAD_MAP.put(9274, "HUD视频服务初始化");
            FSA_FUNCTION_PAYLOAD_MAP.put(9276, "日夜模式切换");
            FSA_FUNCTION_PAYLOAD_MAP.put(9277, "当前地图状态");
            FSA_FUNCTION_PAYLOAD_MAP.put(9278, "获取整体限速状态");
            FSA_FUNCTION_PAYLOAD_MAP.put(9279, "获取NOP导航下个路段和下下个路段");
            FSA_FUNCTION_PAYLOAD_MAP.put(9280, "导航中目的地变更信息（名称、经纬度等）");
            FSA_FUNCTION_PAYLOAD_MAP.put(9281, "导航中自车位置");
            FSA_FUNCTION_PAYLOAD_MAP.put(9282, "低电量周边充电站POI透出（最近5个）");
            FSA_FUNCTION_PAYLOAD_MAP.put(9283, "端导停功周边停车场POI透出（最近5个）");
            FSA_FUNCTION_PAYLOAD_MAP.put(9284, "高速服务区POI透出（最近5个）");
        }
        return FSA_FUNCTION_PAYLOAD_MAP.get(code);
    }
}
