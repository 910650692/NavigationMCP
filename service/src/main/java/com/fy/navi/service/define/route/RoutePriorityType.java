package com.fy.navi.service.define.route;

public interface RoutePriorityType {
    //1 普通算路
    public static final int ROUTE_TYPE_COMMON = 1;
    //2 偏航重算
    public static final int ROUTE_TYPE_YAW = 2;
    //3 切换策略
    public static final int ROUTE_TYPE_CHANGE_STRATEGE = 3;
    //4 平行路切换
    public static final int ROUTE_TYPE_PARALLEL_ROAD = 4;
    //5 tmc引起的重算
    public static final int ROUTE_TYPE_TMC = 5;
    //6 道路限行 （车牌限行）
    public static final int ROUTE_TYPE_LIMIT_LINE = 6;
    //7 道路关闭
    public static final int ROUTE_TYPE_DAMAGED_ROAD = 7;
    //9 修改行程点
    public static final int ROUTE_TYPE_CHANGE_JNY_PNT = 9;
    //11 限时禁行引起的重算（在线）
    public static final int ROUTE_TYPE_LIMIT_FORBID = 11;
    //12 手动刷新
    public static final int ROUTE_TYPE_MANUAL_REFRESH = 12;
    //13 限时禁行引起的重算（本地）
    public static final int ROUTE_TYPE_LIMIT_FORBID_OFF_LINE = 13;
    //14 导航中请求备选路线
    public static final int ROUTE_TYPE_MUTI_ROUTE_REQUEST = 14;
    //16 交警调度请求路线
    public static final int ROUTE_TYPE_DISPATCH = 16;
    //17 更换终点重算
    public static final int ROUTE_TYPE_VOICE_CHANGE_DEST = 17;
    //26 组队更换终点重算
    public static final int ROUTE_TYPE_GROUP_CHANGE_DEST = 26;
    //28 路线纠回
    public static final int ROUTE_TYPE_CHANGE_ROUTE_COVERTLY = 28;

}
