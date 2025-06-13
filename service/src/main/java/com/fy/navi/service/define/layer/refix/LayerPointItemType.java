package com.fy.navi.service.define.layer.refix;

/**
 * 扎标类型
 */
public enum LayerPointItemType {
    NULL,
    SEARCH_PARENT_Line_Road,     //搜索路线图层业务 道路线类型
    SEARCH_PARENT_Line_Park,     //搜索路线图层业务  停车场类型线
    SEARCH_PARENT_AREA,          //搜索区域图层业务，多区域面

    SEARCH_PARENT_POINT,    //搜索POI父点图层业务
    SEARCH_PARENT_PARK,     //搜索终点推荐停车场
    SEARCH_PARENT_CHARGE_STATION,    //充电桩扎标
    SEARCH_CHILD_POINT,     //搜索POI子点图层业务
    SEARCH_POI_CENTRAL,     //搜索中心点扎标
    SEARCH_POI_BEGIN_END,   //搜索起终点途经点扎标
    SEARCH_POI_ALONG_ROUTE, //沿途搜
    SEARCH_POI_LABEL,       //POI扎标图层业务

    ROUTE_PATH,             //路线
    ROUTE_POINT_START,      //起点

    ROUTE_POINT_END,        //终点默认样式扎标
    ROUTE_POINT_END_OIL,    //终点剩余油量扎标
    ROUTE_POINT_END_BATTERY,   //终点剩余电量扎标
    ROUTE_POINT_END_PARK,   //终点可停车扎标
    ROUTE_POINT_END_BUSINESS_HOURS,  //终点营业时间扎标

    ROUTE_POINT_VIA,        //途经点默认扎标
    ROUTE_POINT_VIA_CHARGE,     //途经点是充电站扎标
    ROUTE_POINT_VIA_CHARGE_STATION,  //路线补能规划扎标
    ROUTE_POINT_VIA_REPLACE_CHARGE,  //替换补能扎标

    ROUTE_POINT_WEATHER,    //路线天气
    ROUTE_POINT_REST_AREA,  //路线服务区
    ROUTE_POINT_TRAFFIC_EVENT,  //路线交通事件
    ROUTE_POINT_JAM         //路线拥堵
}
