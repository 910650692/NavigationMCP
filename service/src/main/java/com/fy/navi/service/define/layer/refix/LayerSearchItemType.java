package com.fy.navi.service.define.layer.refix;

public enum LayerSearchItemType {
    SEARCH_PARENT_Line,     //搜索路线图层业务
    SEARCH_PARENT_AREA,     //搜索区域图层业务，多区域面

    SEARCH_PARENT_POINT,    //搜索POI父点图层业务
    SEARCH_PARENT_PARK,     //搜索终点推荐停车场(最多3个扎标)
    SEARCH_PARENT_CHARGE_STATION,    //充电桩扎标
    SEARCH_CHILD_POINT,     //搜索POI子点图层业务
    SEARCH_POI_CENTRAL,     //搜索中心点扎标
    SEARCH_POI_BEGIN_END,   //搜索起终点途经点扎标
    SEARCH_POI_ALONG_ROUTE, //沿途搜
    SEARCH_POI_LABEL        //POI扎标图层业务
}
