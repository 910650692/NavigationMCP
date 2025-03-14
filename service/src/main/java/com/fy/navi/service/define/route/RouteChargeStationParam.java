package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;

public class RouteChargeStationParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    /*** 充电站 **/
    private ArrayList<RouteChargeStationInfo> routeChargeStationInfos;
    /*** 底图上的路线图层信息 **/
    private ArrayList<?> pathInfoList = new ArrayList<>();

    public long getRequestId() {
        return requestId;
    }

    public void setRequestId(long requestId) {
        this.requestId = requestId;
    }

    public MapTypeId getMapTypeId() {
        return mapTypeId;
    }

    public void setMapTypeId(MapTypeId mapTypeId) {
        this.mapTypeId = mapTypeId;
    }

    public ArrayList<RouteChargeStationInfo> getRouteChargeStationInfos() {
        return routeChargeStationInfos;
    }

    public void setRouteChargeStationInfos(ArrayList<RouteChargeStationInfo> routeChargeStationInfos) {
        this.routeChargeStationInfos = routeChargeStationInfos;
    }

    public ArrayList<?> getPathInfoList() {
        return pathInfoList;
    }

    public void setPathInfoList(ArrayList<?> pathInfoList) {
        this.pathInfoList = pathInfoList;
    }
}
