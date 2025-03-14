package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.List;

public class RouteRestTollGateParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;

    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteRestTollGateInfo> routeRestTollGateInfos;

    public void setRequestId(long requestId) {
        this.requestId = requestId;
    }

    public long getRequestId() {
        return requestId;
    }

    public void setMapTypeId(MapTypeId mapTypeId) {
        this.mapTypeId = mapTypeId;
    }

    public MapTypeId getMapTypeId() {
        return mapTypeId;
    }

    public void setRouteRestTollGateInfos(List<RouteRestTollGateInfo> routeRestTollGateInfos) {
        this.routeRestTollGateInfos = routeRestTollGateInfos;
    }

    public List<RouteRestTollGateInfo> getRouteRestTollGateInfos() {
        return routeRestTollGateInfos;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }
}
