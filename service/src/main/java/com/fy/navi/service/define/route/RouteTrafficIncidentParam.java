package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.List;

public class RouteTrafficIncidentParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteTrafficIncidentInfo> routeTrafficIncidentInfos;

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

    public List<RouteTrafficIncidentInfo> getRouteTrafficIncidentInfos() {
        return routeTrafficIncidentInfos;
    }

    public void setRouteTrafficIncidentInfos(List<RouteTrafficIncidentInfo> routeTrafficIncidentInfos) {
        this.routeTrafficIncidentInfos = routeTrafficIncidentInfos;
    }
    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }
}
