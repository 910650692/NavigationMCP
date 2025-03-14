package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.List;

public class RouteAlongCityParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteAlongCityInfo> routeAlongCityInfos;

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

    public void setRouteAlongCityInfos(List<RouteAlongCityInfo> routeAlongCityInfos) {
        this.routeAlongCityInfos = routeAlongCityInfos;
    }

    public List<RouteAlongCityInfo> getRouteAlongCityInfos() {
        return routeAlongCityInfos;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }
}
