package com.fy.navi.service.define.route;


import java.util.List;

public class RouteRestTollGateInfo {
    private List<RouteRestTollGateDetailsInfo> routeRestTollGateDetailsInfos;

    public void setRouteRestTollGateDetailsInfos(List<RouteRestTollGateDetailsInfo> routeRestTollGateDetailsInfos) {
        this.routeRestTollGateDetailsInfos = routeRestTollGateDetailsInfos;
    }

    public List<RouteRestTollGateDetailsInfo> getRouteRestTollGateDetailsInfos() {
        return routeRestTollGateDetailsInfos;
    }
}
