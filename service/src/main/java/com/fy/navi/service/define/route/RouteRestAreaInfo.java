package com.fy.navi.service.define.route;


import java.util.ArrayList;
import java.util.List;

public class RouteRestAreaInfo {

    private List<RouteRestAreaDetailsInfo> routeRestAreaDetailsInfos = new ArrayList<>();

    public void setRouteRestAreaDetailsInfos(List<RouteRestAreaDetailsInfo> routeRestAreaDetailsInfos) {
        this.routeRestAreaDetailsInfos = routeRestAreaDetailsInfos;
    }

    public List<RouteRestAreaDetailsInfo> getRouteRestAreaDetailsInfos() {
        return routeRestAreaDetailsInfos;
    }
}
