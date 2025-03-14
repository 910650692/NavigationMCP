package com.fy.navi.service.define.route;

import java.util.List;

public class RouteTrafficIncidentInfo {

    private List<RouteTrafficIncidentDetailsInfo> routeTrafficIncidentDetailsInfos;

    public void setRouteTrafficIncidentDetailsInfos(List<RouteTrafficIncidentDetailsInfo> routeTrafficIncidentDetailsInfos) {
        this.routeTrafficIncidentDetailsInfos = routeTrafficIncidentDetailsInfos;
    }

    public List<RouteTrafficIncidentDetailsInfo> getRouteTrafficIncidentDetailsInfos() {
        return routeTrafficIncidentDetailsInfos;
    }
}
