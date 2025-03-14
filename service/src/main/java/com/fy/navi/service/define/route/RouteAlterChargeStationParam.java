package com.fy.navi.service.define.route;

import java.util.ArrayList;

public class RouteAlterChargeStationParam {
    /*** 请求Id **/
    private long requestId;
    /*** 补充充电站 **/
    private ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos;

    public long getRequestId() {
        return requestId;
    }

    public void setRequestId(long requestId) {
        this.requestId = requestId;
    }


    public ArrayList<RouteAlterChargeStationInfo> getRouteAlternativeChargeStationInfos() {
        return routeAlterChargeStationInfos;
    }

    public void setRouteAlternativeChargeStationInfos(ArrayList<RouteAlterChargeStationInfo> routeAlterChargeStationInfos) {
        this.routeAlterChargeStationInfos = routeAlterChargeStationInfos;
    }
}
