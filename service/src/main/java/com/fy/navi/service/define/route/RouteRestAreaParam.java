package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.List;

public class RouteRestAreaParam {

    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 服务区信息 **/
    private List<RouteRestAreaInfo> routeRestAreaInfos;
    /*** 服务区扎点使用 **/
    private ArrayList<?> pathInfoList = new ArrayList<>();

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

    public List<RouteRestAreaInfo> getRouteRestAreaInfos() {
        return routeRestAreaInfos;
    }

    public void setRouteRestAreaInfos(List<RouteRestAreaInfo> routeRestAreaInfos) {
        this.routeRestAreaInfos = routeRestAreaInfos;
    }

    public ArrayList<?> getPathInfoList() {
        return pathInfoList;
    }

    public void setPathInfoList(ArrayList<?> pathInfoList) {
        this.pathInfoList = pathInfoList;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }
}
