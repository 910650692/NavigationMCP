package com.fy.navi.service.define.route;

import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.List;

public class RouteWeatherParam {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    private boolean isOnlineRoute = true;
    /*** 详情数据 **/
    private List<RouteWeatherInfo> routeWeatherInfos;
    /*** 绘制天气参数 **/
    private ArrayList<?> weatherLabelItem = new ArrayList<>();

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

    public List<RouteWeatherInfo> getRouteWeatherInfos() {
        return routeWeatherInfos;
    }

    public void setRouteWeatherInfos(List<RouteWeatherInfo> routeWeatherInfos) {
        this.routeWeatherInfos = routeWeatherInfos;
    }

    public ArrayList<?> getWeatherLabelItem() {
        return weatherLabelItem;
    }

    public void setWeatherLabelItem(ArrayList<?> weatherLabelItems) {
        this.weatherLabelItem = weatherLabelItems;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }
}
