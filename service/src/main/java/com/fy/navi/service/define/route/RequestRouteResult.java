package com.fy.navi.service.define.route;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.List;

/**
 * @Description 算路结果实体
 * @Author lww
 * @date 2024/12/10
 */
public class RequestRouteResult {
    /*** 请求Id **/
    private long requestId;
    /*** 屏幕Id **/
    private MapTypeId mapTypeId;
    /*** 请求方式 **/
    private int routeWay;
    private boolean isOnlineRoute = true;
    /*** 算路优先级 **/
    private int routeType = RoutePriorityType.ROUTE_TYPE_COMMON;
    /*** 路线Item结果hmi **/
    private List<RouteLineInfo> routeLineInfos;
    /*** 路线Item结果hmi **/
    private List<RouteParam> routeParams;
    /*** 路线图层参数 **/
    private RouteLineLayerParam lineLayerParam = new RouteLineLayerParam();
    /*** 路线上服务区图层参数+详细数据 **/
    private RouteRestAreaParam routeRestAreaParam = new RouteRestAreaParam();
    /*** 路线上天气图层参数+详细数据 **/
    private RouteWeatherParam routeWeatherParam = new RouteWeatherParam();
    /*** 沿途收费站 **/
    private RouteRestTollGateParam routeRestTollGateParam = new RouteRestTollGateParam();
    /*** 沿途城市 **/
    private RouteAlongCityParam routeAlongCityParam = new RouteAlongCityParam();
    /*** 沿途交通事件 **/
    private RouteTrafficIncidentParam routeTrafficIncidentParam = new RouteTrafficIncidentParam();
    /*** 限行区域+详细数据 **/
    private RouteRestrictionParam routeRestrictionParam = new RouteRestrictionParam();
    /*** 当前选中的道路 **/
    private RouteCurrentPathParam routeCurrentPathParam = new RouteCurrentPathParam();
    /*** 沿途充电站 **/
    private RouteChargeStationParam routeChargeStationParam = new RouteChargeStationParam();
    /*** 是否快速导航 **/
    private boolean fastNavi = false;

    public int getRouteType() {
        return routeType;
    }

    public void setRouteType(int routeType) {
        this.routeType = routeType;
    }

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

    public List<RouteLineInfo> getRouteLineInfos() {
        return routeLineInfos;
    }

    public void setRouteLineInfos(List<RouteLineInfo> routeLineInfos) {
        this.routeLineInfos = routeLineInfos;
    }

    public void setRouteParams(List<RouteParam> routeParams) {
        this.routeParams = routeParams;
    }

    public List<RouteParam> getRouteParams() {
        return routeParams;
    }

    public void setFastNavi(boolean fastNavi) {
        this.fastNavi = fastNavi;
    }

    public boolean isFastNavi() {
        return this.fastNavi;
    }

    public RouteLineLayerParam getLineLayerParam() {
        return lineLayerParam;
    }

    public void setLineLayerParam(RouteLineLayerParam lineLayerParam) {
        this.lineLayerParam = lineLayerParam;
    }

    public RouteRestAreaParam getRouteRestAreaParam() {
        return routeRestAreaParam;
    }

    public void setRouteRestAreaParam(RouteRestAreaParam routeRestAreaParam) {
        this.routeRestAreaParam = routeRestAreaParam;
    }

    public RouteWeatherParam getRouteWeatherParam() {
        return routeWeatherParam;
    }

    public void setRouteWeatherParam(RouteWeatherParam routeWeatherParam) {
        this.routeWeatherParam = routeWeatherParam;
    }

    public RouteRestrictionParam getRouteRestrictionParam() {
        return routeRestrictionParam;
    }

    public void setRouteRestrictionParam(RouteRestrictionParam routeRestrictionParam) {
        this.routeRestrictionParam = routeRestrictionParam;
    }

    public void setRouteRestTollGateParam(RouteRestTollGateParam routeRestTollGateParam) {
        this.routeRestTollGateParam = routeRestTollGateParam;
    }

    public RouteRestTollGateParam getRouteRestTollGateParam() {
        return routeRestTollGateParam;
    }

    public void setRouteAlongCityParam(RouteAlongCityParam routeAlongCityParam) {
        this.routeAlongCityParam = routeAlongCityParam;
    }

    public RouteAlongCityParam getRouteAlongCityParam() {
        return routeAlongCityParam;
    }

    public void setRouteTrafficIncidentParam(RouteTrafficIncidentParam routeTrafficIncidentParam) {
        this.routeTrafficIncidentParam = routeTrafficIncidentParam;
    }

    public RouteTrafficIncidentParam getRouteTrafficIncidentParam() {
        return routeTrafficIncidentParam;
    }

    public RouteCurrentPathParam getRouteCurrentPathParam() {
        return routeCurrentPathParam;
    }

    public void setRouteCurrentPathParam(RouteCurrentPathParam routeCurrentPathParam) {
        this.routeCurrentPathParam = routeCurrentPathParam;
    }

    public RouteChargeStationParam getRouteChargeStationParam() {
        return routeChargeStationParam;
    }

    public void setRouteChargeStationParam(RouteChargeStationParam routeChargeStationParam) {
        this.routeChargeStationParam = routeChargeStationParam;
    }

    public int getRouteWay() {
        return routeWay;
    }

    public void setRouteWay(int routeWay) {
        this.routeWay = routeWay;
    }

    public boolean isOnlineRoute() {
        return isOnlineRoute;
    }

    public void setOnlineRoute(boolean onlineRoute) {
        isOnlineRoute = onlineRoute;
    }

    @NonNull
    @Override
    public String toString() {
        return "RequestRouteResult{" +
                "requestId=" + requestId +
                ", mapTypeId=" + mapTypeId +
                ", routeLineInfos=" + routeLineInfos +
                ", lineLayerParam=" + lineLayerParam +
                ", routeRestAreaParam=" + routeRestAreaParam +
                ", routeWeatherParam=" + routeWeatherParam +
                ", routeRestrictionParam=" + routeRestrictionParam +
                ", routeRestTollGateParam=" + routeRestTollGateParam +
                ", routeAlongCityParam=" + routeAlongCityParam +
                ", routeTrafficIncidentParam=" + routeTrafficIncidentParam +
                ", fastNavi=" + fastNavi +
                '}';
    }
}
