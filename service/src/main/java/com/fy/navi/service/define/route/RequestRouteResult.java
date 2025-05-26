package com.fy.navi.service.define.route;


import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.map.MapType;

import java.util.List;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RequestRouteResult {
    /*** 请求Id **/
    private long mRequestId;
    /*** 屏幕Id **/
    private MapType mMapTypeId;
    /*** 请求方式 **/
    private RouteWayID mRouteWay;
    private boolean mIsOnlineRoute = true;
    /*** 算路优先级 **/
    private int mRouteType = RoutePriorityType.ROUTE_TYPE_COMMON;
    /*** 算路优先级 **/
    private int mRouteRequestCallBackType = -1;
    /*** 路线Item结果hmi **/
    private List<RouteLineInfo> mRouteLineInfos;
    /*** 路线Item结果hmi **/
    private List<RouteParam> mRouteParams;
    /*** 路线图层参数 **/
    private RouteLineLayerParam mLineLayerParam = new RouteLineLayerParam();
    /*** 路线上服务区图层参数+详细数据 **/
    private RouteRestAreaParam mRouteRestAreaParam = new RouteRestAreaParam();
    /*** 路线上天气图层参数+详细数据 **/
    private RouteWeatherParam mRouteWeatherParam = new RouteWeatherParam();
    /*** 沿途收费站 **/
    private RouteRestTollGateParam mRouteRestTollGateParam = new RouteRestTollGateParam();
    /*** 沿途城市 **/
    private RouteAlongCityParam mRouteAlongCityParam = new RouteAlongCityParam();
    /*** 沿途交通事件 **/
    private RouteTrafficIncidentParam mRouteTrafficIncidentParam = new RouteTrafficIncidentParam();
    /*** 限行区域+详细数据 **/
    private RouteRestrictionParam mRouteRestrictionParam = new RouteRestrictionParam();
    /*** 当前选中的道路 **/
    private RouteCurrentPathParam mRouteCurrentPathParam = new RouteCurrentPathParam();
    /*** 沿途充电站 **/
    private RouteChargeStationParam mRouteChargeStationParam = new RouteChargeStationParam();
    /*** 路线终点参数 **/
    private List<LayerItemRouteEndPoint> mLayerItemRouteEndPoint;
    /*** 是否快速导航 **/
    private boolean mFastNavi = false;
    /*** 是否算路还原 **/
    private boolean mRestoration = false;
}
