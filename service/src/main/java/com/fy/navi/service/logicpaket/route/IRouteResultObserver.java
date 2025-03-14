package com.fy.navi.service.logicpaket.route;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWeatherParam;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface IRouteResultObserver {
    default void onRouteSuccess() {
    }

    default void onRouteResult(RequestRouteResult requestRouteResult) {
    }

    default void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
    }

    default void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam) {
    }

    default void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam) {
    }

    default void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam) {
    }

    default void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
    }

    default void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam) {
    }

    default void onRouteDrawReStrictedAreaInfo(RouteRestrictionParam routeRestrictionParam) {
    }

    default void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam) {
    }

    default void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam) {
    }

    default void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam) {
    }

    default void onRouteAllRoutePoiInfo(RequestRouteResult requestRouteResult) {
    }

    default void onRouteFail(MapTypeId mapTypeId, String errorMsg) {
    }

    default void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
    }

    default void onRouteRequest() {
    }

    default void onRouteSlected(MapTypeId mapTypeId, int routeIndex) {
    }

    default void onL2DataCallBack(String json) {

    }

    default void onSpeechViaNum(int size) {

    }

    default void onSpeechEndCityName(String cityName) {

    }
}
