package com.fy.navi.service.adapter.route;

import com.fy.navi.service.define.layer.RouteLineLayerParam;
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
public interface RouteResultObserver {

    void onRouteSuccess();

    void onRouteResult(RequestRouteResult requestRouteResult);

    void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam);

    void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam);

    void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam);

    void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam);
    void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam);

    void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam);

    void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam);

    void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam);

    void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam);

    void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos);

    void onRouteFail(RequestRouteResult requestRouteResult, int errorCode, String errorMsg);

    void onRouteL2Info(String json);
}
