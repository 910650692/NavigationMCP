package com.fy.navi.service.adapter.route;

import com.autonavi.gbl.route.observer.INaviRerouteObserver;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public interface IRouteApi {

    void initRouteService();

    void registerRouteObserver(String key, RouteResultObserver routeResultObserver);

    void setRoutePreference(RoutePreferenceID routePreferenceID);

    long requestRoute(MapTypeId mapTypeId, List<RouteParam> paramList,boolean fastNavi, int routeWay, boolean isOnline, int routeType);

    long requestRouteWeather(RouteLineLayerParam routeLineLayerParam, int index);

    long requestRouteAlternativeChargeStation(Object pathInfo, String poiId);

    boolean cancelRoute(long requestId);

    void removeRouteObserver(String key);

    void unInitRouteService();

    void setRoutePlan(boolean isNaviActive);

    void setFamiliarRoute(boolean isFamiliarRoute);

    void setAvoidRoad(RouteAvoidInfo routeAvoidInfo);

    void setRestriction(String plateNumber, boolean isTrafficRestrictionOpen);

    void setCarElecPlantion(boolean isCarElecPlanOpen);

    void setTrafficTrip(boolean isOpenTraffic);

    void sendEndEntity(PoiInfoEntity poiInfoEntity);

    void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo, MapTypeId mapTypeId);
}
