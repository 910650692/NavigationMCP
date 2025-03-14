package com.fy.navi.service.adapter.route;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class RouteAdapter {
    private static final String CLASS_API_PKG = Objects.requireNonNull(RouteAdapter.class.getPackage()).getName();
    private static final String CLASS_API_NAME = "RouteAdapterImpl";

    public static final String REGISTRE_FROM_ROUTE = "register_from_route";

    private IRouteApi mRouteApi;
    public Map<MapTypeId, List<RouteParam>> paramMap;
    public Map<MapTypeId, RouteCurrentPathParam> mRouteCurrentPathParamMap;

    private RouteAdapter() {
        paramMap = new HashMap<>();
        mRouteCurrentPathParamMap = new HashMap<>();
        mRouteApi = (IRouteApi) AdapterConfig.getObject(CLASS_API_PKG, CLASS_API_NAME);
    }

    public void initRouteService() {
        mRouteApi.initRouteService();
    }

    public void registerRouteObserver(String key, RouteResultObserver routeResultObserver) {
        mRouteApi.registerRouteObserver(key, routeResultObserver);
    }

    public void unInitRouteService() {
        mRouteApi.unInitRouteService();
    }

    public static RouteAdapter getInstance() {
        return Helper.ra;
    }

    public void setRoutePreference(RoutePreferenceID routePreferenceID) {
        mRouteApi.setRoutePreference(routePreferenceID);
    }

    public long requestRoute(MapTypeId mapTypeId, List<RouteParam> paramList,boolean fastNavi, int routeWay, boolean isOnline, int routeType) {
        return mRouteApi.requestRoute(mapTypeId, paramList,fastNavi, routeWay, isOnline, routeType);
    }

    public void saveAllPoiParamList(MapTypeId mapTypeId, List<RouteParam> paramList) {
        paramMap.put(mapTypeId, paramList);
    }

    public List<RouteParam> getAllPoiParamList(MapTypeId mapTypeId) {
        List<RouteParam> routeParams = new ArrayList<>();
        if (!ConvertUtils.isEmpty(paramMap.get(mapTypeId))) {
            routeParams.addAll(paramMap.get(mapTypeId));
        }
        return routeParams;
    }

    public long requestRouteWeather(RouteLineLayerParam routeLineLayerParam, int index) {
        return mRouteApi.requestRouteWeather(routeLineLayerParam, index);
    }

    public long requestRouteAlternativeChargeStation(Object pathInfo, String poiId) {
        return mRouteApi.requestRouteAlternativeChargeStation(pathInfo, poiId);
    }

    public boolean cancelRoute(long requestId) {
        return mRouteApi.cancelRoute(requestId);
    }

    public void setRoutePlan(boolean isNaviActive) {
        mRouteApi.setRoutePlan(isNaviActive);
    }

    public void setFamiliarRoute(boolean isFamiliarRoute) {
        mRouteApi.setFamiliarRoute(isFamiliarRoute);
    }

    public void setAvoidRoad(RouteAvoidInfo routeAvoidInfo) {
        mRouteApi.setAvoidRoad(routeAvoidInfo);
    }

    public void setRestriction(String plateNumber, boolean isTrafficRestrictionOpen) {
        mRouteApi.setRestriction(plateNumber, isTrafficRestrictionOpen);
    }

    public void setCarElecPlantion(boolean isCarElecPlanOpen) {
        mRouteApi.setCarElecPlantion(isCarElecPlanOpen);
    }

    public void setTrafficTrip(boolean isOpenTraffic) {
        mRouteApi.setTrafficTrip(isOpenTraffic);
    }

    public void setCurrentPath(RouteCurrentPathParam routeCurrentPathParam) {
        mRouteCurrentPathParamMap.put(routeCurrentPathParam.getMapTypeId(), routeCurrentPathParam);
    }

    public RouteCurrentPathParam getCurrentPath(MapTypeId mapTypeId) {
        if (!ConvertUtils.isEmpty(mRouteCurrentPathParamMap.get(mapTypeId))) {
            return mRouteCurrentPathParamMap.get(mapTypeId);
        }
        return null;
    }

    public void sendEndEntity(PoiInfoEntity poiInfoEntity) {
        mRouteApi.sendEndEntity(poiInfoEntity);
    }

    public void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo, MapTypeId mapTypeId) {
        mRouteApi.requestRouteRestoration(routeMsgPushInfo,mapTypeId);
    }

    private static final class Helper {
        private static final RouteAdapter ra = new RouteAdapter();
    }
}
