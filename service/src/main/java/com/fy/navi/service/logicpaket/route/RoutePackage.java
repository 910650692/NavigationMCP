package com.fy.navi.service.logicpaket.route;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.aos.QueryRestrictedObserver;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.adapter.route.RouteResultObserver;
import com.fy.navi.service.adapter.search.SearchAdapter;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.route.RouteWeatherParam;
import com.fy.navi.service.define.search.ETAInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchRequestParameter;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.utils.BevPowerCarUtils;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

import lombok.Getter;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class RoutePackage implements RouteResultObserver, QueryRestrictedObserver {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private SignalPackage mSignalPackage;
    private CalibrationPackage mCalibrationPackage;
    private ConcurrentHashMap<String, IRouteResultObserver> routeResultObserverMap;
    private Map<MapTypeId, RouteParam> startRouteParams;
    private Map<MapTypeId, List<RouteParam>> viaRouteParams;
    private Map<MapTypeId, List<RouteParam>> saveViaRouteParams;
    private Map<MapTypeId, RouteParam> endRouteParams;
    private Map<MapTypeId, RouteParam> saveEndRouteParams;
    private Map<MapTypeId, RequestRouteResult> mRequestRouteResults;
    private ArrayList<EvRangeOnRouteInfo> mEvRangeOnRouteInfos;
    private RouteAdapter mRouteAdapter;
    private NavistatusAdapter mNaviStatusAdapter;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private PositionAdapter mPositionAdapter;
    private MapAdapter mMapAdapter;
    private BlAosAdapter mBlAosAdapter;

    private EngineAdapter mEngineAdapter;

    private int[] offlineRouteErrorCode = {822083585, 822083587, 822083584, 822083590, 822083592, 822083593, 822083594, 822083595, 822083596, 822083599, 822083600, 822083602};

    public boolean isNeedAutoOfflineRoute(int errorCode) {
        for (int t = 0; t < offlineRouteErrorCode.length; t++) {
            if (errorCode == offlineRouteErrorCode[t]) return true;
        }
        return false;
    }

    @Getter
    private Map<MapTypeId, Integer> selectRouteIndex;

    private RoutePackage() {
        routeResultObserverMap = new ConcurrentHashMap<>();
        startRouteParams = new HashMap<>();
        viaRouteParams = new HashMap<>();
        endRouteParams = new HashMap<>();
        saveViaRouteParams = new HashMap<>();
        saveEndRouteParams = new HashMap<>();
        selectRouteIndex = new HashMap<>();

        mRequestRouteResults = new HashMap<>();
        mEvRangeOnRouteInfos = new ArrayList<>();
        mRouteAdapter = RouteAdapter.getInstance();
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mPositionAdapter = PositionAdapter.getInstance();
        mMapAdapter = MapAdapter.getInstance();
        mBlAosAdapter = BlAosAdapter.getInstance();
        mSignalPackage = SignalPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mEngineAdapter = EngineAdapter.getInstance();
    }

    public void initRouteService() {
        mRouteAdapter.initRouteService();
        mRouteAdapter.registerRouteObserver(RouteAdapter.REGISTRE_FROM_ROUTE, this);
        mBlAosAdapter.addRestrictedObserver("routePackage", this);
    }

    public void registerRouteObserver(String key, IRouteResultObserver observer) {
        routeResultObserverMap.put(key, observer);
    }

    public void unRegisterRouteObserver(String key) {
        routeResultObserverMap.remove(key);
    }

    public void unInitNaviService() {
        mRouteAdapter.unInitRouteService();
    }

    public static RoutePackage getInstance() {
        return Helper.ep;
    }

    @Override
    public void onRouteSuccess() {
        Logger.i(TAG, "onRouteSuccess");
        for (String key : routeResultObserverMap.keySet()) {
            IRouteResultObserver routeResultObserver = routeResultObserverMap.get(key);
            if (null == routeResultObserver) routeResultObserverMap.remove(key);
            else routeResultObserver.onRouteSuccess();
        }
    }

    @Override
    public void onRouteResult(RequestRouteResult requestRouteResult) {
        Logger.i(TAG, "onRouteResult");
        mRequestRouteResults.put(requestRouteResult.getMapTypeId(), requestRouteResult);
        updateParamList(requestRouteResult.getMapTypeId());
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.SELECT_ROUTE);
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteResult(requestRouteResult);
        }
        callBackToSpeech(requestRouteResult.getMapTypeId());
    }

    private void callBackToSpeech(MapTypeId mapTypeId) {
        int size = viaRouteParams.get(mapTypeId).size();
        String cityName = "上海市";
        if (getEndPoint(mapTypeId).getAdCode() != 0 && !ConvertUtils.isEmpty(MapDataPackage.getInstance().getCityInfo(getEndPoint(mapTypeId).getAdCode()))) {
            cityName = MapDataPackage.getInstance().getCityInfo(getEndPoint(mapTypeId).getAdCode()).name;
        }
        Logger.i(TAG, cityName + "----" + size);
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onSpeechViaNum(size);
            routeResultObserver.onSpeechEndCityName(cityName);
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine");
        RequestRouteResult requestRouteResult = mRequestRouteResults.get(routeLineLayerParam.getMapTypeId());
        if (!ConvertUtils.isEmpty(requestRouteResult)){
            if (requestRouteResult.isFastNavi()) {
                mNaviAdapter.updateNaviPath(NumberUtils.NUM_0, requestRouteResult.getLineLayerParam());
            }
        }
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteDrawLine(routeLineLayerParam);
        }
    }

    @Override
    public void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam) {
        Logger.i(TAG, "onRouteRestAreaInfo");
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteRestAreaInfo(routeRestAreaParam);
        }
    }

    @Override
    public void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam) {
        Logger.i(TAG, "onRouteWeatherInfo");
        if (ConvertUtils.isEmpty(routeWeatherParam)) {
            for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                routeResultObserver.onRouteWeatherInfo(null);
            }
            return;
        }
        showWeatherView(routeWeatherParam.getMapTypeId());
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteWeatherInfo(routeWeatherParam);
        }
    }

    @Override
    public void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam) {
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteChargeStationInfo(routeChargeStationParam);
        }
    }

    @Override
    public void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    @Override
    public void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam) {
        Logger.i(TAG, "onRouteRestrictionInfo");
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteRestrictionInfo(routeRestrictionParam);
        }
    }

    @Override
    public void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam) {
        Logger.i(TAG, "onRouteRestTollGateInfo");
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteRestTollGateInfo(routeRestTollGateParam);
        }
    }

    @Override
    public void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam) {
        Logger.i(TAG, "onRouteCityInfo");
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteCityInfo(routeAlongCityParam);
        }
    }

    @Override
    public void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam) {
        Logger.i(TAG, "onRouteTrafficIncidentInfo");
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteTrafficIncidentInfo(routeTrafficIncidentParam);
        }
    }

    @Override
    public void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
        Logger.i(TAG, "onRouteRanges");
        mEvRangeOnRouteInfos = evRangeOnRouteInfos;
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteRanges(evRangeOnRouteInfos);
        }
    }

    @Override
    public void onRouteFail(RequestRouteResult requestRouteResult, int errorCode, String errorMsg) {
        Logger.i(TAG, "onRouteFail");
        if (ConvertUtils.isEmpty(requestRouteResult)) return;
        if (isNeedAutoOfflineRoute(errorCode) && requestRouteResult.isOnlineRoute()) {
            requestRouteOffline(requestRouteResult.getMapTypeId(), requestRouteResult.isFastNavi(), requestRouteResult.getRouteWay());
            return;
        }
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        reGetParamList(requestRouteResult.getMapTypeId());
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteFail(requestRouteResult.getMapTypeId(), errorMsg);
        }
    }

    @Override
    public void onRouteL2Info(String json) {
        l2DatacallBack(json);
    }

    private void l2DatacallBack(String json) {
        if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                routeResultObserver.onL2DataCallBack(json);
            }
        }
    }

    //---------------主动方法-------------------------------

    public long requestRoute(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity, int poiType, boolean fastNavi, int routeWay) {
        return requestRoute(mapTypeId, poiInfoEntity, poiType, fastNavi, routeWay, RoutePriorityType.ROUTE_TYPE_COMMON);
    }

    public long requestRoute(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity, int poiType, boolean fastNavi, int routeWay, int routeType) {
        if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                routeResultObserver.onRouteRequest();
            }
        }
        //后续使用setting adapter
        mRouteAdapter.setRoutePreference(SettingPackage.getInstance().getRoutePreference());
        RouteParam routeParam = getRouteParam(poiInfoEntity, poiType);
        List<RouteParam> paramList = getParamList(mapTypeId, routeParam);
        if (ConvertUtils.isEmpty(paramList)) return NumberUtils.NUM_ERROR;

        mRouteAdapter.setRoutePlan(mNaviStatusAdapter.isGuidanceActive());
        mRouteAdapter.setFamiliarRoute(true);
        String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        String aVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        mRouteAdapter.setRestriction(platNum, "true".equals(aVoidLimit));
        initBevCarData();
        mRouteAdapter.setCarElecPlantion(BevPowerCarUtils.getInstance().bevCarElicOpen);
        mRouteAdapter.setTrafficTrip(true);
        selectRouteIndex.put(mapTypeId, NumberUtils.NUM_ERROR);
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        return mRouteAdapter.requestRoute(mapTypeId, paramList, fastNavi, routeWay, true, routeType);
    }

    public void requestRouteRestoration(RouteMsgPushInfo routeMsgPushInfo, MapTypeId mapTypeId) {
        if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                routeResultObserver.onRouteRequest();
            }
        }
        mRouteAdapter.setRoutePreference(SettingPackage.getInstance().getRoutePreference());
        List<RouteParam> paramList = new ArrayList<>();
        RouteParam routeParam = getRouteParam(routeMsgPushInfo.getPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_END);
        paramList = getParamList(mapTypeId, routeParam);
        if (routeMsgPushInfo.getViaPoiInfoEntity() != null
                && !routeMsgPushInfo.getViaPoiInfoEntity().isEmpty()) {
            clearVai(mapTypeId);
            for (PoiInfoEntity vaiPoiInfoEntity :routeMsgPushInfo.getViaPoiInfoEntity()) {
                RouteParam vaiParam = getRouteParam(vaiPoiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                paramList = getParamList(mapTypeId, vaiParam);
            }
        } else {
            clearVai(mapTypeId);
        }

        if (ConvertUtils.isEmpty(paramList)) return ;

        mRouteAdapter.setRoutePlan(mNaviStatusAdapter.isGuidanceActive());
        mRouteAdapter.setFamiliarRoute(true);
        String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        String aVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        mRouteAdapter.setRestriction(platNum, "true".equals(aVoidLimit));
        initBevCarData();
        mRouteAdapter.setCarElecPlantion(BevPowerCarUtils.getInstance().bevCarElicOpen);
        mRouteAdapter.setTrafficTrip(true);
        selectRouteIndex.put(mapTypeId, NumberUtils.NUM_ERROR);
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        mRouteAdapter.requestRouteRestoration(routeMsgPushInfo,mapTypeId);
    }

    private void initBevCarData() {
        if (mSignalPackage.getMaxBatteryEnergy() > 0) {
            BevPowerCarUtils.getInstance().maxBattenergy = mSignalPackage.getMaxBatteryEnergy();
        }
        if (mCalibrationPackage.vehicleWeight() > 0) {
            BevPowerCarUtils.getInstance().vehicleWeight = (short) mCalibrationPackage.vehicleWeight();
        }
        BevPowerCarUtils.getInstance().extraBrand = mCalibrationPackage.brandName();
        BevPowerCarUtils.getInstance().vehicleType = mCalibrationPackage.modelName();
        if (mSignalPackage.getBatteryEnergy() > 0) {
            BevPowerCarUtils.getInstance().initlialHVBattenergy = mSignalPackage.getBatteryEnergy();
        }
        if (mSignalPackage.getChargeSystemStatus() > 0) {
            BevPowerCarUtils.getInstance().charging = mSignalPackage.getChargeSystemStatus();
        }
        if (mSignalPackage.getOutsideTemperature() > 0) {
            BevPowerCarUtils.getInstance().temperature = (int) mSignalPackage.getOutsideTemperature();
        }

        BevPowerCarUtils.getInstance().engineVersion = mEngineAdapter.getEngineVersion();
        BevPowerCarUtils.getInstance().sdkVersion = mEngineAdapter.getSdkVersion();
    }

    private long requestRouteOffline(MapTypeId mapTypeId, boolean fastNavi, int routeWay) {
        return requestRouteOffline(mapTypeId, fastNavi, routeWay, RoutePriorityType.ROUTE_TYPE_COMMON);
    }

    private long requestRouteOffline(MapTypeId mapTypeId, boolean fastNavi, int routeWay, int routeType) {
        if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                routeResultObserver.onRouteRequest();
            }
        }
        mRouteAdapter.setRoutePreference(SettingPackage.getInstance().getRoutePreference());
        RouteParam routeParam = getRouteParam(null, -1);
        List<RouteParam> paramList = getParamList(mapTypeId, routeParam);
        if (ConvertUtils.isEmpty(paramList)) return NumberUtils.NUM_ERROR;
        mRouteAdapter.setRoutePlan(mNaviStatusAdapter.isGuidanceActive());
        mRouteAdapter.setFamiliarRoute(true);
        String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        String aVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        mRouteAdapter.setRestriction(platNum, "true".equals(aVoidLimit));
        mRouteAdapter.setCarElecPlantion(BevPowerCarUtils.getInstance().bevCarElicOpen);
        mRouteAdapter.setTrafficTrip(true);
        selectRouteIndex.put(mapTypeId, NumberUtils.NUM_ERROR);
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        return mRouteAdapter.requestRoute(mapTypeId, paramList, fastNavi, routeWay, false, routeType);
    }

    //数据转化-搜索poi转param
    public RouteParam getRouteParam(PoiInfoEntity poiInfoEntity, int poiType) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return null;
        }
        if (poiType == RoutePoiType.ROUTE_POI_TYPE_END) {
            mRouteAdapter.sendEndEntity(poiInfoEntity);
        }
        RouteParam routeParam = new RouteParam();
        routeParam.setName(poiInfoEntity.getName());
        routeParam.setAddress(poiInfoEntity.getAddress());
        routeParam.setPoiType(poiType);
        routeParam.setPoiID(poiInfoEntity.getPid());
        routeParam.setAdCode(poiInfoEntity.getAdCode());
        if (!ConvertUtils.isEmpty(poiInfoEntity.getCityInfo())) {
            routeParam.setAdCode(poiInfoEntity.getCityInfo().getCityCode());
        }
        GeoPoint geoPoint = new GeoPoint();
        geoPoint.setLon(poiInfoEntity.getPoint().getLon());
        geoPoint.setLat(poiInfoEntity.getPoint().getLat());
        routeParam.setRealPos(geoPoint);
        return routeParam;
    }

    public void addViaPoint(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity, int poiType) {
        if (isBelongRouteParam(mapTypeId, poiInfoEntity)) {
            if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
                for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                    if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                    routeResultObserver.onRouteFail(mapTypeId, "途经点添加失败：途经点已经在路线上");
                }
            }
            return;
        }
        if (isMaxRouteParam(mapTypeId)) {
            if (!ConvertUtils.isEmpty(routeResultObserverMap)) {
                for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
                    if (ConvertUtils.isEmpty(routeResultObserver)) continue;
                    routeResultObserver.onRouteFail(mapTypeId, "途经点添加失败：途经点超过最大限制");
                }
            }
            return;
        }
        requestRoute(mapTypeId, poiInfoEntity, poiType,mNaviStatusAdapter.isGuidanceActive(), RouteWayID.ROUTE_WAY_ADD_VIA);
    }
    //删除途经点--是否马上发起请求
    public boolean removeVia(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity, boolean isRequestRoute) {
        List<RouteParam> routeParams = viaRouteParams.get(mapTypeId);
        if (routeParams.size() > 0) {
            int index = -1;
            for (int t = NumberUtils.NUM_0; t < routeParams.size(); t++) {
                if (poiInfoEntity.getPid() == routeParams.get(t).getPoiID() || (poiInfoEntity.getPoint().lon == routeParams.get(t).getRealPos().lon && poiInfoEntity.getPoint().lat == routeParams.get(t).getRealPos().lat)) {
                    index = t;
                }
            }
            routeParams.remove(index);
            viaRouteParams.put(mapTypeId, routeParams);
            if (isRequestRoute) requestRoute(mapTypeId, null, -1, mNaviStatusAdapter.isGuidanceActive(), RouteWayID.ROUTE_WAY_DELETE_VIA);
            else mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
            return true;
        }
        return false;
    }

    //清空途径点
    public void clearVai(MapTypeId mapTypeId) {
        viaRouteParams.put(mapTypeId, new ArrayList<>());
    }

    //此点是否已经在路线上了
    public boolean isBelongRouteParam(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity) {
        List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    public boolean isStartOrEndRouteParam(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity) {
        List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_START || routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_END) {
                    return true;
                }
            }
        }
        return false;
    }
    public boolean isTheSamePoi(RouteParam routeParam, PoiInfoEntity poiInfoEntity) {
        if (routeParam.getPoiID() == poiInfoEntity.getPid() || (routeParam.getRealPos().lat == poiInfoEntity.getPoint().lat && routeParam.getRealPos().lon == poiInfoEntity.getPoint().lon)) {
            return true;
        }
        return false;
    }
    //超过途经点的数量
    public boolean isMaxRouteParam(MapTypeId mapTypeId) {
        List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        if (allPoiParamList.size() < 7) {
            return false;
        }
        return true;
    }
    //回调当前路线点的信息
    private void savePointAndCallBack(MapTypeId mapTypeId, List<RouteParam> routeParams) {
        if (ConvertUtils.isEmpty(mRequestRouteResults)) return;
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        mRequestRouteResults.get(mapTypeId).setRouteParams(routeParams);
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteAllRoutePoiInfo(mRequestRouteResults.get(mapTypeId));
        }
    }
    //请求路线上的天气
    public long requestRouteWeather(MapTypeId mapTypeId, int index) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return -1;
        return mRouteAdapter.requestRouteWeather(mRequestRouteResults.get(mapTypeId).getLineLayerParam(), index);
    }
    //请求备用充电桩
    public long requestRouteAlternativeChargeStation(MapTypeId mapTypeId, String poiId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return -1;
        RouteChargeStationParam routeChargeStationParam =mRequestRouteResults.get(mapTypeId).getRouteChargeStationParam();
        if (ConvertUtils.isEmpty(routeChargeStationParam)) return -1;
        if (ConvertUtils.isEmpty(routeChargeStationParam.getPathInfoList())) return -1;
        return mRouteAdapter.requestRouteAlternativeChargeStation(routeChargeStationParam.getPathInfoList().get(selectRouteIndex.get(mapTypeId)) , poiId);
    }
    //绘制路线
    public void showRouteLine(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        ArrayList<String> arrivalTimes = new ArrayList<>();
        List<RouteLineInfo> routeLineInfos = mRequestRouteResults.get(mapTypeId).getRouteLineInfos();
        RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getLineLayerParam();
        for (RouteLineInfo routeLineInfo : routeLineInfos) {
            arrivalTimes.add(routeLineInfo.getTravelTime());
        }
        routeLineLayerParam.setEstimatedTimeOfArrival(arrivalTimes);
        mLayerAdapter.drawRouteLine(routeLineLayerParam);
    }
    //设置路线全览
    public void showPreview(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getLineLayerParam();
        PreviewParams previewParams = mLayerAdapter.getPathResultBound(mapTypeId, routeLineLayerParam.getPathInfoList());
        previewParams.setScreenLeft(1144);
        previewParams.setScreenBottom(132);
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }

    //设置路线全览
    public void naviShowPreview(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getLineLayerParam();
        PreviewParams previewParams = mLayerAdapter.getPathResultBound(mapTypeId, routeLineLayerParam.getPathInfoList());
        previewParams.setScreenLeft(100);
        previewParams.setScreenRight(100);
        previewParams.setScreenTop(150);
        previewParams.setScreenBottom(200);
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }
    //选择路线
    public void selectRoute(MapTypeId mapTypeId, int routeIndex) {
        //更新路线
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        mLayerAdapter.setSelectedPathIndex(mapTypeId, routeIndex);
        selectRouteIndex.put(mapTypeId, routeIndex);
        mNaviAdapter.setNaviPath(routeIndex, mRequestRouteResults.get(mapTypeId).getLineLayerParam());
        RouteCurrentPathParam routeCurrentPathParam = mRequestRouteResults.get(mapTypeId).getRouteCurrentPathParam();
        routeCurrentPathParam.setMapTypeId(mapTypeId);
        routeCurrentPathParam.setRequestId(mRequestRouteResults.get(mapTypeId).getRequestId());
        routeCurrentPathParam.setPathInfo(mRequestRouteResults.get(mapTypeId).getLineLayerParam().getPathInfoList().get(routeIndex));
        mRouteAdapter.setCurrentPath(routeCurrentPathParam);
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteSlected(mapTypeId, routeIndex);
        }
    }
    //请求限行数据
    public long requestRestirction(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults) || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId)) || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId).getRouteRestrictionParam())) return -1;
        List<String> ruleIDs = mRequestRouteResults.get(mapTypeId).getRouteRestrictionParam().getRuleIds();
        if (ruleIDs.size() > selectRouteIndex.get(mapTypeId)
                && !ConvertUtils.isEmpty(ruleIDs.get(selectRouteIndex.get(mapTypeId)))) {
            RestrictedParam restrictedParam = new RestrictedParam();
            restrictedParam.setRestrict_type(9);
            restrictedParam.setPlate(SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER));
            restrictedParam.setRuleids(ruleIDs.get(selectRouteIndex.get(mapTypeId)));
            return mBlAosAdapter.queryRestrictedInfo(restrictedParam);
        }
        return -1;
    }

    public boolean cancelRoute(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return false;
        return mRouteAdapter.cancelRoute(mRequestRouteResults.get(mapTypeId).getRequestId());
    }
    //清除路线信息
    public void clearRouteLine(MapTypeId mapTypeId) {
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        mLayerAdapter.clearRouteLine(mapTypeId);
        removeAllRouteInfo(mapTypeId);
    }
    //展示路线服务区扎点
    public void showRestArea(MapTypeId mapTypeId, int index) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        mLayerAdapter.showRestArea(mapTypeId, mRequestRouteResults.get(mapTypeId).getRouteRestAreaParam().getPathInfoList() ,index);
    }
    //清除路线服务区扎点
    public void clearRestArea(MapTypeId mapTypeId) {
        mLayerAdapter.showRestArea(mapTypeId, null ,-1);
    }
    //展示天气扎点
    public void showWeatherView(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) return;
        mLayerAdapter.showWeatherView(mapTypeId, mRequestRouteResults.get(mapTypeId).getRouteWeatherParam().getWeatherLabelItem());
    }
    //清除天气扎点
    public void clearWeatherView(MapTypeId mapTypeId) {
        mLayerAdapter.showWeatherView(mapTypeId, null);
    }
    //展示限行图层
    public void showRestrictionView(MapTypeId mapTypeId, Object object) {
        mLayerAdapter.showRestrictionView(mapTypeId, object);
    }
    //清除限行图层
    public void clearRestrictionView(MapTypeId mapTypeId) {
        mLayerAdapter.showRestrictionView(mapTypeId, null);
    }
    //排序删除途经点
    public void updateViaParamList(MapTypeId mapTypeId, List<RouteParam> routeParams) {
        viaRouteParams.put(mapTypeId, routeParams);
    }
    //算路拿到点信息
    private List<RouteParam> getParamList(MapTypeId mapTypeId, RouteParam routeParam) {
        if (ConvertUtils.isEmpty(startRouteParams.get(mapTypeId))) {
            Logger.i(TAG, "use location start point");
            RouteParam startParam = new RouteParam();
            GeoPoint startCoordinate = new GeoPoint();
            startCoordinate.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
            startCoordinate.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
            startParam.setRealPos(startCoordinate);
            startParam.setPoiType(RoutePoiType.ROUTE_POI_TYPE_START);
            startParam.setRoadID(mPositionAdapter.getLastCarLocation().getRoadId());
            Logger.i(TAG, mPositionAdapter.getLastCarLocation().getRoadId());
            startRouteParams.put(mapTypeId, startParam);
        }
        if (ConvertUtils.isEmpty(viaRouteParams.get(mapTypeId))) {
            viaRouteParams.put(mapTypeId, new ArrayList<>());
        }
        List<RouteParam> routeParams = new ArrayList<>();
        if (ConvertUtils.isEmpty(routeParam)) {
            routeParams.add(startRouteParams.get(mapTypeId));
            if (viaRouteParams.get(mapTypeId).size() != NumberUtils.NUM_0) {
                routeParams.addAll(viaRouteParams.get(mapTypeId));
            }
            routeParams.add(endRouteParams.get(mapTypeId));
            return routeParams;
        }
        if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_WAY) {
            List<RouteParam> viaParams = viaRouteParams.get(mapTypeId);
            viaParams.add(routeParam);
            viaRouteParams.put(mapTypeId, viaParams);
            routeParams.add(startRouteParams.get(mapTypeId));
            routeParams.addAll(viaRouteParams.get(mapTypeId));
            routeParams.add(endRouteParams.get(mapTypeId));
            return routeParams;
        } else if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_END) {
            endRouteParams.put(mapTypeId, routeParam);
            routeParams.add(startRouteParams.get(mapTypeId));
            if (viaRouteParams.get(mapTypeId).size() != NumberUtils.NUM_0) {
                routeParams.addAll(viaRouteParams.get(mapTypeId));
            }
            routeParams.add(endRouteParams.get(mapTypeId));
            return routeParams;
        }
        Logger.i(TAG, "error param");
        return routeParams;
    }
    //获取当前路线上的所有点--去除掉已经经过的点
    public List<RouteParam> getAllPoiParamList(MapTypeId mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return new ArrayList<>();
        }
        return mRouteAdapter.getAllPoiParamList(mapTypeId);
    }
    //算路成功--保存当前路线上的所有点
    private void updateParamList(MapTypeId mapTypeId) {
        List<RouteParam> routeParamList = viaRouteParams.get(mapTypeId);
        List<RouteParam> routeParamVias = new ArrayList<>();
        routeParamVias.addAll(routeParamList);
        saveViaRouteParams.put(mapTypeId, routeParamVias);
        saveEndRouteParams.put(mapTypeId, endRouteParams.get(mapTypeId));
        mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
        savePointAndCallBack(mapTypeId, getCurrentParamsList(mapTypeId));
    }
    //算路失败，重新获取原来的点并保存
    private void reGetParamList(MapTypeId mapTypeId) {
        if (!ConvertUtils.isEmpty(saveViaRouteParams)) {
            List<RouteParam> routeParamList = saveViaRouteParams.get(mapTypeId);
            List<RouteParam> routeParamVias = new ArrayList<>();
            routeParamVias.addAll(routeParamList);
            viaRouteParams.put(mapTypeId, routeParamVias);
        }
        if (!ConvertUtils.isEmpty(saveEndRouteParams)) {
            endRouteParams.put(mapTypeId, saveEndRouteParams.get(mapTypeId));
        }
        mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
        savePointAndCallBack(mapTypeId, getCurrentParamsList(mapTypeId));
    }
    //获取原始输入的点
    private List<RouteParam> getCurrentParamsList(MapTypeId mapTypeId) {
        List<RouteParam> params = new ArrayList<>();
        if (!ConvertUtils.isEmpty(startRouteParams.get(mapTypeId))) {
            params.add(startRouteParams.get(mapTypeId));
        }

        if (!ConvertUtils.isEmpty(viaRouteParams.get(mapTypeId))) {
            params.addAll(viaRouteParams.get(mapTypeId));
        }

        if (!ConvertUtils.isEmpty(endRouteParams.get(mapTypeId))) {
            params.add(endRouteParams.get(mapTypeId));
        }
        return params;
    }
    //清空数据缓存
    public void removeAllRouteInfo(MapTypeId mapTypeId) {
        mRequestRouteResults.put(mapTypeId, null);
        startRouteParams.put(mapTypeId, null);
        viaRouteParams.put(mapTypeId, new ArrayList<>());
        endRouteParams.put(mapTypeId, null);
        saveViaRouteParams.put(mapTypeId, new ArrayList<>());
        saveEndRouteParams.put(mapTypeId, null);
    }
    //获取终点信息
    public RouteParam getEndPoint(MapTypeId mapTypeId) {
        return endRouteParams.get(mapTypeId);
    }
    //获取当前的路线信息---沿途搜索
    public RouteCurrentPathParam getCurrentPathInfo(MapTypeId mapTypeId) {
        return mRouteAdapter.getCurrentPath(mapTypeId);
    }
    //提供给语音--能量耗尽信息
    public ArrayList<EvRangeOnRouteInfo> getEvRangeOnRouteInfos() {
        return mEvRangeOnRouteInfos;
    }
    //绘制限行区域
    public void drawRestrictionForLimit(MapTypeId mapTypeId, Object param, int position) {
        mLayerAdapter.showRestrictionView(mapTypeId, null);
        mLayerAdapter.showRestrictionView(mapTypeId, param, position);
    }
    //避开道路
    public void setAvoidRoad(RouteAvoidInfo routeAvoidInfo) {
        mRouteAdapter.setAvoidRoad(routeAvoidInfo);
    }
    //提供给语音--一次性传入途经点和终点
    public long requestRouteFromSpeech(RouteSpeechRequestParam param){
        removeAllRouteInfo(param.getMapTypeId());
        if (!ConvertUtils.isEmpty(param.getStartPoiInfoEntity())) {
            RouteParam start = getRouteParam(param.getStartPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_START);
            startRouteParams.put(param.getMapTypeId(), start);
        }

        if (!ConvertUtils.isEmpty(param.getViaPoiInfoEntityList()) && param.getViaPoiInfoEntityList().size() > 0) {
            List<RouteParam> via = new ArrayList<>();
            for (PoiInfoEntity info : param.getViaPoiInfoEntityList()) {
                via.add(getRouteParam(info, RoutePoiType.ROUTE_POI_TYPE_WAY));
            }
            viaRouteParams.put(param.getMapTypeId(), via);
        }

        if (!ConvertUtils.isEmpty(param.getEndPoiInfoEntity())) {
            RouteParam end = getRouteParam(param.getEndPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_END);
            endRouteParams.put(param.getMapTypeId(), end);
        }
        return requestRoute(param.getMapTypeId(), null, -1, false, RouteWayID.ROUTE_WAY_DEFAULT);
    }

    public long requestManyVia(MapTypeId mapTypeId, List<RouteParam> routeParams){
        viaRouteParams.put(mapTypeId, routeParams);
        return requestRoute(mapTypeId, null, -1, false, RouteWayID.ROUTE_WAY_DEFAULT);
    }

    public long requestChangeEnd(MapTypeId mapTypeId, PoiInfoEntity poiInfoEntity){
        endRouteParams.put(mapTypeId, getRouteParam(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_END));
        return requestRoute(mapTypeId, null, -1, false, RouteWayID.ROUTE_WAY_DEFAULT);
    }

    //提供给语音--获取途经点个数
    public int getViaPointsCount(MapTypeId mapTypeId) {
        return viaRouteParams.get(mapTypeId).size();
    }
    //提供给语音--获取两个点的距离和时间
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(GeoPoint geoPointStart, GeoPoint geoPointend) {
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(geoPointStart)
                .poiLoc(geoPointend)
                .build();

        return SearchAdapter.getInstance().getTravelTimeFuture(requestParameterBuilder);
    }

    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(GeoPoint geoPointStart, GeoPoint geoPointend) {
        SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(geoPointStart)
                .poiLoc(geoPointend)
                .build();

        return SearchAdapter.getInstance().getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }

    //限行数据回调
    @Override
    public void onDrawRestrictionAndDetails(RouteRestrictionParam param) {
        Logger.i(TAG, "onDrawRestrictionAndDetails");
        if (ConvertUtils.isEmpty(mRequestRouteResults)) {
            Logger.i(TAG, "mRequestRouteResults is null");
            return;
        }
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(MapTypeId.MAIN_SCREEN_MAIN_MAP))) {
            Logger.i(TAG, "MAIN_SCREEN_MAIN_MAP have no data");
            return;
        }
        RouteRestrictionParam routeRestrictionParam = mRequestRouteResults.get(MapTypeId.MAIN_SCREEN_MAIN_MAP).getRouteRestrictionParam();
        routeRestrictionParam.setGReStrictedAreaResponseParam(param.getGReStrictedAreaResponseParam());
        routeRestrictionParam.setRestrictedArea(param.getRestrictedArea());
        if (ConvertUtils.isEmpty(routeResultObserverMap)) return;
        for (IRouteResultObserver routeResultObserver : routeResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) continue;
            routeResultObserver.onRouteDrawReStrictedAreaInfo(routeRestrictionParam);
        }
    }

    private static final class Helper {
        private static final RoutePackage ep = new RoutePackage();
    }
}
