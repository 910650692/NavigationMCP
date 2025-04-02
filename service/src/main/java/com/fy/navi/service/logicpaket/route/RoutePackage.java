package com.fy.navi.service.logicpaket.route;

import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.aos.QueryRestrictedObserver;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
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
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
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
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTMCParam;
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
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;

import lombok.Getter;

/**
 * @author lvww
 * @version  \$Revision.1.0\$
 * date 2024/11/24
 * Description TODO
 */
final public class RoutePackage implements RouteResultObserver, QueryRestrictedObserver, ILayerAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private SignalPackage mSignalPackage;
    private CalibrationPackage mCalibrationPackage;
    private ConcurrentHashMap<String, IRouteResultObserver> mRouteResultObserverMap;
    private Map<MapType, RouteParam> mStartRouteParams;
    private Map<MapType, List<RouteParam>> mViaRouteParams;
    private Map<MapType, List<RouteParam>> mSaveViaRouteParams;
    private Map<MapType, RouteParam> mEndRouteParams;
    private Map<MapType, RouteParam> mSaveEndRouteParams;
    private Map<MapType, RequestRouteResult> mRequestRouteResults;
    private ArrayList<EvRangeOnRouteInfo> mEvRangeOnRouteInfos;
    private RouteAdapter mRouteAdapter;
    private NavistatusAdapter mNaviStatusAdapter;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private PositionAdapter mPositionAdapter;
    private MapAdapter mMapAdapter;
    private BlAosAdapter mBlAosAdapter;

    private EngineAdapter mEngineAdapter;

    private int[] mOfflineRouteErrorCode = {822083585, 822083587, 822083584, 822083590,
            822083592, 822083593, 822083594, 822083595, 822083596, 822083599, 822083600, 822083602};

    /**
     * 获取报错信息是否需要离线
     * @param errorCode 错误码
     * @return 返回是否
     */
    public boolean isNeedAutoOfflineRoute(final int errorCode) {
        for (int t = 0; t < mOfflineRouteErrorCode.length; t++) {
            if (errorCode == mOfflineRouteErrorCode[t]) {
                return true;
            }
        }
        return false;
    }

    @Getter
    private Map<MapType, Integer> mSelectRouteIndex;
    private Map<MapType, Long> mRequestId;

    public Map<MapType, Integer> getSelectRouteIndex() {
        return mSelectRouteIndex;
    }

    private RoutePackage() {
        mRouteResultObserverMap = new ConcurrentHashMap<>();
        mStartRouteParams = new HashMap<>();
        mViaRouteParams = new HashMap<>();
        mEndRouteParams = new HashMap<>();
        mSaveViaRouteParams = new HashMap<>();
        mSaveEndRouteParams = new HashMap<>();
        mSelectRouteIndex = new HashMap<>();
        mRequestId = new HashMap<>();

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

    /**
     * 初始化算路服务
     */
    public void initRouteService() {
        mRouteAdapter.initRouteService();
        mRouteAdapter.registerRouteObserver(RouteAdapter.REGISTRE_FROM_ROUTE, this);
        mBlAosAdapter.addRestrictedObserver("routePackage", this);
    }

    /**
     * 注册算路回调监听
     * @param key key
     * @param observer 回调监听对象
     */
    public void registerRouteObserver(final String key, final IRouteResultObserver observer) {
        mRouteResultObserverMap.put(key, observer);
    }

    /**
     * 解注册算路回调监听
     * @param key key
     */
    public void unRegisterRouteObserver(final String key) {
        mRouteResultObserverMap.remove(key);
    }

    /**
     * 反初始化路线服务
     */
    public void unInitNaviService() {
        mRouteAdapter.unInitRouteService();
    }

    public static RoutePackage getInstance() {
        return Helper.EP;
    }

    @Override
    public void onRouteSuccess(final String successMsg) {
        Logger.i(TAG, "onRouteSuccess");
        for (String key : mRouteResultObserverMap.keySet()) {
            final IRouteResultObserver routeResultObserver = mRouteResultObserverMap.get(key);
            if (null == routeResultObserver) {
                mRouteResultObserverMap.remove(key);
            } else {
                routeResultObserver.onRouteSuccess(successMsg);
            }
        }
    }

    @Override
    public void onRouteResult(final RequestRouteResult requestRouteResult) {
        Logger.i(TAG, "onRouteResult");
        mRequestRouteResults.put(requestRouteResult.getMMapTypeId(), requestRouteResult);
        updateParamList(requestRouteResult.getMMapTypeId());
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.SELECT_ROUTE);
        }
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteResult(requestRouteResult);
        }
        callBackToSpeech(requestRouteResult.getMMapTypeId());
        checkDestinationAndStartBatterEnable(requestRouteResult.getMMapTypeId());
    }

    /**
     * 检查终点/下个途经点是否是充电站，开启预加热
     * @param mapTypeId 屏幕id
     */
    private void checkDestinationAndStartBatterEnable(final MapType mapTypeId) {
        PoiInfoEntity poiInfo = null;
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        if (!ConvertUtils.isEmpty(routeParams)) {
            poiInfo  = routeParams.get(0).getMPoiInfoEntity();

        } else {
            final RouteParam routeParam = mEndRouteParams.get(mapTypeId);
            if (!ConvertUtils.isEmpty(routeParam)) {
                poiInfo = routeParam.getMPoiInfoEntity();
            }
        }

        if (!ConvertUtils.isEmpty(poiInfo)
                && !ConvertUtils.isEmpty(poiInfo.getPointTypeCode())
                && AutoMapConstant.PointTypeCode.CHARGING_STATION ==
                SearchPackage.getInstance().getPointTypeCode(poiInfo.getPointTypeCode())) {
            //todo 发送信号给电池预加热
        } else {
            //todo 发送信号给电池预加热
        }
    }

    /**
     * 返回数据
     * @param mapTypeId 屏幕id
     */
    private void callBackToSpeech(final MapType mapTypeId) {
        final int size = mViaRouteParams.get(mapTypeId).size();
        String cityName = "上海市";
        if (getEndPoint(mapTypeId).getAdCode() != 0 && !ConvertUtils.isEmpty(MapDataPackage.getInstance()
                .getCityInfo(getEndPoint(mapTypeId).getAdCode()))) {
            cityName = MapDataPackage.getInstance().getCityInfo(getEndPoint(mapTypeId).getAdCode()).getName();
        }
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onSpeechViaNum(size);
            routeResultObserver.onSpeechEndCityName(cityName);
        }
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine");
        final RequestRouteResult requestRouteResult = mRequestRouteResults.get(routeLineLayerParam.getMMapTypeId());
        if (!ConvertUtils.isEmpty(requestRouteResult)){
            if (requestRouteResult.isMFastNavi()) {
                mNaviAdapter.updateNaviPath(NumberUtils.NUM_0, requestRouteResult.getMLineLayerParam());
            }
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteDrawLine(routeLineLayerParam);
        }
    }

    @Override
    public void onRouteRestAreaInfo(final RouteRestAreaParam routeRestAreaParam) {
        Logger.i(TAG, "onRouteRestAreaInfo");
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteRestAreaInfo(routeRestAreaParam);
        }
    }

    @Override
    public void onRouteWeatherInfo(final RouteWeatherParam routeWeatherParam) {
        Logger.i(TAG, "onRouteWeatherInfo");
        if (ConvertUtils.isEmpty(routeWeatherParam)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteWeatherInfo(null);
            }
            return;
        }
        showWeatherView(routeWeatherParam.getMMapTypeId());
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteWeatherInfo(routeWeatherParam);
        }
    }

    @Override
    public void onRouteChargeStationInfo(final RouteChargeStationParam routeChargeStationParam) {
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteChargeStationInfo(routeChargeStationParam);
        }
    }

    @Override
    public void onRouteAlterChargeStationInfo(final RouteAlterChargeStationParam routeAlterChargeStationParam) {
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteAlterChargeStationInfo(routeAlterChargeStationParam);
        }
    }

    @Override
    public void onRouteRestrictionInfo(final RouteRestrictionParam routeRestrictionParam) {
        Logger.i(TAG, "onRouteRestrictionInfo");
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteRestrictionInfo(routeRestrictionParam);
        }
    }

    @Override
    public void onRouteRestTollGateInfo(final RouteRestTollGateParam routeRestTollGateParam) {
        Logger.i(TAG, "onRouteRestTollGateInfo");
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteRestTollGateInfo(routeRestTollGateParam);
        }
    }

    @Override
    public void onRouteCityInfo(final RouteAlongCityParam routeAlongCityParam) {
        Logger.i(TAG, "onRouteCityInfo");
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteCityInfo(routeAlongCityParam);
        }
    }

    @Override
    public void onRouteTrafficIncidentInfo(final RouteTrafficIncidentParam routeTrafficIncidentParam) {
        Logger.i(TAG, "onRouteTrafficIncidentInfo");
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteTrafficIncidentInfo(routeTrafficIncidentParam);
        }
    }

    @Override
    public void onRouteRanges(final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
        Logger.i(TAG, "onRouteRanges");
        mEvRangeOnRouteInfos = evRangeOnRouteInfos;
        if (ConvertUtils.isEmpty(mRouteResultObserverMap))  {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteRanges(evRangeOnRouteInfos);
        }
    }

    @Override
    public void onRouteFail(final RequestRouteResult requestRouteResult, final int errorCode, final String errorMsg) {
        Logger.i(TAG, "onRouteFail");
        if (ConvertUtils.isEmpty(requestRouteResult)) {
            return;
        }
        if (isNeedAutoOfflineRoute(errorCode) && requestRouteResult.isMIsOnlineRoute()) {
            final RouteRequestParam param = new RouteRequestParam();
            param.setMMapTypeId(requestRouteResult.getMMapTypeId());
            param.setMFastNavi(requestRouteResult.isMFastNavi());
            param.setMRouteWay(requestRouteResult.getMRouteWay());
            param.setMIsOnline(false);
            requestRoute(param);
            return;
        }
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        reGetParamList(requestRouteResult.getMMapTypeId());
        callBackFailMsg(requestRouteResult.getMMapTypeId(), errorMsg);
    }

    /**
     * 回调所有点的信息
     * @param mapTypeId 屏幕id
     * @param routeParams 点信息
     */
    private void savePointAndCallBack(final MapType mapTypeId, final List<RouteParam> routeParams) {
        if (ConvertUtils.isEmpty(mRequestRouteResults)) {
            return;
        }
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        mRequestRouteResults.get(mapTypeId).setMRouteParams(routeParams);
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteAllRoutePoiInfo(mRequestRouteResults.get(mapTypeId));
        }
    }

    @Override
    public void onRouteL2Info(final String json) {
        l2DatacallBack(json);
    }

    @Override
    public void onRouteTMCInfo(RouteTMCParam param) {
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteTMCInfo(param);
        }
    }

    /**
     * 路线上充电站数据回调    、
     * @param json 路线信息
     */
    private void l2DatacallBack(final String json) {
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onL2DataCallBack(json);
            }
        }
    }

    //限行数据回调
    @Override
    public void onDrawRestrictionAndDetails(final RouteRestrictionParam param) {
        Logger.i(TAG, "onDrawRestrictionAndDetails");
        if (ConvertUtils.isEmpty(mRequestRouteResults)) {
            Logger.i(TAG, "mRequestRouteResults is null");
            return;
        }
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(MapType.MAIN_SCREEN_MAIN_MAP))) {
            Logger.i(TAG, "MAIN_SCREEN_MAIN_MAP have no data");
            return;
        }
        final RouteRestrictionParam routeRestrictionParam = mRequestRouteResults.get(MapType.MAIN_SCREEN_MAIN_MAP).getMRouteRestrictionParam();
        routeRestrictionParam.setMReStrictedAreaResponseParam(param.getMReStrictedAreaResponseParam());
        routeRestrictionParam.setMRestrictedArea(param.getMRestrictedArea());
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteDrawReStrictedAreaInfo(routeRestrictionParam);
        }
    }

    //---------------主动方法-------------------------------
    /**
     * 基本算路请求
     *
     * @param param 算路请求参数
     * @return 返回请求taskId
     */
    public long requestRoute(final RouteRequestParam param) {
        if (ConvertUtils.isEmpty(param)) {
            return NumberUtils.NUM_ERROR;
        }
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteRequest();
            }
        }
        final RouteParam routeParam = getRouteParamFromPoiInfoEntity(param.getMPoiInfoEntity(), param.getMRoutePoiType());
        final List<RouteParam> paramList = getParamList(param.getMMapTypeId(), routeParam);
        if (ConvertUtils.isEmpty(paramList) || paramList.size() < 2 ) {
            callBackFailMsg(param.getMMapTypeId(), "点参数异常");
            return NumberUtils.NUM_ERROR;
        }
        final RoutePreferenceID perfrenceId;
        if (ConvertUtils.isEmpty(param.getMRoutePreferenceID())) {
            perfrenceId = SettingPackage.getInstance().getRoutePreference();
        } else {
            perfrenceId = param.getMRoutePreferenceID();
        }
        final String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        final String mVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        initBevCarData();
        mRouteAdapter.setRequestControl(perfrenceId, platNum, "true".equals(mVoidLimit), mNaviStatusAdapter.isGuidanceActive());
        mSelectRouteIndex.put(param.getMMapTypeId(), NumberUtils.NUM_ERROR);
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        }
        final long requestId = mRouteAdapter.requestRoute(param, paramList);
        mRequestId.put(param.getMMapTypeId(), requestId);
        return requestId;
    }
    /**
     * 第一次传入多个途经点算路请求
     *
     * @param mapTypeId 屏幕ID
     * @param routeParams 途经点列表
     * @return 返回请求的taskId
     */
    public long requestManyVia(final MapType mapTypeId, final List<RouteParam> routeParams){
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam routeRequestParam = new RouteRequestParam();;
        routeRequestParam.setMMapTypeId(mapTypeId);
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_ALL_VIA);
        routeRequestParam.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        return requestRoute(routeRequestParam);
    }
    /**
     * 修改终点算路请求
     *
     * @param mapTypeId 屏幕ID
     * @param poiInfoEntity 终点数据
     * @return 返回请求taskId
     */
    public long requestChangeEnd(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity){
        mEndRouteParams.put(mapTypeId, getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_END));
        final RouteRequestParam routeRequestParam = new RouteRequestParam();;
        routeRequestParam.setMMapTypeId(mapTypeId);
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_END);
        routeRequestParam.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_VOICE_CHANGE_DEST);
        return requestRoute(routeRequestParam);
    }
    /**
     * 单个添加途经点算路请求
     *
     * @param mapTypeId 屏幕ID
     * @param poiInfoEntity 途径点数据
     */
    public void addViaPoint(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        if (isBelongRouteParam(mapTypeId, poiInfoEntity)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点已经在路线上");
            return;
        }
        if (isMaxRouteParam(mapTypeId)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点超过最大限制");
            return;
        }
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        routeParams.add(getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mapTypeId);
        param.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        param.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRoute(param);
    }

    /**
     * 单个添加途经点带索引算路请求
     *
     * @param mapTypeId 屏幕ID
     * @param poiInfoEntity 途径点数据
     * @param index 添加索引
     */
    public void addViaPoint(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity, final int index) {
        if (isBelongRouteParam(mapTypeId, poiInfoEntity)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点已经在路线上");
            return;
        }
        if (isMaxRouteParam(mapTypeId)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点超过最大限制");
            return;
        }
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        if (routeParams == null || routeParams.size() <= index) {
            callBackFailMsg(mapTypeId, "途经点添加失败：下标越界");
            return;
        }
        routeParams.add(index, getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mapTypeId);
        param.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        param.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRoute(param);
    }

    /**
     * 单个删除添加途经点算路请求
     *
     * @param mapTypeId 屏幕ID
     * @param poiInfoEntity 途径点数据
     * @param isRequestRoute 是否发起算路
     * @return 返回是否成功
     */
    public boolean removeVia(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity, final boolean isRequestRoute) {
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        if (routeParams.size() > 0) {
            int index = -1;
            for (int t = NumberUtils.NUM_0; t < routeParams.size(); t++) {
                if (poiInfoEntity.getPid() == routeParams.get(t).getPoiID()
                        || (poiInfoEntity.getPoint().getLon() == routeParams.get(t).getRealPos().getLon()
                        && poiInfoEntity.getPoint().getLat() == routeParams.get(t).getRealPos().getLat())) {
                    index = t;
                }
            }
            if (index == -1) {
                Logger.i(TAG, "have no this point");
                return false;
            }
            routeParams.remove(index);
            mViaRouteParams.put(mapTypeId, routeParams);
            if (isRequestRoute) {
                final RouteRequestParam param = new RouteRequestParam();
                param.setMMapTypeId(mapTypeId);
                param.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
                param.setMRouteWay(RouteWayID.ROUTE_WAY_DELETE_VIA);
                param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
                requestRoute(param);
            } else {
                mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
            }
            return true;
        }
        return false;
    }
    /**
     * send2car 算路请求
     *
     * @param routeMsgPushInfo 算路请求参数
     * @param mapTypeId 屏幕ID
     */
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo, final MapType mapTypeId) {
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteRequest();
            }
        }
        final RouteParam routeParam = getRouteParamFromPoiInfoEntity(routeMsgPushInfo.getMPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_END);
        List<RouteParam> paramList = getParamList(mapTypeId, routeParam);
        if (routeMsgPushInfo.getMViaPoiInfoEntity() != null
                && !routeMsgPushInfo.getMViaPoiInfoEntity().isEmpty()) {
            clearVai(mapTypeId);
            for (PoiInfoEntity vaiPoiInfoEntity :routeMsgPushInfo.getMViaPoiInfoEntity()) {
                final RouteParam vaiParam = getRouteParamFromPoiInfoEntity(vaiPoiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                paramList = getParamList(mapTypeId, vaiParam);
            }
        } else {
            clearVai(mapTypeId);
        }
        if (ConvertUtils.isEmpty(paramList) || paramList.size() < 2) {
            callBackFailMsg(mapTypeId, "点参数异常");
            return ;
        }

        final RoutePreferenceID  perfrenceId = SettingPackage.getInstance().getRoutePreference();
        final String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        final String mVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        initBevCarData();
        mRouteAdapter.setRequestControl(perfrenceId, platNum, "true".equals(mVoidLimit), mNaviStatusAdapter.isGuidanceActive());
        mSelectRouteIndex.put(mapTypeId, NumberUtils.NUM_ERROR);
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        }
        mRouteAdapter.requestRouteRestoration(routeMsgPushInfo,mapTypeId);
    }
    /**
     * 请求路线上的天气
     *
     * @param mapTypeId 屏幕ID
     * @param index 路线ID
     * @return 返回请求的taskId
     */
    public long requestRouteWeather(final MapType mapTypeId, final int index) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return -1;
        }
        return mRouteAdapter.requestRouteWeather(mRequestRouteResults.get(mapTypeId).getMLineLayerParam(), index);
    }
    /**
     * 回调错误信息
     *
     * @param mapTypeId 屏幕ID
     * @param errorText 错误信息
     */
    public void callBackFailMsg(final MapType mapTypeId, final String errorText) {
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteFail(mapTypeId, errorText);
        }
    }
    /**
     * 请求备选充电站
     *
     * @param mapTypeId 屏幕ID
     * @param poiId 推荐充电站
     * @return 返回请求的taskId
     */
    public long requestRouteAlternativeChargeStation(final MapType mapTypeId, final String poiId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return -1;
        }
        final RouteChargeStationParam routeChargeStationParam =mRequestRouteResults.get(mapTypeId).getMRouteChargeStationParam();
        if (ConvertUtils.isEmpty(routeChargeStationParam)) {
            return -1;
        }
        if (ConvertUtils.isEmpty(routeChargeStationParam.getMPathInfoList())) {
            return -1;
        }
        return mRouteAdapter.requestRouteAlternativeChargeStation(routeChargeStationParam.getMPathInfoList()
                .get(mSelectRouteIndex.get(mapTypeId)) , poiId);
    }

    /**
     * 请求限行数据
     *
     * @param mapTypeId 屏幕ID
     * @return 返回请求的taskId
     */
    public long requestRestirction(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults) || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))
                || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId).getMRouteRestrictionParam())) {
            return -1;
        }
        final List<String> ruleIDs = mRequestRouteResults.get(mapTypeId).getMRouteRestrictionParam().getMRuleIds();
        if (ruleIDs.size() > mSelectRouteIndex.get(mapTypeId)
                && !ConvertUtils.isEmpty(ruleIDs.get(mSelectRouteIndex.get(mapTypeId)))) {
            final RestrictedParam restrictedParam = new RestrictedParam();
            restrictedParam.setRestrict_type(9);
            restrictedParam.setPlate(SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER));
            restrictedParam.setRuleids(ruleIDs.get(mSelectRouteIndex.get(mapTypeId)));
            return mBlAosAdapter.queryRestrictedInfo(restrictedParam);
        }
        return -1;
    }
    /**
     * 电车请求算路的时候，拿一次能耗模型
     */
    private void initBevCarData() {
        if (Boolean.FALSE.equals(BevPowerCarUtils.getInstance().isElecPlanRoute)) {
            return;
        }
        if (mSignalPackage.getMaxBatteryEnergy() > 0) {
            BevPowerCarUtils.getInstance().maxBattenergy = mSignalPackage.getMaxBatteryEnergy();
        }
        if (mCalibrationPackage.vehicleWeight() > 0) {
            BevPowerCarUtils.getInstance().vehicleWeight = (short) mCalibrationPackage.vehicleWeight();
        }
        if (!ConvertUtils.isEmpty(mCalibrationPackage.brandName())) {
            BevPowerCarUtils.getInstance().extraBrand = mCalibrationPackage.brandName();
        }
        if (!ConvertUtils.isEmpty(mCalibrationPackage.modelName())) {
            BevPowerCarUtils.getInstance().vehicleType = mCalibrationPackage.modelName();
        }
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

    /**
     * 数据转化
     * @param poiInfoEntity 点信息
     * @param poiType 点的类别
     * @return 转换后数据
     */
    public RouteParam getRouteParamFromPoiInfoEntity(final PoiInfoEntity poiInfoEntity,  final int poiType) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return null;
        }
        if (ConvertUtils.isEmpty(poiInfoEntity.getPoint())) {
            return null;
        }
        if (poiType == RoutePoiType.ROUTE_POI_TYPE_END) {
            mRouteAdapter.sendEndEntity(poiInfoEntity);
        }
        final RouteParam routeParam = new RouteParam();
        routeParam.setName(poiInfoEntity.getName());
        routeParam.setAddress(poiInfoEntity.getAddress());
        routeParam.setPoiType(poiType);
        routeParam.setPoiID(poiInfoEntity.getPid());
        routeParam.setAdCode(poiInfoEntity.getAdCode());
        if (!ConvertUtils.isEmpty(poiInfoEntity.getCityInfo())) {
            routeParam.setAdCode(poiInfoEntity.getCityInfo().getCityCode());
        }
        final GeoPoint geoPoint = new GeoPoint();
        geoPoint.setLon(poiInfoEntity.getPoint().getLon());
        geoPoint.setLat(poiInfoEntity.getPoint().getLat());
        routeParam.setRealPos(geoPoint);
        routeParam.setMPoiInfoEntity(poiInfoEntity);
        return routeParam;
    }

    /**
     * 清空途经点
     * @param mapTypeId 屏幕id
     */
    private void clearVai(final MapType mapTypeId) {
        mViaRouteParams.put(mapTypeId, new ArrayList<>());
    }

    /**
     * 判断是否是路线上的点
     * @param mapTypeId 屏幕Id
     * @param poiInfoEntity 点信息
     * @return 返回是否
     */
    public boolean isBelongRouteParam(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        final List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 是否是起点&终点
     * @param mapTypeId 屏幕Id
     * @param poiInfoEntity 点信息
     * @return 返回是否是起点&终点
     */
    public boolean isStartOrEndRouteParam(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        final List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_START || routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_END) {
                    return true;
                }
            }
        }
        return false;
    }

    /**
     * 是否是同一个点
     * @param routeParam 点信息
     * @param poiInfoEntity 点信息
     * @return 返回是否是起点&终点
     */
    public boolean isTheSamePoi(final RouteParam routeParam, final PoiInfoEntity poiInfoEntity) {
        if (routeParam.getPoiID() == poiInfoEntity.getPid() || (routeParam.getRealPos().getLat() == poiInfoEntity.getPoint().getLat()
                && routeParam.getRealPos().getLon() == poiInfoEntity.getPoint().getLon())) {
            return true;
        }
        return false;
    }

    /**
     * 是否超过最大点的数量
     * @param mapTypeId 屏幕Id
     * @return 返回是否是起点&终点
     */
    public boolean isMaxRouteParam(final MapType mapTypeId) {
        final List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        if (allPoiParamList.size() < 7) {
            return false;
        }
        return true;
    }
    /**
     * 绘制路线
     *
     * @param mapTypeId 屏幕ID
     */
    public void showRouteLine(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        final ArrayList<String> arrivalTimes = new ArrayList<>();
        final List<RouteLineInfo> routeLineInfos = mRequestRouteResults.get(mapTypeId).getMRouteLineInfos();
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getMLineLayerParam();
        for (RouteLineInfo routeLineInfo : routeLineInfos) {
            arrivalTimes.add(routeLineInfo.getMTravelTime());
        }
        routeLineLayerParam.setMEstimatedTimeOfArrival(arrivalTimes);
        mLayerAdapter.drawRouteLine(mapTypeId,routeLineLayerParam);
    }

    /**
     * 清除路线
     * @param mapTypeId 屏幕ID
     */
    public void clearRouteLine(final MapType mapTypeId) {
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        mLayerAdapter.clearRouteLine(mapTypeId);
        removeAllRouteInfo(mapTypeId);
    }

    /**
     * 算路页面路线全览
     * @param mapTypeId 屏幕ID
     */
    public void showPreview(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getMLineLayerParam();
        final PreviewParams previewParams = mLayerAdapter.getPathResultBound(mapTypeId, routeLineLayerParam.getMPathInfoList());
        if (ConvertUtils.isEmpty(previewParams)) {
            Logger.e(TAG, "previewParams is null");
            return;
        }
        previewParams.setScreenLeft(1350);
        previewParams.setScreenRight(500);
        previewParams.setScreenTop(170);
        previewParams.setScreenBottom(20);
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }
    /**
     * 导航页面路线全览
     *
     * @param mapTypeId 屏幕ID
     */
    public void naviShowPreview(final MapType mapTypeId) {
        Logger.i(TAG, "naviShowPreview mapTypeId = " + mapTypeId);
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getMLineLayerParam();
        final PreviewParams previewParams = mLayerAdapter.getPathResultBound(mapTypeId, routeLineLayerParam.getMPathInfoList());
        if (ConvertUtils.isEmpty(previewParams)) {
            Logger.e(TAG, "previewParams is null");
            return;
        }
        previewParams.setScreenLeft(1350);
        previewParams.setScreenRight(500);
        previewParams.setScreenTop(170);
        previewParams.setScreenBottom(20);
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }

    /**
     * 切换路线
     *
     * @param mapTypeId 屏幕ID
     * @param routeIndex 路线id
     */
    public void selectRoute(final MapType mapTypeId, final int routeIndex) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        mLayerAdapter.setSelectedPathIndex(mapTypeId, routeIndex);
        mSelectRouteIndex.put(mapTypeId, routeIndex);
        if (mRequestRouteResults.get(mapTypeId) != null) {
            mNaviAdapter.setNaviPath(routeIndex, mRequestRouteResults.get(mapTypeId).getMLineLayerParam());
        }
        final RouteCurrentPathParam routeCurrentPathParam = mRequestRouteResults.get(mapTypeId).getMRouteCurrentPathParam();
        routeCurrentPathParam.setMMapTypeId(mapTypeId);
        routeCurrentPathParam.setMRequestId(mRequestRouteResults.get(mapTypeId).getMRequestId());
        routeCurrentPathParam.setMPathInfo(mRequestRouteResults.get(mapTypeId).getMLineLayerParam().getMPathInfoList().get(routeIndex));
        mRouteAdapter.setCurrentPath(routeCurrentPathParam);
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteSlected(mapTypeId, routeIndex);
        }
    }

    /**
     * 展示路线服务区扎点
     *
     * @param mapTypeId 屏幕ID
     * @param index 路线
     */
    public void showRestArea(final MapType mapTypeId, final int index) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        mLayerAdapter.showRestArea(mapTypeId, mRequestRouteResults.get(mapTypeId).getMRouteRestAreaParam().getMPathInfoList() ,index);
    }

    /**
     * 清除路线服务区扎点
     * @param mapTypeId 屏幕ID
     */
    public void clearRestArea(final MapType mapTypeId) {
        mLayerAdapter.showRestArea(mapTypeId, null ,-1);
    }
    /**
     * 展示天气扎点
     *
     * @param mapTypeId 屏幕ID
     */
    public void showWeatherView(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        mLayerAdapter.showWeatherView(mapTypeId, mRequestRouteResults.get(mapTypeId).getMRouteWeatherParam().getMWeatherLabelItem());
    }
    /**
     * 清除天气扎点
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearWeatherView(final MapType mapTypeId) {
        mLayerAdapter.showWeatherView(mapTypeId, null);
    }
    /**
     * 展示限行图层
     *
     * @param mapTypeId 屏幕ID
     * @param object 限行数据
     */
    public void showRestrictionView(final MapType mapTypeId, final Object object) {
        mLayerAdapter.showRestrictionView(mapTypeId, object);
    }
    /**
     * 清除限行图层
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearRestrictionView(final MapType mapTypeId) {
        mLayerAdapter.showRestrictionView(mapTypeId, null);
    }
    /**
     * 途经点全量更新
     *
     * @param mapTypeId 屏幕ID
     * @param routeParams 途径点信息
     */
    public void updateViaParamList(final MapType mapTypeId, final List<RouteParam> routeParams) {
        mViaRouteParams.put(mapTypeId, routeParams);
    }
    /**
     * 获取所有点的信息
     *
     * @param mapTypeId 屏幕ID
     * @param routeParam 传入的点
     * @return 返回所有点的信息
     */
    private List<RouteParam> getParamList(final MapType mapTypeId, final RouteParam routeParam) {
        mStartRouteParams.put(mapTypeId, getLocationParam());
        if (ConvertUtils.isEmpty(mViaRouteParams.get(mapTypeId))) {
            mViaRouteParams.put(mapTypeId, new ArrayList<>());
        }
        final List<RouteParam> routeParams = new ArrayList<>();
        if (ConvertUtils.isEmpty(routeParam)) {
            Logger.i(TAG, "have no point add");
            routeParams.add(mStartRouteParams.get(mapTypeId));
            if (mViaRouteParams.get(mapTypeId).size() != NumberUtils.NUM_0) {
                routeParams.addAll(mViaRouteParams.get(mapTypeId));
            }
            if (!ConvertUtils.isEmpty(mEndRouteParams.get(mapTypeId))) {
                routeParams.add(mEndRouteParams.get(mapTypeId));
            }
            return routeParams;
        }
        if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_WAY) {
            Logger.i(TAG, "have via point add");
            final List<RouteParam> viaParams = mViaRouteParams.get(mapTypeId);
            viaParams.add(routeParam);
            mViaRouteParams.put(mapTypeId, viaParams);
            routeParams.add(mStartRouteParams.get(mapTypeId));
            routeParams.addAll(mViaRouteParams.get(mapTypeId));
            routeParams.add(mEndRouteParams.get(mapTypeId));
            return routeParams;
        } else if (routeParam.getPoiType() == RoutePoiType.ROUTE_POI_TYPE_END) {
            Logger.i(TAG, "have end point add");
            mEndRouteParams.put(mapTypeId, routeParam);
            routeParams.add(mStartRouteParams.get(mapTypeId));
            if (mViaRouteParams.get(mapTypeId).size() != NumberUtils.NUM_0) {
                routeParams.addAll(mViaRouteParams.get(mapTypeId));
            }
            routeParams.add(mEndRouteParams.get(mapTypeId));
            return routeParams;
        }
        return routeParams;
    }
    /**
     * 把定位点当成起点
     * @return 返回点信息
     */
    private RouteParam getLocationParam() {
        final RouteParam startParam = new RouteParam();
        final GeoPoint startCoordinate = new GeoPoint();
        startCoordinate.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        startCoordinate.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        startParam.setRealPos(startCoordinate);
        startParam.setPoiType(RoutePoiType.ROUTE_POI_TYPE_START);
        startParam.setRoadID(mPositionAdapter.getLastCarLocation().getRoadId());
        return startParam;
    }

    /**
     * 获取路线上所有的点
     * @param mapTypeId 屏幕Id
     * @return 返回所有点信心
     */
    public List<RouteParam> getAllPoiParamList(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return new ArrayList<>();
        }
        return mRouteAdapter.getAllPoiParamList(mapTypeId);
    }
    /**
     * 算路成功保存所有点
     * @param mapTypeId 屏幕Id
     */
    private void updateParamList(final MapType mapTypeId) {
        final List<RouteParam> routeParamList = mViaRouteParams.get(mapTypeId);
        final List<RouteParam> routeParamVias = new ArrayList<>();
        routeParamVias.addAll(routeParamList);
        mSaveViaRouteParams.put(mapTypeId, routeParamVias);
        mSaveEndRouteParams.put(mapTypeId, mEndRouteParams.get(mapTypeId));
        mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
        savePointAndCallBack(mapTypeId, getCurrentParamsList(mapTypeId));
    }
    /**
     * 算路失败恢复原始点
     * @param mapTypeId 屏幕Id
     */
    private void reGetParamList(final MapType mapTypeId) {
        if (!ConvertUtils.isEmpty(mSaveViaRouteParams)) {
            final List<RouteParam> routeParamList = mSaveViaRouteParams.get(mapTypeId);
            final List<RouteParam> routeParamVias = new ArrayList<>();
            routeParamVias.addAll(routeParamList);
            mViaRouteParams.put(mapTypeId, routeParamVias);
        }
        if (!ConvertUtils.isEmpty(mSaveEndRouteParams)) {
            mEndRouteParams.put(mapTypeId, mSaveEndRouteParams.get(mapTypeId));
        }
        mRouteAdapter.saveAllPoiParamList(mapTypeId, getCurrentParamsList(mapTypeId));
        savePointAndCallBack(mapTypeId, getCurrentParamsList(mapTypeId));
    }

    /**
     * 获取当前算路数据
     * @param mapTypeId 屏幕Id
     * @return 当前算路数据
     */
    private List<RouteParam> getCurrentParamsList(final MapType mapTypeId) {
        final List<RouteParam> params = new ArrayList<>();
        if (!ConvertUtils.isEmpty(mStartRouteParams.get(mapTypeId))) {
            params.add(mStartRouteParams.get(mapTypeId));
        }

        if (!ConvertUtils.isEmpty(mViaRouteParams.get(mapTypeId))) {
            params.addAll(mViaRouteParams.get(mapTypeId));
        }

        if (!ConvertUtils.isEmpty(mEndRouteParams.get(mapTypeId))) {
            params.add(mEndRouteParams.get(mapTypeId));
        }
        return params;
    }
    /**
     * 路线结束
     * @param mapTypeId 屏幕Id
     */
    public void removeAllRouteInfo(final MapType mapTypeId) {
        mRequestRouteResults.put(mapTypeId, null);
        mStartRouteParams.put(mapTypeId, null);
        mViaRouteParams.put(mapTypeId, new ArrayList<>());
        mEndRouteParams.put(mapTypeId, null);
        mSaveViaRouteParams.put(mapTypeId, new ArrayList<>());
        mSaveEndRouteParams.put(mapTypeId, null);
    }
    /**
     * 获取终点信息
     * @param mapTypeId 屏幕Id
     * @return 返回终点信息
     */
    public RouteParam getEndPoint(final MapType mapTypeId) {
        return mEndRouteParams.get(mapTypeId);
    }
    /**
     * 获取当前路线信息
     * @param mapTypeId 屏幕Id
     * @return 算路信息
     */
    public RouteCurrentPathParam getCurrentPathInfo(final MapType mapTypeId) {
        return mRouteAdapter.getCurrentPath(mapTypeId);
    }

    /**
     * 获取能量耗尽点信息
     * @return 返回能耗信息
     */
    public ArrayList<EvRangeOnRouteInfo> getEvRangeOnRouteInfos() {
        return mEvRangeOnRouteInfos;
    }

    /**
     * 绘制限行区域
     * @param mapTypeId 屏幕Id
     * @param param 绘制参数
     * @param position index
     */
    public void drawRestrictionForLimit(final MapType mapTypeId, final Object param, final int position) {
        mLayerAdapter.showRestrictionView(mapTypeId, null);
        mLayerAdapter.showRestrictionView(mapTypeId, param, position);
    }

    /**
     * 设置避开道路
     * @param routeAvoidInfo 避开参数
     */
    public void setAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mRouteAdapter.setAvoidRoad(routeAvoidInfo);
    }

    /**
     * 语音请求算路
     * @param param 算路参数
     * @return taskId
     */
    public long requestRouteFromSpeech(final RouteSpeechRequestParam param){
        removeAllRouteInfo(param.getMMapTypeId());
        if (!ConvertUtils.isEmpty(param.getMStartPoiInfoEntity())) {
            final RouteParam start = getRouteParamFromPoiInfoEntity(param.getMStartPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_START);
            mStartRouteParams.put(param.getMMapTypeId(), start);
        }

        if (!ConvertUtils.isEmpty(param.getMViaPoiInfoEntityList()) && param.getMViaPoiInfoEntityList().size() > 0) {
            final List<RouteParam> via = new ArrayList<>();
            for (PoiInfoEntity info : param.getMViaPoiInfoEntityList()) {
                via.add(getRouteParamFromPoiInfoEntity(info, RoutePoiType.ROUTE_POI_TYPE_WAY));
            }
            mViaRouteParams.put(param.getMMapTypeId(), via);
        }

        if (!ConvertUtils.isEmpty(param.getMEndPoiInfoEntity())) {
            final RouteParam end = getRouteParamFromPoiInfoEntity(param.getMEndPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_END);
            mEndRouteParams.put(param.getMMapTypeId(), end);
        }

        final RouteRequestParam routeRequestParam = new RouteRequestParam();;
        routeRequestParam.setMMapTypeId(param.getMMapTypeId());
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_SPEECH);
        routeRequestParam.setMRoutePreferenceID(param.getMPreferenceID());
        return requestRoute(routeRequestParam);
    }

    /**
     * 提供给语音--获取途经点个数
     * @param mapTypeId 屏幕Id
     * @return 途径点个数
     */
    public int getViaPointsCount(final MapType mapTypeId) {
        return mViaRouteParams.get(mapTypeId).size();
    }

    /**
     * 提供给语音--获取两个点的距离和时间
     * @param geoPointStart 起点坐标
     * @param geoPointend 终点坐标
     * @return 距离和时间参数
     */
    public CompletableFuture<Pair<String, String>> getTravelTimeFuture(final GeoPoint geoPointStart, final GeoPoint geoPointend) {
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(geoPointStart)
                .poiLoc(geoPointend)
                .build();

        return SearchAdapter.getInstance().getTravelTimeFuture(requestParameterBuilder);
    }

    /**
     * 获取两个点的距离和时间和其他参数
     * @param geoPointStart 起点坐标
     * @param geoPointend 终点坐标
     * @return 距离和时间参数
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint geoPointStart, final GeoPoint geoPointend) {
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(geoPointStart)
                .poiLoc(geoPointend)
                .build();
        return SearchAdapter.getInstance().getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }
    /**
     * 取消算路
     * @param mapTypeId 屏幕ID
     */
    public void abortRequest(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestId)) {
            return;
        }
        mRouteAdapter.abortRequest(mRequestId.get(mapTypeId));
    }

    /**
     * 获取主图的通勤TMC
     * @param mapTypeId 屏幕ID
     */
    public void refreshHomeOfficeTMC(final MapType mapTypeId, final boolean isHome) {
        final PoiInfoEntity poiInfoEntity = BehaviorPackage.getInstance().getFavoriteHomeData(
                isHome ? AutoMapConstant.HomeCompanyType.HOME : AutoMapConstant.HomeCompanyType.COMPANY);
        if (ConvertUtils.isEmpty(poiInfoEntity) || ConvertUtils.isEmpty(poiInfoEntity.getPoint())) {
            Logger.i(TAG, "通勤预测没有数据");
            return;
        }
        final RouteParam endParam = getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_END);
        final List<RouteParam> routeParams = new ArrayList<>();
        routeParams.add(getLocationParam());
        routeParams.add(endParam);
        RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mapTypeId);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_COMMON);
        param.setRouteRequestCallBackType(isHome ? 0 : 1);
        mRouteAdapter.requestRoute(param, routeParams);
    }

    /**
     * 路线图层点击事件回调
     * @param pItem
     */
    @Override
    public void onRouteItemClick(GemLayerItem pItem) {

    }

    private static final class Helper {
        private static final RoutePackage EP = new RoutePackage();
    }
}
