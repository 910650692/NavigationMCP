package com.fy.navi.service.logicpaket.route;

import android.graphics.Rect;
import android.util.Pair;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.aos.QueryRestrictedObserver;
import com.fy.navi.service.adapter.calibration.CalibrationAdapter;
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
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteAvoidInfo;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteLineSegmentInfo;
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
import com.fy.navi.service.define.route.RouteSupplementParams;
import com.fy.navi.service.define.route.RouteSupplementInfo;
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
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import lombok.Getter;

/**
 * @author lvww
 * @version \$Revision.1.0\$
 * date 2024/11/24
 * Description TODO
 */
final public class RoutePackage implements RouteResultObserver, QueryRestrictedObserver {
    private static final String TAG = MapDefaultFinalTag.ROUTE_SERVICE_TAG;
    private SignalPackage mSignalPackage;
    private CalibrationPackage mCalibrationPackage;
    private ConcurrentHashMap<String, IRouteResultObserver> mRouteResultObserverMap;
    private Map<MapType, RouteParam> mStartRouteParams;
    private Map<MapType, List<RouteParam>> mViaRouteParams;
    private Map<MapType, List<RouteParam>> mSaveViaRouteParams;
    private Map<MapType, RouteParam> mEndRouteParams;
    private Map<MapType, RouteParam> mSaveEndRouteParams;
    private Map<MapType, PoiInfoEntity> mEndPoiEntity;
    private Map<MapType, RequestRouteResult> mRequestRouteResults;
    private ArrayList<EvRangeOnRouteInfo> mEvRangeOnRouteInfos;
    private RouteAdapter mRouteAdapter;
    private SearchAdapter mSearchAdapter;
    private NavistatusAdapter mNaviStatusAdapter;
    private NaviAdapter mNaviAdapter;
    private LayerAdapter mLayerAdapter;
    private PositionAdapter mPositionAdapter;
    private MapPackage mMapPackage;
    private BlAosAdapter mBlAosAdapter;

    private EngineAdapter mEngineAdapter;

    private int[] mOfflineRouteErrorCode = {822083585, 822083587, 822083584, 822083590,
            822083592, 822083593, 822083594, 822083595, 822083596, 822083599, 822083600, 822083602};
    private long mPathID = -1;

    /**
     * 获取报错信息是否需要离线
     *
     * @param errorCode 错误码
     * @return 返回是否
     */
    public boolean isNeedAutoOfflineRoute(final int errorCode) {
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            // 当前有网，不进行离线算路
            Logger.d(TAG, "Currently online");
            return false;
        }
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
        mEndPoiEntity = new HashMap<>();
        mSaveViaRouteParams = new HashMap<>();
        mSaveEndRouteParams = new HashMap<>();
        mSelectRouteIndex = new HashMap<>();
        mRequestId = new HashMap<>();

        mRequestRouteResults = new HashMap<>();
        mEvRangeOnRouteInfos = new ArrayList<>();
        mRouteAdapter = RouteAdapter.getInstance();
        mSearchAdapter = SearchAdapter.getInstance();
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mNaviAdapter = NaviAdapter.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        mPositionAdapter = PositionAdapter.getInstance();
        mMapPackage = MapPackage.getInstance();
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
     *
     * @param key      key
     * @param observer 回调监听对象
     */
    public void registerRouteObserver(final String key, final IRouteResultObserver observer) {
        mRouteResultObserverMap.put(key, observer);
    }

    /*清除指定路线类型扎标*/
    public void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type) {
        mLayerAdapter.clearRouteItemByType(mapTypeId, type);
    }

    /**
     * 解注册算路回调监听
     *
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
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteResult(requestRouteResult);
            }
            callBackToSpeech(requestRouteResult.getMMapTypeId());
        }
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.SELECT_ROUTE);
        }

        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mLayerAdapter.setCarLogoVisible(requestRouteResult.getMMapTypeId(), false);
        }
    }

    /**
     * 返回数据
     *
     * @param mapTypeId 屏幕id
     */
    private void callBackToSpeech(final MapType mapTypeId) {
        final int size = mViaRouteParams.get(mapTypeId).size();
        String cityName = "";
        String endName = "";
        if (!ConvertUtils.isEmpty(getEndPoint(mapTypeId)) && getEndPoint(mapTypeId).getName() != null) {
            endName = getEndPoint(mapTypeId).getName();
        }
        if (!ConvertUtils.isEmpty(getEndPoint(mapTypeId))
                && getEndPoint(mapTypeId).getAdCode() != 0
                && !ConvertUtils.isEmpty(MapDataPackage.getInstance().getCityInfo(getEndPoint(mapTypeId).getAdCode()))) {
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
            routeResultObserver.onSpeechEndCityName(cityName, endName);
        }
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine");
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteDrawLine(routeLineLayerParam);
        }
        //默认选择第一条路线
        selectDefaultRoute(routeLineLayerParam.getMMapTypeId());
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
        if (routeChargeStationParam == null) {
            return;
        }
        final ArrayList<RouteSupplementParams> routeSupplementParams = routeChargeStationParam.getMRouteSupplementParams();
        if (routeSupplementParams == null || routeSupplementParams.isEmpty()) {
            return;
        }
        final AtomicInteger infoTotal = new AtomicInteger(0);
        for (int i = 0; i < routeSupplementParams.size(); i++) {
            final ArrayList<RouteSupplementInfo> routeSupplementInfo = routeSupplementParams.get(i)
                    .getMRouteSupplementInfos();
            if (routeSupplementInfo != null && !routeSupplementInfo.isEmpty()) {
                final AtomicInteger paramTotal = new AtomicInteger(0);
                for (int j = 0; j< routeSupplementInfo.size(); j++) {
                    final int currentIndex = j;
                    getTravelTimeFutureIncludeChargeLeft(new GeoPoint(routeSupplementInfo.get(currentIndex).getMShow().getLon(),
                            routeSupplementInfo.get(currentIndex).getMShow().getLat()))
                            .thenAccept(etaInfo -> {
                                routeSupplementInfo.get(currentIndex).setMDistance(etaInfo.getDistance());
                                routeSupplementInfo.get(currentIndex).setMUnitDistance(TimeUtils.getInstance()
                                        .getDistanceString(etaInfo.getDistance()));
                                paramTotal.getAndIncrement();
                                if (paramTotal.get() == routeSupplementInfo.size()) {
                                    infoTotal.getAndIncrement();
                                    if (infoTotal.get() == routeSupplementParams.size()) {
                                        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                                            if (ConvertUtils.isEmpty(routeResultObserver)) {
                                                continue;
                                            }
                                            routeSupplementInfo.sort(Comparator.comparingInt(RouteSupplementInfo::getMDistance));
                                            routeResultObserver.onRouteChargeStationInfo(routeChargeStationParam);
                                        }
                                    }
                                }
                            })
                            .exceptionally(error -> {
                                Logger.d(TAG, "showChargeStationDetail error:" + error);
                                return null;
                            });
                }
            }
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
        mLayerAdapter.updateRouteReplaceChargePoints(MapType.MAIN_SCREEN_MAIN_MAP, routeAlterChargeStationParam.getMRouteAlterChargeStationInfos());
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
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
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
    public void onRouteFail(final RequestRouteResult requestRouteResult, final int errorCode, final String errorMsg, final long requestId) {
        Logger.i(TAG, "onRouteFail");
        //在引导态不是主动触发的算路失败
        if (mRouteAdapter.getRequestRouteId() != requestId && NaviStatus.NaviStatusType.NAVING
                .equals(NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
            Logger.i(TAG, "onReRouteError");
            if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
                return;
            }
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onReRouteError();
            }
        }

        if (ConvertUtils.isEmpty(requestRouteResult) || mRouteAdapter.getRequestRouteId() != requestId) {
            Logger.i(TAG, "onRouteFail requestRouteResult is null or exists multiple times");
            return;
        }
        if (isNeedAutoOfflineRoute(errorCode) && requestRouteResult.isMIsOnlineRoute()) {
            callBackOfflineRouting(requestRouteResult.getMMapTypeId(), errorMsg);
            //延迟3秒等待无网络toast显示完成，自动发起离线算路
            final ScheduledExecutorService executorService = Executors.newSingleThreadScheduledExecutor();
            executorService.schedule(new Runnable() {
                @Override
                public void run() {
                    final RouteRequestParam param = new RouteRequestParam();
                    param.setMMapTypeId(requestRouteResult.getMMapTypeId());
                    param.setMFastNavi(requestRouteResult.isMFastNavi());
                    param.setMRouteWay(requestRouteResult.getMRouteWay());
                    param.setMIsOnline(false);
                    requestRoute(param);
                }
            }, 3, TimeUnit.SECONDS);
            return;
        }
        reGetParamList(requestRouteResult.getMMapTypeId());
        callBackFailMsg(requestRouteResult.getMMapTypeId(), errorMsg);
    }

    /**
     * 回调所有点的信息
     *
     * @param mapTypeId   屏幕id
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
    public void onRouteL2Info(final RouteL2Data routeL2Data) {
        l2DatacallBack(routeL2Data);
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

    @Override
    public void onReRoute() {
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onReroute();
        }
    }

    @Override
    public void onRouteDetails(List<RouteLineSegmentInfo> routeLineDetail) {
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteDetails(routeLineDetail);
        }
    }

    /**
     * 路线上充电站数据回调    、
     *
     * @param routeL2Data 路线信息
     */
    private void l2DatacallBack(final RouteL2Data routeL2Data) {
        if (mPathID == routeL2Data.getMPathID()
                && mNaviStatusAdapter.getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
            Logger.e(TAG, "新路线和老路线是同一条，不再发送L2++数据");
            return;
        }
        if (routeL2Data.getMPathID() != -1) {
            mPathID = routeL2Data.getMPathID();
            Logger.i(TAG, "mPathID:", mPathID);
        } else {
            Logger.e(TAG, "没有path id");
            mPathID = -1;
        }
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onL2DataCallBack(routeL2Data);
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
        final RouteParam routeParam = getRouteParamFromPoiInfoEntity(param.getMPoiInfoEntity(), param.getMRoutePoiType());
        final List<RouteParam> paramList = getParamList(param.getMMapTypeId(), routeParam);
        if (ConvertUtils.isEmpty(paramList) || paramList.size() < 2) {
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
        setCarType();
        mRouteAdapter.setRequestControl(perfrenceId, platNum, "true".equals(mVoidLimit), mNaviStatusAdapter.isGuidanceActive());
        mSelectRouteIndex.put(param.getMMapTypeId(), NumberUtils.NUM_ERROR);
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        }
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteRequest();
            }
        }
        final long requestId = mRouteAdapter.requestRoute(param, paramList);
        mRequestId.put(param.getMMapTypeId(), requestId);
        return requestId;
    }

    /**
     * 平行路切换完成重算路
     */
    public long requestSwitchParallelRoute(int switchRoadType, LocInfoBean locInfoBean, BigInteger roadID, short flag, short hwFlag) {
        return mRouteAdapter.requestSwitchParallelRoute(switchRoadType, locInfoBean, roadID, flag, hwFlag);
    }

    /**
     * 设置车辆类别
     */
    public void setCarType() {
        final int carType = CalibrationAdapter.getInstance().powerType();
        if (carType == 1 || carType == -1) {
            BevPowerCarUtils.getInstance().carType = String.valueOf(2);
            BevPowerCarUtils.getInstance().bevCarElicOpen = true;
        } else {
            BevPowerCarUtils.getInstance().carType = String.valueOf(0);
            BevPowerCarUtils.getInstance().bevCarElicOpen = false;
        }
    }

    /**
     * 第一次传入多个途经点算路请求
     *
     * @param mapTypeId   屏幕ID
     * @param routeParams 途经点列表
     * @return 返回请求的taskId
     */
    public long requestManyVia(final MapType mapTypeId, final List<RouteParam> routeParams) {
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam routeRequestParam = new RouteRequestParam();
        routeRequestParam.setMMapTypeId(mapTypeId);
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_ALL_VIA);
        routeRequestParam.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        routeRequestParam.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        return requestRoute(routeRequestParam);
    }

    /**
     * 修改终点算路请求
     *
     * @param mapTypeId     屏幕ID
     * @param poiInfoEntity 终点数据
     * @return 返回请求taskId
     */
    public long requestChangeEnd(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        mEndRouteParams.put(mapTypeId, getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_END));
        final RouteRequestParam routeRequestParam = new RouteRequestParam();
        routeRequestParam.setMMapTypeId(mapTypeId);
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_END);
        routeRequestParam.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        routeRequestParam.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_VOICE_CHANGE_DEST);
        return requestRoute(routeRequestParam);
    }

    /**
     * 单个添加途经点算路请求
     *
     * @param mapTypeId     屏幕ID
     * @param poiInfoEntity 途径点数据
     */
    public void addViaPoint(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        if (isBelongRouteParam(mapTypeId, poiInfoEntity)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点已经在路线上");
            return;
        }
        if (isMaxRouteParam(mapTypeId)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：最多只能添加5个途径点");
            return;
        }
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        if (routeParams != null) {
            routeParams.add(getRouteParamFromPoiInfoEntity(poiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY));
        }
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mapTypeId);
        param.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        param.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRoute(param);
        sendBuryPointForAddMiddle(poiInfoEntity.getName());
    }

    /**
     * 替换补能点事件
     *
     * @param mapTypeId     屏幕ID
     * @param newPoiInfoEntity 替换补能点
     * @param oldPoiInfoEntity 被他替换补能点
     */
    public void replaceSupplement(final MapType mapTypeId,
                                  final PoiInfoEntity newPoiInfoEntity,
                                  final PoiInfoEntity oldPoiInfoEntity) {
        if (isBelongRouteParam(mapTypeId, newPoiInfoEntity)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点已经在路线上");
            return;
        }
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        removeRouteParam(routeParams, oldPoiInfoEntity);
        if (routeParams != null && routeParams.size() >= 5) {
            callBackFailMsg(mapTypeId, "途经点添加失败：最多只能添加5个途径点");
            return;
        }
        RouteParam replaceParam = getRouteParamFromPoiInfoEntity(newPoiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
        if (replaceParam != null) {
            replaceParam.setMAddressType(AutoMapConstant.ParamPoiType.SUPPLEMENT_POINT);
        }
        if (routeParams != null) {
            routeParams.add(replaceParam);
        }
        mViaRouteParams.put(mapTypeId, routeParams);
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mapTypeId);
        param.setMFastNavi(mNaviStatusAdapter.isGuidanceActive());
        param.setMRouteWay(RouteWayID.ROUTE_WAY_ADD_VIA);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_CHANGE_JNY_PNT);
        requestRoute(param);
        sendBuryPointForAddMiddle(newPoiInfoEntity.getName());
    }

    /**
     * 单个添加途经点带索引算路请求
     *
     * @param mapTypeId     屏幕ID
     * @param poiInfoEntity 途径点数据
     * @param index         添加索引
     */
    public void addViaPoint(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity, final int index) {
        if (isBelongRouteParam(mapTypeId, poiInfoEntity)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：途经点已经在路线上");
            return;
        }
        if (isMaxRouteParam(mapTypeId)) {
            callBackFailMsg(mapTypeId, "途经点添加失败：最多只能添加5个途径点");
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
        sendBuryPointForAddMiddle(poiInfoEntity.getName());
    }

    /**
     * 单个删除添加途经点算路请求
     *
     * @param mapTypeId      屏幕ID
     * @param poiInfoEntity  途径点数据
     * @param isRequestRoute 是否发起算路
     * @return 返回是否成功
     */
    public boolean removeVia(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity, final boolean isRequestRoute) {
        final List<RouteParam> routeParams = mViaRouteParams.get(mapTypeId);
        if (routeParams.size() > 0) {
            int index = -1;
            for (int t = NumberUtils.NUM_0; t < routeParams.size(); t++) {
                if ((!ConvertUtils.isEmpty(routeParams.get(t).getPoiID()) && routeParams.get(t).getPoiID() == poiInfoEntity.getPid())
                        || (poiInfoEntity.getPoint().getLon() == routeParams.get(t).getMRealPos().getLon()
                        && poiInfoEntity.getPoint().getLat() == routeParams.get(t).getMRealPos().getLat())) {
                    index = t;
                }
            }
            if (index == -1) {
                Logger.i(TAG, "have no this point");
                return false;
            }
            Logger.i(TAG, "index:" + index + " Name:" + poiInfoEntity.getName());
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
            sendBuryPointForDeleteMiddle(poiInfoEntity.getName());
            return true;
        }
        return false;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_ROUTE_ADD_MIDDLE)
    private void sendBuryPointForAddMiddle(final String msg) {
        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, msg)
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_ROUTE_DELETE_MIDDLE)
    private void sendBuryPointForDeleteMiddle(final String msg) {
        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, msg)
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }

    /**
     * send2car 算路请求
     *
     * @param routeMsgPushInfo 算路请求参数
     * @param mapTypeId        屏幕ID
     */
    public void requestRouteRestoration(final RouteMsgPushInfo routeMsgPushInfo, final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(routeMsgPushInfo) || ConvertUtils.isEmpty(routeMsgPushInfo.getMPoiInfoEntity())) {
            Logger.e(TAG, "have no push info");
            return;
        }
        final RouteParam routeParam = getRouteParamFromPoiInfoEntity(routeMsgPushInfo.getMPoiInfoEntity(), RoutePoiType.ROUTE_POI_TYPE_END);
        List<RouteParam> paramList = getParamList(mapTypeId, routeParam);
        if (routeMsgPushInfo.getMViaPoiInfoEntity() != null
                && !routeMsgPushInfo.getMViaPoiInfoEntity().isEmpty()) {
            clearVai(mapTypeId);
            for (PoiInfoEntity vaiPoiInfoEntity : routeMsgPushInfo.getMViaPoiInfoEntity()) {
                final RouteParam vaiParam = getRouteParamFromPoiInfoEntity(vaiPoiInfoEntity, RoutePoiType.ROUTE_POI_TYPE_WAY);
                paramList = getParamList(mapTypeId, vaiParam);
            }
        } else {
            clearVai(mapTypeId);
        }
        if (ConvertUtils.isEmpty(paramList) || paramList.size() < 2) {
            callBackFailMsg(mapTypeId, "点参数异常");
            return;
        }

        final RoutePreferenceID perfrenceId = SettingPackage.getInstance().getRoutePreference();
        final String platNum = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        final String mVoidLimit = SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        initBevCarData();
        setCarType();
        mRouteAdapter.setRequestControl(perfrenceId, platNum, "true".equals(mVoidLimit), mNaviStatusAdapter.isGuidanceActive());
        mSelectRouteIndex.put(mapTypeId, NumberUtils.NUM_ERROR);
        if (!mNaviStatusAdapter.isGuidanceActive()) {
            mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.ROUTING);
        }
        mRouteAdapter.requestRouteRestoration(routeMsgPushInfo, mapTypeId);
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteRequest();
            }
        }
    }

    /**
     * 请求路线上的天气
     *
     * @param mapTypeId 屏幕ID
     * @param index     路线ID
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
     * 回调离线算路信息
     *
     * @param mapTypeId 屏幕ID
     * @param errorText 错误信息
     */
    public void callBackOfflineRouting(final MapType mapTypeId, final String errorText) {
        if (ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            return;
        }
        for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
            if (ConvertUtils.isEmpty(routeResultObserver)) {
                continue;
            }
            routeResultObserver.onRouteOffline(mapTypeId, errorText);
        }
    }

    /**
     * 请求备选充电站
     *
     * @param mapTypeId 屏幕ID
     * @param poiId     推荐充电站
     * @return 返回请求的taskId
     */
    public long requestRouteAlternativeChargeStation(final MapType mapTypeId, final String poiId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return -1;
        }
        final RouteChargeStationParam routeChargeStationParam = mRequestRouteResults.get(mapTypeId).getMRouteChargeStationParam();
        if (ConvertUtils.isEmpty(routeChargeStationParam)) {
            return -1;
        }
        if (ConvertUtils.isEmpty(routeChargeStationParam.getMPathInfoList())) {
            return -1;
        }
        return mRouteAdapter.requestRouteAlternativeChargeStation(routeChargeStationParam.getMPathInfoList()
                .get(mSelectRouteIndex.get(mapTypeId)), poiId);
    }

    /**
     * 请求限行数据
     *
     * @param mapTypeId 屏幕ID
     * @return 返回请求的taskId
     */
    public long requestRestirction(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults) || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))
                || ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId).getMRouteRestrictionParam()) || mSelectRouteIndex == null) {
            return -1;
        }
        Integer index = mSelectRouteIndex.get(mapTypeId);
        if (index == null) {
            return -1;
        }
        final List<String> ruleIDs = mRequestRouteResults.get(mapTypeId).getMRouteRestrictionParam().getMRuleIds();
        if (index != -1 && ruleIDs.size() > index
                && !ConvertUtils.isEmpty(ruleIDs.get(index))) {
            final RestrictedParam restrictedParam = new RestrictedParam();
            restrictedParam.setRestrict_type(9);
            restrictedParam.setPlate(SettingManager.getInstance().getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER));
            restrictedParam.setRuleids(ruleIDs.get(index));
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
     *
     * @param poiInfoEntity 点信息
     * @param poiType       点的类别
     * @return 转换后数据
     */
    public RouteParam getRouteParamFromPoiInfoEntity(final PoiInfoEntity poiInfoEntity, final int poiType) {
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
        if (poiInfoEntity.getPointTypeCode() != null && poiInfoEntity.getPointTypeCode().startsWith("0111")) {
            routeParam.setMAddressType(AutoMapConstant.ParamPoiType.CHARGING_STATION_POINT);
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
     *
     * @param mapTypeId 屏幕id
     */
    private void clearVai(final MapType mapTypeId) {
        mViaRouteParams.put(mapTypeId, new ArrayList<>());
    }

    /**
     * 判断是否是路线上的点
     *
     * @param mapTypeId     屏幕Id
     * @param poiInfoEntity 点信息
     * @return 返回是否
     */
    public boolean isBelongRouteParam(final MapType mapTypeId, final PoiInfoEntity poiInfoEntity) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return false;
        }
        final List<RouteParam> allPoiParamList = getAllPoiParamList(mapTypeId);
        for (RouteParam routeParam : allPoiParamList) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                return true;
            }
        }
        return false;
    }

    /**
     * 删除相同的点
     *
     * @param routeParams   点集合
     * @param poiInfoEntity 点信息
     */
    public void removeRouteParam(final List<RouteParam> routeParams, final PoiInfoEntity poiInfoEntity) {
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            return;
        }
        for (RouteParam routeParam : routeParams) {
            if (isTheSamePoi(routeParam, poiInfoEntity)) {
                routeParams.remove(routeParam);
                return;
            }
        }
    }

    /**
     * 是否是起点&终点
     *
     * @param mapTypeId     屏幕Id
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
     *
     * @param routeParam    点信息
     * @param poiInfoEntity 点信息
     * @return 返回是否是起点&终点
     */
    public boolean isTheSamePoi(final RouteParam routeParam, final PoiInfoEntity poiInfoEntity) {
        if (ConvertUtils.isEmpty(routeParam) || ConvertUtils.isEmpty(poiInfoEntity)
                || ConvertUtils.isEmpty(routeParam.getRealPos())
                || ConvertUtils.isEmpty(poiInfoEntity.getPoint())) {
            return false;
        }
        if ((!ConvertUtils.isEmpty(routeParam.getPoiID()) && routeParam.getPoiID() == poiInfoEntity.getPid()) || (routeParam.getRealPos().getLat() == poiInfoEntity.getPoint().getLat()
                && routeParam.getRealPos().getLon() == poiInfoEntity.getPoint().getLon())) {
            return true;
        }
        return false;
    }

    /**
     * 是否超过最大点的数量
     *
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
        RequestRouteResult requestRouteResult = mRequestRouteResults.get(MapType.MAIN_SCREEN_MAIN_MAP);
        if (ConvertUtils.isEmpty(requestRouteResult)) return;
        final List<RouteLineInfo> routeLineInfos = mRequestRouteResults.get(MapType.MAIN_SCREEN_MAIN_MAP).getMRouteLineInfos();
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(MapType.MAIN_SCREEN_MAIN_MAP).getMLineLayerParam();
        final ArrayList<String> arrivalTimes = new ArrayList<>();
        for (RouteLineInfo routeLineInfo : routeLineInfos) {
            arrivalTimes.add(routeLineInfo.getMTravelTime());
        }
        routeLineLayerParam.setMEstimatedTimeOfArrival(arrivalTimes);
        mLayerAdapter.drawRouteLine(mapTypeId, requestRouteResult);
//            if (!mNaviStatusAdapter.isGuidanceActive()) {
//                mLayerAdapter.setCarLogoVisible(mapTypeId, false);
//            }
    }

    /**
     * 绘制补能点扎标
     *
     * @param mapTypeId 屏幕ID
     * @param routeChargeStation 补能点参数
     */
    public void updateRouteChargeStation(MapType mapTypeId, RouteChargeStationParam routeChargeStation) {
        mLayerAdapter.updateRouteChargeStation(mapTypeId, routeChargeStation);
    }

    /*更新终点扎标数据*/
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        mLayerAdapter.updateRouteEndPoint(mapTypeId, endPoint);
    }

    /**
     * 绘制终点附近有可用停车点
     *
     * @param mapTypeId 屏幕ID
     */
    public void showRoutePark(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            Logger.d(TAG, "showRoutePark mRequestRouteResults is null");
            return;
        }
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getMLineLayerParam();
        if (ConvertUtils.isEmpty(routeLineLayerParam)) {
            Logger.d(TAG, "showRoutePark routeLineLayerParam is null");
            return;
        }
        final LayerItemLabelResult layerItemLabelResult = new LayerItemLabelResult();
        layerItemLabelResult.setPointType(LayerItemLabelResult.ILabelLayerPointType.LABEL_POINT_TYPE_PARK);
        layerItemLabelResult.setPos(routeLineLayerParam.getMRouteLinePoints().getMEndPoints().get(0).getMPos());
        mLayerAdapter.updatePopSearchPointInfo(mapTypeId, layerItemLabelResult);
    }

    /**
     * 清除路线
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearRouteLine(final MapType mapTypeId) {
        mNaviStatusAdapter.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
        mLayerAdapter.clearRouteLine(mapTypeId);
        mLayerAdapter.clearLabelItem(mapTypeId);
        mLayerAdapter.setCarLogoVisible(mapTypeId, true);
        removeAllRouteInfo(mapTypeId);
    }

    /**
     * 清除终点附近停车场扎标
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearEndParkPoint(final MapType mapTypeId) {
        mLayerAdapter.clearLabelItem(mapTypeId);
    }

    /**
     * 设置路线样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(final MapType mapTypeId, final boolean isStartNavi, final boolean isOffLine, final boolean isMultipleMode) {
        mLayerAdapter.setPathStyle(mapTypeId, isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index     隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(final MapType mapTypeId, final int index, final boolean isVisible) {
        return mLayerAdapter.setPathVisible(mapTypeId, index, isVisible);
    }

    /**
     * 算路页面路线全览
     *
     * @param mapTypeId 屏幕ID
     */
    public void showPreview(final MapType mapTypeId) {
        ThreadManager.getInstance().execute(() -> {
            mLayerAdapter.setFollowMode(mapTypeId, false);
            if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
                return;
            }
            final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId)
                    .getMLineLayerParam();
            int routeScreenLeft = ResourceUtils.Companion.getInstance().
                    getDimensionPixelSize(R.dimen.route_margin_screen_left);
            int routeScreenRight = ResourceUtils.Companion.getInstance().
                    getDimensionPixelSize(R.dimen.route_margin_screen_right);
            int routeScreenTop = ResourceUtils.Companion.getInstance().
                    getDimensionPixelSize(R.dimen.route_margin_screen_top);
            int routeScreenBottom = ResourceUtils.Companion.getInstance().
                    getDimensionPixelSize(R.dimen.route_margin_screen_bottom);
            mMapPackage.showPreview(mapTypeId, true, routeScreenLeft, routeScreenTop,
                    routeScreenRight, routeScreenBottom, mLayerAdapter.getPathResultBound(mapTypeId,
                            routeLineLayerParam.getMPathInfoList()));
        });
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
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.
                get(mapTypeId).getMLineLayerParam();
        int screenLeft = ResourceUtils.Companion.getInstance().
                getDimensionPixelSize(R.dimen.margin_screen_left);
        int screenRight = ResourceUtils.Companion.getInstance().
                getDimensionPixelSize(R.dimen.margin_screen_right);
        int screenTop = ResourceUtils.Companion.getInstance().
                getDimensionPixelSize(R.dimen.margin_screen_top);
        int screenBottom = ResourceUtils.Companion.getInstance().
                getDimensionPixelSize(R.dimen.margin_screen_bottom);
        mMapPackage.showPreview(mapTypeId, true, screenLeft, screenTop, screenRight,
                screenBottom, mLayerAdapter.getPathResultBound(mapTypeId,
                        routeLineLayerParam.getMPathInfoList()));
    }

    /**
     * 1/3屏页面路线全览
     *
     * @param mapTypeId 屏幕ID
     */
    public void oneThirdScreeShowPreview(final MapType mapTypeId, Rect rect) {
        Logger.i(TAG, "oneThirdScreeShowPreview mapTypeId = " + mapTypeId);
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        final RouteLineLayerParam routeLineLayerParam = mRequestRouteResults.get(mapTypeId).getMLineLayerParam();
        mMapPackage.showPreview(mapTypeId, true, rect.left, rect.top, rect.right, rect.bottom, mLayerAdapter.getPathResultBound(mapTypeId, routeLineLayerParam.getMPathInfoList()));
    }

    /**
     * 切换路线
     * @param mapTypeId  屏幕ID
     */
    public void selectDefaultRoute(final MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mRequestRouteResults)) {
            return;
        }
        final RequestRouteResult requestRouteResult = mRequestRouteResults.get(mapTypeId);
        if (ConvertUtils.isEmpty(requestRouteResult)) {
            Logger.e(TAG, "no data");
            return;
        }
        RouteLineLayerParam routeLineLayerParam = requestRouteResult.getMLineLayerParam();
        if (routeLineLayerParam.getMPathInfoList().isEmpty()) {
            Logger.e(TAG, "out of bounds");
            return;
        }
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteSlected(mapTypeId, 0, true);
            }
        }
        routeLineLayerParam.setMSelectIndex(0);
        mSelectRouteIndex.put(mapTypeId, 0);
        if (!ConvertUtils.isEmpty(requestRouteResult)) {
            final RouteCurrentPathParam routeCurrentPathParam = requestRouteResult.getMRouteCurrentPathParam();
            routeCurrentPathParam.setMMapTypeId(mapTypeId);
            routeCurrentPathParam.setMRequestId(requestRouteResult.getMRequestId());
            routeCurrentPathParam.setMPathInfo(routeLineLayerParam.getMPathInfoList().get(0));
            routeCurrentPathParam.setMIsOnlineRoute(routeLineLayerParam.isMIsOnlineRoute());
            mRouteAdapter.setCurrentPath(routeCurrentPathParam);
        }
        if (!ConvertUtils.isEmpty(requestRouteResult)) {
            mNaviAdapter.updateNaviPath(routeLineLayerParam);
        }
        // TODO: 2025/6/8 暂时先放在这里 后续OpenApiHelper需要删除
        OpenApiHelper.setCurrentPathInfos((ArrayList<PathInfo>)
                routeLineLayerParam.getMPathInfoList());
    }

    /**
     * 切换路线
     *
     * @param mapTypeId  屏幕ID
     * @param routeIndex 路线id
     */
    public void selectRoute(final MapType mapTypeId, final int routeIndex) {
        Logger.i(TAG, "selectRoute mapTypeId = " + mapTypeId +
                " routeIndex = " + routeIndex);
        Logger.i(TAG, mapTypeId.getMapType());
        if (ConvertUtils.isEmpty(mRequestRouteResults)) {
            return;
        }
        final RequestRouteResult requestRouteResult = mRequestRouteResults.get(mapTypeId);
        if (ConvertUtils.isEmpty(requestRouteResult)) {
            Logger.e(TAG, "no data");
            return;
        }
        RouteLineLayerParam routeLineLayerParam = requestRouteResult.getMLineLayerParam();
        if (routeIndex == -1 || routeIndex >= routeLineLayerParam.getMPathInfoList().size()) {
            Logger.e(TAG, "out of bounds");
            return;
        }
        if (!ConvertUtils.isEmpty(mRouteResultObserverMap)) {
            for (IRouteResultObserver routeResultObserver : mRouteResultObserverMap.values()) {
                if (ConvertUtils.isEmpty(routeResultObserver)) {
                    continue;
                }
                routeResultObserver.onRouteSlected(mapTypeId, routeIndex, false);
            }
        }
        routeLineLayerParam.setMSelectIndex(routeIndex);
        mLayerAdapter.setSelectedPathIndex(mapTypeId, routeIndex);
        mSelectRouteIndex.put(mapTypeId, routeIndex);
        if (!ConvertUtils.isEmpty(requestRouteResult)) {
            mNaviAdapter.updateNaviPath(routeLineLayerParam);
        }
        if (!ConvertUtils.isEmpty(requestRouteResult)) {
            final RouteCurrentPathParam routeCurrentPathParam = requestRouteResult.getMRouteCurrentPathParam();
            routeCurrentPathParam.setMMapTypeId(mapTypeId);
            routeCurrentPathParam.setMRequestId(requestRouteResult.getMRequestId());
            routeCurrentPathParam.setMPathInfo(routeLineLayerParam.getMPathInfoList().get(routeIndex));
            routeCurrentPathParam.setMIsOnlineRoute(routeLineLayerParam.isMIsOnlineRoute());
            mRouteAdapter.setCurrentPath(routeCurrentPathParam);
        }
        // TODO: 2025/6/8 暂时先放在这里 后续OpenApiHelper需要删除
        OpenApiHelper.setCurrentPathInfos((ArrayList<PathInfo>)
                routeLineLayerParam.getMPathInfoList());
    }

    /**
     * 展示路线服务区扎点
     *
     * @param mapTypeId 屏幕ID
     * @param index     路线
     */
    public void showRestArea(final MapType mapTypeId, final int index) {
        if (ConvertUtils.isEmpty(mRequestRouteResults.get(mapTypeId))) {
            return;
        }
        mLayerAdapter.showRestArea(mapTypeId, mRequestRouteResults.get(mapTypeId).getMRouteRestAreaParam().getMPathInfoList(), index);
    }

    /**
     * 清除路线服务区扎点
     *
     * @param mapTypeId 屏幕ID
     */
    public void clearRestArea(final MapType mapTypeId) {
        mLayerAdapter.showRestArea(mapTypeId, null, -1);
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
     * @param object    限行数据
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
     * @param mapTypeId   屏幕ID
     * @param routeParams 途径点信息
     */
    public void updateViaParamList(final MapType mapTypeId, final List<RouteParam> routeParams) {
        mViaRouteParams.put(mapTypeId, routeParams);
    }

    /**
     * 获取所有点的信息
     *
     * @param mapTypeId  屏幕ID
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
     *
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
     * 判断是否为算路态。
     *
     * @return true为算路态，false为算路态
     */
    public boolean isRouteState() {
        final String currentNaviStatus = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        final Set<String> validStatuses = new HashSet<>(Set.of(
                NaviStatus.NaviStatusType.ROUTING,
                NaviStatus.NaviStatusType.SELECT_ROUTE
        ));
        return validStatuses.contains(currentNaviStatus);
    }

    /**
     * 获取路线上所有的点
     *
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
     *
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
     *
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
     *
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
     *
     * @param mapTypeId 屏幕Id
     */
    public void removeAllRouteInfo(final MapType mapTypeId) {
        mRequestRouteResults.put(mapTypeId, null);
        mStartRouteParams.put(mapTypeId, null);
        mViaRouteParams.put(mapTypeId, new ArrayList<>());
        mEndRouteParams.put(mapTypeId, null);
        mEndPoiEntity.put(mapTypeId, null);
        mSaveViaRouteParams.put(mapTypeId, new ArrayList<>());
        mSaveEndRouteParams.put(mapTypeId, null);
    }

    /**
     * 获取终点信息
     *
     * @param mapTypeId 屏幕Id
     * @return 返回终点信息
     */
    public RouteParam getEndPoint(final MapType mapTypeId) {
        return mEndRouteParams.get(mapTypeId);
    }

    /**
     * 获取终点详情信息
     *
     * @param mapTypeId 屏幕Id
     * @return 返回终点信息
     */
    public PoiInfoEntity getEndEntity(final MapType mapTypeId) {
        return mEndPoiEntity.get(mapTypeId);
    }

    /**
     * 设置终点详情信息
     *
     * @param mapTypeId    屏幕Id
     * @param endPoiEntity 终点信息
     */
    public void setEndEntity(final MapType mapTypeId, final PoiInfoEntity endPoiEntity) {
        mEndPoiEntity.put(mapTypeId, endPoiEntity);
    }

    /**
     * 获取当前路线信息
     *
     * @param mapTypeId 屏幕Id
     * @return 算路信息
     */
    public RouteCurrentPathParam getCurrentPathInfo(final MapType mapTypeId) {
        return mRouteAdapter.getCurrentPath(mapTypeId);
    }

    /**
     * 获取能量耗尽点信息
     *
     * @return 返回能耗信息
     */
    public ArrayList<EvRangeOnRouteInfo> getEvRangeOnRouteInfos() {
        return mEvRangeOnRouteInfos;
    }

    /**
     * 绘制限行区域
     *
     * @param mapTypeId 屏幕Id
     * @param param     绘制参数
     * @param position  index
     */
    public void drawRestrictionForLimit(final MapType mapTypeId, final Object param, final int position) {
        mLayerAdapter.showRestrictionView(mapTypeId, null);
        mLayerAdapter.showRestrictionView(mapTypeId, param, position);
    }

    /**
     * 设置避开道路
     *
     * @param routeAvoidInfo 避开参数
     */
    public void setAvoidRoad(final RouteAvoidInfo routeAvoidInfo) {
        mRouteAdapter.setAvoidRoad(routeAvoidInfo);
    }

    /**
     * 语音请求算路
     *
     * @param param 算路参数
     * @return taskId
     */
    public long requestRouteFromSpeech(final RouteSpeechRequestParam param) {
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

        final RouteRequestParam routeRequestParam = new RouteRequestParam();
        ;
        routeRequestParam.setMMapTypeId(param.getMMapTypeId());
        routeRequestParam.setMRouteWay(RouteWayID.ROUTE_WAY_SPEECH);
        routeRequestParam.setMRoutePreferenceID(param.getMPreferenceID());
        return requestRoute(routeRequestParam);
    }

    /**
     * 提供给语音--获取途经点个数
     *
     * @param mapTypeId 屏幕Id
     * @return 途径点个数
     */
    public int getViaPointsCount(final MapType mapTypeId) {
        return mViaRouteParams.get(mapTypeId).size();
    }

    /**
     * 提供给语音--获取两个点的距离和时间
     *
     * @param geoPointStart 起点坐标
     * @param geoPointend   终点坐标
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
     *
     * @param geoPointStart 起点坐标
     * @param geoPointend   终点坐标
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
     *
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
     *
     * @param mapTypeId 屏幕ID
     */
    public void refreshHomeOfficeTMC(final MapType mapTypeId, final boolean isHome) {
        final PoiInfoEntity poiInfoEntity = isHome ? BehaviorPackage.getInstance().getHomeFavoriteInfo()
                : BehaviorPackage.getInstance().getCompanyFavoriteInfo();
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
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_YAW);
        param.setRouteRequestCallBackType(isHome ? 0 : 1);
        mRouteAdapter.requestRoute(param, routeParams);
    }

    /**
     *
     * @param geoPoint 经纬度点
     * @return ETAInfo 包含距离，剩余时间和剩余电量
     */
    public CompletableFuture<ETAInfo> getTravelTimeFutureIncludeChargeLeft(final GeoPoint geoPoint) {
        final GeoPoint userLoc = new GeoPoint();
        userLoc.setLon(mPositionAdapter.getLastCarLocation().getLongitude());
        userLoc.setLat(mPositionAdapter.getLastCarLocation().getLatitude());
        final SearchRequestParameter requestParameterBuilder = new SearchRequestParameter.Builder()
                .userLoc(userLoc)
                .poiLoc(geoPoint)
                .build();
        return mSearchAdapter.getTravelTimeFutureIncludeChargeLeft(requestParameterBuilder);
    }

    public Map<MapType, Long> getRequestIds() {
        return mRequestId;
    }

    public void requestRouteDetails(int index) {
        mRouteAdapter.requestRouteDetails(index);
    }

    public void requestRouteRestArea(int index) {
        mRouteAdapter.requestRouteRestArea(index);
    }

    private static final class Helper {
        private static final RoutePackage EP = new RoutePackage();
    }
}
