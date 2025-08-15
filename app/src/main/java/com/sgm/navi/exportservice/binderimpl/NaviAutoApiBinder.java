package com.sgm.navi.exportservice.binderimpl;

import android.os.Bundle;
import android.os.RemoteCallbackList;
import android.os.RemoteException;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.fsa.FsaConstant;
import com.sgm.navi.fsa.MyFsaService;
import com.sgm.navi.fsa.bean.PoiInfoForExport;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.splitscreen.SRFloatWindowService;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.mapservice.bean.common.BaseCityInfo;
import com.sgm.navi.mapservice.bean.common.BaseDestInfo;
import com.sgm.navi.mapservice.bean.common.BaseDistrictInfo;
import com.sgm.navi.mapservice.bean.common.BaseGeoPoint;
import com.sgm.navi.mapservice.bean.common.BaseLocationInfo;
import com.sgm.navi.mapservice.bean.common.BaseFsaPoiInfo;
import com.sgm.navi.mapservice.bean.common.BaseRouteLine;
import com.sgm.navi.mapservice.bean.common.BaseRouteResult;
import com.sgm.navi.mapservice.bean.common.BaseSearchPoi;
import com.sgm.navi.mapservice.bean.common.BaseSearchResult;
import com.sgm.navi.mapservice.bean.common.BaseTurnInfo;
import com.sgm.navi.mapservice.common.INaviAutoApiBinder;
import com.sgm.navi.mapservice.common.INaviAutoApiCallback;
import com.sgm.navi.mapservice.common.INaviAutoCountDownLightCallback;
import com.sgm.navi.mapservice.common.INaviAutoLocationCallback;
import com.sgm.navi.mapservice.common.INaviAutoPoiCallBack;
import com.sgm.navi.mapservice.common.INaviAutoRouteCallback;
import com.sgm.navi.mapservice.common.INaviAutoSearchCallback;
import com.sgm.navi.mapservice.common.INaviAutoSpeedCallBack;
import com.sgm.navi.mapservice.common.INaviAutoStatusCallback;
import com.sgm.navi.mapservice.util.ExportConvertUtil;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.DrBean;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocStatus;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.search.CityInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.search.SearchResultEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.vrbridge.IVrBridgeConstant;
import com.google.gson.reflect.TypeToken;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ScheduledFuture;


public class NaviAutoApiBinder extends INaviAutoApiBinder.Stub implements StartService.ISdkInitCallback {

    private static final String TAG = NaviAutoApiBinder.class.getSimpleName();
    private static final String INNER_CLIENT = "default";
    private static final String DBA_CLIENT = "com.patac.hmi.dba";
    private static final String ONSTAR_CLIENT = "com.patac.hmi.onstar";
    private static final String ADM_CLIENT = "com.sgm.hmi.lvmapa.adm";
    private static final String ADMHC_CLIENT = "com.sgm.hmi.lvmapa.admhc";
    private static final String ADCU_CLIENT = "com.sgm.hmi.lvmapa.adcu";
    private static final String GALLERY_CLIENT = "com.patac.hmi.gallery";
    private static final int LOCATION_INTERVAL = 5000;
    private static final int REVERSE_INTERVAL = 30 * 1000;
    private static final String DIST_ZERO = "0m";


    private final RemoteCallbackList<INaviAutoApiCallback> mNaviAutoCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoLocationCallback> mLocationCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoRouteCallback> mRouteCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoSearchCallback> mSearchCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoStatusCallback> mStatusCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoSpeedCallBack> mSpeedCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoPoiCallBack> mPoiCallbackList = new RemoteCallbackList<>();
    private final RemoteCallbackList<INaviAutoCountDownLightCallback> mCountDownLightCallbackList = new RemoteCallbackList<>();

    /*--------------------------------各个Package对应的回调-----------------------------------------*/
    private IPositionPackageCallback mPositionCallback;
    private SearchResultCallback mSearchResultCallback;
    private NaviStatusCallback mNaviStatusCallback;
    private IRouteResultObserver mRouteResultObserver;
    private IGuidanceObserver mGuidanceObserver;
    private SettingPackage.SettingChangeCallback mSettingChangeCallback;


    private boolean mInCallback;
    private boolean mInRouteCallBack;
    private BaseTurnInfo mBaseTurnInfo = null;

    //收到定位改变后发起逆地理搜索获取DistrictInfo
    private int mDistrictSearchId;
    private BaseLocationInfo mLocationInfo;
    private long mLocationCallbackMillis = 0L;
    private BaseDistrictInfo mDistrictInfo = null;
    private ScheduledFuture mDistrictIntervalFuture;
    private int mGeoSearchInterval = 0;

    //周边搜索taskId
    private int mCommonSearchId = -1;
    //逆地理搜索taskId
    private int mGeoSearchId = -1;
    //关键字 匹配关键字搜索结果返回
    private String mSearchKeyword = "";
    //收到搜索结果后是否发起算路，对应searchAndNavi接口的需求，
    private boolean mRouteRequestAfterSearch = false;

    private ScheduledFuture mGuideStatusHolder;
    private int mTmcTotalDistance = 0;
    private int mTmcFinishDistance = 0;
    private String DestName = "安吉星地址推荐";
    //当前引导面板状态
    private int mGuidePanelStatus;

    //允许调用逆地理编码接口的应用包名集合
    private ConcurrentHashMap<String, Long> mReversePkgMap;

    private final MyFsaService.ExportEventCallBack mEventCallBack =
            (eventId, eventStr) -> handleExportEvent(eventId, transformBeanDefine(eventStr));


    public NaviAutoApiBinder() {
        StartService.getInstance().registerSdkCallback("", this);
        if (!StartService.getInstance().checkSdkIsNeedInit()) {
            registerCallback();
        }
        mGuidePanelStatus = getGuidePanelStatus(TAG);
        mReversePkgMap = new ConcurrentHashMap<>();
        mReversePkgMap.put(DBA_CLIENT, 0L);
        mReversePkgMap.put(ONSTAR_CLIENT, 0L);
        mReversePkgMap.put(ADM_CLIENT, 0L);
        mReversePkgMap.put(ADMHC_CLIENT, 0L);
        mReversePkgMap.put(ADCU_CLIENT, 0L);
        mReversePkgMap.put(GALLERY_CLIENT, 0L);
    }

    @Override
    public void onSdkInitSuccess() {
        registerCallback();
    }

    private void registerCallback() {
        initPositionCallback();
        initSearchCallback();
        initNaviStatusCallback();
        initRouteCallback();
        initNaviInfoCallback();
        initSettingCallback();
        PositionPackage.getInstance().registerCallBack(mPositionCallback);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        SearchPackage.getInstance().registerCallBack("NaviAutoApiBinder", mSearchResultCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, mRouteResultObserver);
        NaviPackage.getInstance().registerObserver(TAG, mGuidanceObserver);
        SettingPackage.getInstance().setSettingChangeCallback(TAG, mSettingChangeCallback);
        MyFsaService.getInstance().registerExportEventCallBack(mEventCallBack);
    }

    /**
     * 初始化定位回调.
     */
    private void initPositionCallback() {
        //定位信息回调
        mPositionCallback = new IPositionPackageCallback() {
            @Override
            public void onLocationInfo(final LocInfoBean locationInfo) {
                if (null == locationInfo) {
                    Logger.e(TAG, "locationInfo is null");
                    return;
                }

                final String locationData = GsonUtils.toJson(locationInfo);
                mLocationInfo = GsonUtils.fromJson(locationData, BaseLocationInfo.class);

                if (null != mLocationInfo && System.currentTimeMillis() - mLocationCallbackMillis > LOCATION_INTERVAL) {
                    //对外分发定位信息
                    mLocationCallbackMillis = System.currentTimeMillis();
                    dispatchLocationInfo();
                    //收到定位消息后通过逆地理搜索获取DistrictInfo和最近Poi详细信息
                    if (null == mDistrictInfo || mGeoSearchInterval <= 0) {
                        final GeoPoint geoPoint = new GeoPoint(locationInfo.getLongitude(), locationInfo.getLatitude());
                        initDistrict(geoPoint);
                    }
                }
            }

            @Override
            public void onLocationStatus(final LocStatus locStatus) {
                //empty
            }

            @Override
            public void onDrInfo(final DrBean drInfo) {
                //empty
            }
        };
    }

    /**
     * 分发定位信息.
     */
    private void dispatchLocationInfo() {
        if (null == mLocationInfo) {
            return;
        }

        try {
            Logger.d(TAG, "onLocationInfoChange in broadcast");
            final int count = mLocationCallbackList.beginBroadcast();
            final String locationData = GsonUtils.toJson(mLocationInfo);
            for (int i = 0; i < count; i++) {
                final INaviAutoLocationCallback locationCallback = mLocationCallbackList.getRegisteredCallbackItem(i);
                if (null != locationCallback) {
                    try {
                        locationCallback.onLocationInfoChange(locationData);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeLocationCallback();
        }
    }

    /**
     * 根据定位到的经纬度信息获取行政区划信息.
     *
     * @param geoPoint GeoPoint，当前定位经纬度.
     */
    private void initDistrict(final GeoPoint geoPoint) {
        if (mGeoSearchInterval > 0) {
            return;
        }
        final SearchPackage searchPackage = SearchPackage.getInstance();
        if (null != searchPackage) {
            Logger.d(TAG, "getDistrictInfo by point ", geoPoint);
            mGeoSearchInterval = INaviConstant.ScheduleInterval.THREE_MINUTE;
            mDistrictSearchId = searchPackage.geoSearch(geoPoint, true);
            mDistrictIntervalFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> {
                if (null != mDistrictIntervalFuture) {
                    mDistrictIntervalFuture.cancel(true);
                    mDistrictIntervalFuture = null;
                }
                mGeoSearchInterval = 0;
            }, mGeoSearchInterval);
        }
    }

    /**
     * 结束定位和行政区划信息透出.
     */
    private void closeLocationCallback() {
        if (null != mLocationCallbackList) {
            try {
                mLocationCallbackList.finishBroadcast();
            } catch (IllegalStateException illegalStateException) {
                Logger.e(TAG, "finishLocationBroadcast error: " + illegalStateException.getMessage());
            }
        }
    }

    /**
     * 根据逆地理搜索结果更新行政区域和位置信息.
     *
     * @param poiInfo PoiInfoEntity，逆地理搜索结果.
     */
    private void updateDistrictAndLocation(final PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Logger.e(TAG, "poiInfo is null");
            return;
        }
        if (null != mLocationInfo) {
            mLocationInfo.setName(poiInfo.getName());
            mLocationInfo.setAddress(poiInfo.getAddress());
        }

        final CityInfo cityInfo = poiInfo.getCityInfo();
        Logger.d(TAG, "updateDistrictAndLocation: " + cityInfo);
        if (null != cityInfo) {
            if (null == mDistrictInfo) {
                mDistrictInfo = new BaseDistrictInfo();
            }
            mDistrictInfo.setProvince(cityInfo.getProvince());
            mDistrictInfo.setProvinceId(cityInfo.getProvinceAdCode());
            mDistrictInfo.setCity(cityInfo.getCityName());
            final int cityId = cityInfo.getCityCode();
            final int convertedId = ExportConvertUtil.getInstance().cityIdConvert(cityId);
            mDistrictInfo.setCityId(convertedId);
            mDistrictInfo.setDistrict(cityInfo.getDistrict());
            mDistrictInfo.setDistrictId(cityInfo.getDistrictAdCode());
            dispatchDistrictInfo();
        }
    }

    /**
     * 分发行政区域信息.
     */
    private void dispatchDistrictInfo() {
        Logger.d(TAG, "dispatchDistrictInfo");
        if (null == mDistrictInfo) {
            Logger.d(TAG, "DistrictInfo is null");
            return;
        }

        try {
            final int count = mLocationCallbackList.beginBroadcast();
            final String districtData = GsonUtils.toJson(mDistrictInfo);
            Logger.d(TAG, "dispatchDistrictInfo: districtData" + districtData);
            for (int i = 0; i < count; i++) {
                final INaviAutoLocationCallback locationCallback = mLocationCallbackList.getRegisteredCallbackItem(i);
                if (null != locationCallback) {
                    try {
                        locationCallback.onDistrictInfoChange(districtData);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch districtInfo error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.w(TAG, exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeLocationCallback();
        }
    }


    /**
     * 初始化搜索回调.
     */
    private void initSearchCallback() {
        mSearchResultCallback = new SearchResultCallback() {
            @Override
            public void onSearchResult(final int taskId, final int errorCode, final String message,
                                       final SearchResultEntity searchResultEntity) {

                String keyword = "";
                if (null != searchResultEntity) {
                    keyword = searchResultEntity.getKeyword();
                }
                if (!TextUtils.isEmpty(keyword) && (keyword.equals(mSearchKeyword) || keyword.contains(mSearchKeyword))) {
                    //jumpToSearchPage 结果通过keyword匹配
                    mSearchKeyword = "";
                    final boolean searchSuccess = null != searchResultEntity && null != searchResultEntity.getPoiList()
                            && !searchResultEntity.getPoiList().isEmpty();
                    Logger.i(TAG, "onSearchResult " + searchSuccess);
                    if (searchSuccess) {
                        dispatchSearchSuccess(false, searchResultEntity);
                    } else {
                        dispatchSearchFailed(false, errorCode);
                    }
                }
            }

            @Override
            public void onSilentSearchResult(final int taskId, final int errorCode, final String message,
                                             final SearchResultEntity searchResultEntity) {

                final boolean requestDistrict = mDistrictSearchId == taskId;
                final boolean geoSearch = mGeoSearchId == taskId;
                final boolean nearbySearch = mCommonSearchId == taskId;
                if (!(requestDistrict || geoSearch || nearbySearch || mRouteRequestAfterSearch)) {
                    Logger.w(TAG, "not binder search request");
                    return;
                }

                final boolean success = null != searchResultEntity && null != searchResultEntity.getPoiList()
                        && !searchResultEntity.getPoiList().isEmpty();
                Logger.d(TAG, "onSilentSearchResult success: " + success);
                PoiInfoEntity firstPoi = null;
                if (success) {
                    firstPoi = searchResultEntity.getMPoiList().get(0);
                }

                //逆地理解析获取行政区域信息，不对外分发
                if (mDistrictSearchId == taskId) {
                    Logger.d(TAG, "onSearchSilentResult, mDistrictSearchId == taskId, success: " + success);
                    mDistrictSearchId = -1;
                    if (success && null != firstPoi) {
                        updateDistrictAndLocation(firstPoi);
                    }
                    return;
                }

                boolean hasProcess = true;
                if (taskId == mGeoSearchId) {
                    //requestReverseGeoSearch结果
                    Logger.d(TAG, "onSilentSearch: taskId == mGeoSearchId");
                    if (success) {
                        String poiMsg = "empty poi";
                        if (null != firstPoi) {
                            poiMsg = (firstPoi.getName() + " ,address: " + firstPoi.getAddress() + " ,pId: " + firstPoi.getPid());
                        }
                        Logger.d(TAG, "reverseSearchResult: " + poiMsg);
                        dispatchReverseSearch(mGeoSearchId, firstPoi);
                    } else {
                        hasProcess = false;
                    }
                    mGeoSearchId = -1;
                } else if (mRouteRequestAfterSearch) {
                    //静默搜索结果发起路线规划
                    Logger.d(TAG, "onSilentSearchResult: mRouteRequestAfterSearch = true");
                    mRouteRequestAfterSearch = false;
                    if (success && null != firstPoi) {
                        Logger.d(TAG, "onSilentSearchResult: null != poiInfo");
                        processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", firstPoi);
                    } else {
                        hasProcess = false;
                    }
                } else if (mCommonSearchId == taskId) {
                    //nearbySearch
                    mCommonSearchId = -1;
                    if (success) {
                        dispatchSearchSuccess(true, searchResultEntity);
                    } else {
                        hasProcess = false;
                    }
                }

                if (!hasProcess) {
                    dispatchSearchFailed(true, errorCode);
                }
            }
        };
    }

    /**
     * 分发搜索失败回调.
     *
     * @param silent    boolean，是否静默搜索.
     * @param errorCode 错误码.
     */
    private void dispatchSearchFailed(final boolean silent, final int errorCode) {
        try {
            final int count = mSearchCallbackList.beginBroadcast();
            Logger.d(TAG, "dispatchSearchFailed: errorCode = " + errorCode);
            for (int i = 0; i < count; i++) {
                final INaviAutoSearchCallback searchCallback = mSearchCallbackList.getRegisteredCallbackItem(i);
                if (null != searchCallback) {
                    try {
                        searchCallback.onSearchFailed(silent, errorCode);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeSearchCallback();
        }
    }

    /**
     * 关闭搜索结果回调.
     */
    private void closeSearchCallback() {
        try {
            mSearchCallbackList.finishBroadcast();
        } catch (IllegalStateException exception) {
            Logger.e(TAG, "finishLocationBroadcast error: " + exception.getMessage());
        }
    }

    /**
     * 分发搜索成功回调.
     *
     * @param silent             true-静默搜索  false-非静默搜索
     * @param searchResultEntity SearchResultEntity，搜索结果实体类.
     */
    private void dispatchSearchSuccess(final boolean silent, final SearchResultEntity searchResultEntity) {
        try {
            Logger.d(TAG, "onSearchSuccess inCallback, silent: " + silent);
            sortSearchResult(searchResultEntity);
            final int count = mSearchCallbackList.beginBroadcast();
            final BaseSearchResult baseSearchResult = GsonUtils.convertToT(searchResultEntity, BaseSearchResult.class);
            final String searchResultStr = GsonUtils.toJson(baseSearchResult);
            for (int i = 0; i < count; i++) {
                final INaviAutoSearchCallback searchCallback = mSearchCallbackList.getRegisteredCallbackItem(i);
                if (null != searchCallback) {
                    try {
                        searchCallback.onSearchResult(silent, searchResultStr);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch searchSuccess error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeSearchCallback();
        }
    }

    /**
     * 按照距离对搜索结果进行排序.
     *
     * @param searchResultEntity 搜索结果.
     */
    private void sortSearchResult(final SearchResultEntity searchResultEntity) {
        if (null == searchResultEntity || null == searchResultEntity.getPoiList()
                || searchResultEntity.getPoiList().isEmpty() || searchResultEntity.getPoiList().size() < 2) {
            return;
        }

        final List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
        final int size = poiList.size();
        for (int index = 0; index < size; index++) {
            final PoiInfoEntity searchPoi = poiList.get(index);
            if (null == searchPoi || null == searchPoi.getPoint()) {
                continue;
            }

            if (TextUtils.isEmpty(searchPoi.getDistance()) || DIST_ZERO.equals(searchPoi.getDistance())
                    || searchPoi.getSort_distance() <= 0) {
                final int distance = SearchPackage.getInstance().calcStraightDistanceWithInt(searchPoi.getPoint());
                searchPoi.setSort_distance(distance);
                final String[] distanceArray = ConvertUtils.formatEnDistanceArray(AppCache.getInstance().getMContext(), distance);
                searchPoi.setDistance(distanceArray[0] + distanceArray[1]);
            }
        }

        poiList.sort(Comparator.comparingInt(PoiInfoEntity::getSort_distance));
    }

    /**
     * 处理逆地址搜索回调结果.
     *
     * @param taskId  搜索接口返回的唯一任务标识.
     * @param poiInfo 搜索结果.
     */
    private void dispatchReverseSearch(final int taskId, final PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Logger.e(TAG, "reverse info empty");
            return;
        }

        try {
            Logger.d(TAG, "onReverseSearch inCallback");
            final int count = mSearchCallbackList.beginBroadcast();
            final BaseSearchPoi baseSearchPoi = GsonUtils.convertToT(poiInfo, BaseSearchPoi.class);
            final BaseCityInfo cityInfo = baseSearchPoi.getCityInfo();
            if (null != cityInfo) {
                final int cityCode = cityInfo.getCityCode();
                final int bdCode = ExportConvertUtil.getInstance().cityIdConvert(cityCode);
                cityInfo.setCityCode(bdCode);
            }
            final String singlePoiStr = GsonUtils.toJson(baseSearchPoi);
            for (int i = 0; i < count; i++) {
                final INaviAutoSearchCallback searchCallback = mSearchCallbackList.getRegisteredCallbackItem(i);
                if (null != searchCallback) {
                    try {
                        searchCallback.onReverseGeoSearchResult(taskId, singlePoiStr);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch reverseSearch error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeSearchCallback();
        }
    }

    /**
     * 初始化Map状态回调.
     */
    private void initNaviStatusCallback() {
        mNaviStatusCallback = naviStatus -> {
            int guidePanelStatus = INaviConstant.GuidePanelStatus.NOT_IN_NAVIGATION;
            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(naviStatus)) {
                Logger.d(TAG, "获取到进入导航状态");
                mGuideStatusHolder = ThreadManager.getInstance().asyncDelayWithResult(this::dispatchNaviStartDelay, 300);
                sendDestinationInfo();
                guidePanelStatus = INaviConstant.GuidePanelStatus.COMMON_NAVIGATION;
            }
            boolean guidePanelChanged = false;
            if (mGuidePanelStatus != guidePanelStatus) {
                mGuidePanelStatus = guidePanelStatus;
                guidePanelChanged = true;
            }
            try {
                Logger.d(TAG, "onNaviStatusChange inCallback " + naviStatus);
                final int count = mStatusCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    final INaviAutoStatusCallback statusCallback = mStatusCallbackList.getRegisteredCallbackItem(i);
                    if (null != statusCallback) {
                        try {
                            statusCallback.onNaviStatusChange(naviStatus);
                            if (guidePanelChanged) {
                                statusCallback.onPanelData(mGuidePanelStatus);
                            }
                        } catch (RemoteException exception) {
                            Logger.e(TAG, "dispatch naviStatus or panel error: " + exception.getMessage());
                        }
                    }
                }
            } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
                Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
            } finally {
                closeNaviStatusCallback();
            }
        };
    }

    /**
     * 进入导航后分发一次目的地信息
     */
    private void sendDestinationInfo() {
        if (mInRouteCallBack) {
            Logger.e(TAG, "already in route callback broadcast, can't process tbt ");
            return;
        }

        try {
            mInRouteCallBack = true;
            Logger.d(TAG, "sendDestinationInfo inCallback");
            final int count = mRouteCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoRouteCallback routeCallback = mRouteCallbackList.getRegisteredCallbackItem(i);
                if (null != routeCallback) {
                    try {
                        final BaseDestInfo destInfo = getBaseDestInfo();
                        if (destInfo != null) {
                            routeCallback.onDestChanged(GsonUtils.toJson(destInfo));
                        }
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch destInfo error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeRouteCallback();
        }
    }

    /**
     * 关闭mStatusCallbackList.
     */
    private void closeNaviStatusCallback() {
        try {
            mStatusCallbackList.finishBroadcast();
        } catch (IllegalStateException finishException) {
            Logger.e(finishException.getMessage() + Arrays.toString(finishException.getStackTrace()));
        }
    }

    /**
     * 初始化路线规划接口回调.
     */
    private void initRouteCallback() {
        mRouteResultObserver = new IRouteResultObserver() {
            @Override
            public void onRouteResult(final RequestRouteResult requestRouteResult) {
                if (isNaviStatus(INNER_CLIENT)) {
                    return;
                }
                if (mInRouteCallBack) {
                    Logger.e(TAG, "already in route callback broadcast, can't process tbt");
                    return;
                }
                final BaseRouteResult baseRouteResult = convertToBaseResult(requestRouteResult);
                if (null == baseRouteResult) {
                    return;
                }
                try {
                    mInRouteCallBack = true;
                    Logger.d(TAG, "onRouteResult inCallback");
                    final int count = mRouteCallbackList.beginBroadcast();
                    final String routeResultStr = GsonUtils.toJson(baseRouteResult);
                    for (int i = 0; i < count; i++) {
                        final INaviAutoRouteCallback routeCallback = mRouteCallbackList.getRegisteredCallbackItem(i);
                        if (null != routeCallback) {
                            try {
                                routeCallback.onRoutePlanResult(routeResultStr);
                                final BaseDestInfo destInfo = getBaseDestInfo();
                                if (destInfo != null) {
                                    routeCallback.onDestChanged(GsonUtils.toJson(destInfo));
                                }
                            } catch (RemoteException exception) {
                                Logger.e(TAG, "dispatch routeResult error: " + exception.getMessage());
                            }
                        }
                    }
                } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
                    Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
                } finally {
                    closeRouteCallback();
                }
            }

            @Override
            public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
                if (isNaviStatus(INNER_CLIENT)) {
                    return;
                }
                if (mInRouteCallBack) {
                    Logger.e(TAG, "already in route callback broadcast, can't process tbt");
                    return;
                }
                try {
                    mInRouteCallBack = true;
                    Logger.d(TAG, "onRouteFail inCallback");
                    final int count = mRouteCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        final INaviAutoRouteCallback routeCallback = mRouteCallbackList.getRegisteredCallbackItem(i);
                        if (null != routeCallback) {
                            try {
                                routeCallback.onRoutePlanFailed(-1, errorMsg);
                            } catch (RemoteException exception) {
                                Logger.e(TAG, "dispatch routeFailed error: " + exception.getMessage());
                            }
                        }
                    }
                } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
                    Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
                } finally {
                    closeRouteCallback();
                }
            }
        };
    }

    /**
     * 获取更新后的目的地信息
     *
     * @return BaseDestInfo
     */
    private BaseDestInfo getBaseDestInfo() {
        final RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null != routeParam) {
            final BaseDestInfo destInfo = new BaseDestInfo();
            destInfo.setName(routeParam.getName());
            destInfo.setAddress(routeParam.getAddress());
            if (null != routeParam.getRealPos()) {
                final BaseGeoPoint location = new BaseGeoPoint(routeParam.getRealPos().getLon(), routeParam.getRealPos().getLat());
                destInfo.setLocation(location);
            }
            Logger.i(TAG, "getDestInfo");
            return destInfo;
        } else {
            Logger.i(TAG, "EndPoint is null");
            return null;
        }
    }

    /**
     * 关闭路线规划结果回调.
     */
    private void closeRouteCallback() {
        try {
            mRouteCallbackList.finishBroadcast();
            mInRouteCallBack = false;
        } catch (IllegalStateException exception) {
            Logger.e(TAG, "finishRouteBroadcast error: " + exception.getMessage());
        }
    }


    private String transformBeanDefine(final String orig) {
        if (ConvertUtils.isEmpty(orig)) {
            Logger.d(TAG, "empty orig bean");
            return "";
        }
        final List<PoiInfoForExport> poiList = GsonUtils.fromJson2List(orig,new TypeToken<List<PoiInfoForExport>>(){}.getType());
        final List<BaseFsaPoiInfo> poiExportList = new ArrayList<>();
        for (int i = 0; i < poiList.size(); ++i) {
            final BaseFsaPoiInfo temp = new BaseFsaPoiInfo();
            temp.setName(poiList.get(i).getName());
            temp.setLocation(new BaseGeoPoint(
                    poiList.get(i).getLocation().getLng(),
                    poiList.get(i).getLocation().getLat())
            );
            poiExportList.add(temp);
        }
        Logger.d(TAG, "Export POI size : ", poiExportList.size());
        return GsonUtils.toJson(poiExportList);
    }

    private void handleExportEvent(final int eventId, final String eventStr) {
        try {
            Logger.d(TAG, "onPoiInform in broadcast = ", eventId);
            final int count = mPoiCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoPoiCallBack poiCallback = mPoiCallbackList.getRegisteredCallbackItem(i);
                if (null != poiCallback) {
                    try {
                        switch (eventId) {
                            case FsaConstant.FsaFunction.ID_CHARGING_STATIONS_POI:
                                poiCallback.onChargingStationInform(eventStr);
                                break;
                            case FsaConstant.FsaFunction.ID_PARKING_LOT_POI:
                                poiCallback.onParkingLotInform(eventStr);
                                break;
                            case FsaConstant.FsaFunction.ID_SERVICE_POI:
                                poiCallback.onSAPAInform(eventStr);
                                break;
                            case FsaConstant.FsaFunction.ID_GAS_STATION_POI:
                                poiCallback.onGasStationInform(eventStr);
                                break;
                            default:
                                Logger.e(TAG, "unKnown eventId");
                                break;
                        }
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch poi inform error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closePoiInformCallback();
        }
    }

    private void closePoiInformCallback() {
        if (null != mPoiCallbackList) {
            try {
                mPoiCallbackList.finishBroadcast();
            } catch (IllegalStateException illegalStateException) {
                Logger.e(TAG, "finishPoiBroadcast error: " + illegalStateException.getMessage());
            }
        }
    }

    /**
     * 转换路线规划结果.
     *
     * @param routeResult RequestRouteResult Package返回的路线规划结果.
     * @return BaseRouteResult export对外传递的数据.
     */
    private BaseRouteResult convertToBaseResult(final RequestRouteResult routeResult) {
        if (null == routeResult || null == routeResult.getMRouteLineInfos()
                || routeResult.getMRouteLineInfos().isEmpty()) {
            return null;
        }

        final BaseRouteResult baseRouteResult = new BaseRouteResult();
        baseRouteResult.setOnlineRoute(routeResult.isMIsOnlineRoute());
        final int mapType = getOutMapType(routeResult.getMMapTypeId());
        baseRouteResult.setMapId(mapType);
        baseRouteResult.setFastNavi(routeResult.isMFastNavi());
        baseRouteResult.setRequestId(routeResult.getMRequestId());
        final ArrayList<BaseRouteLine> routeLineList = new ArrayList<>();
        final int pathSize = routeResult.getMRouteLineInfos().size();
        for (int i = 0; i < pathSize; i++) {
            final RouteLineInfo routePath = routeResult.getMRouteLineInfos().get(i);
            if (null == routePath) {
                continue;
            }
            final BaseRouteLine baseRouteLine = new BaseRouteLine();
            baseRouteLine.setPathID(routePath.getMPathID());
            baseRouteLine.setType(routePath.getMType());
            baseRouteLine.setLabel(routePath.getMLabel());
            baseRouteLine.setDistance(routePath.getMDistance());
            baseRouteLine.setLength(routePath.getMLength());
            baseRouteLine.setTravelTime(routePath.getMTravelTime());
            baseRouteLine.setStaticTravelTime(routePath.getMStaticTravelTime());
            baseRouteLine.setTollCost(routePath.getMTollCost());
            baseRouteLine.setNaviID(routePath.getMNaviID());
            baseRouteLine.setElecRouteBool(routePath.isMElecRouteBool());
            baseRouteLine.setIsOnline(routePath.isMIsOnline());
            baseRouteLine.setCanBeArrive(routePath.isMCanBeArrive());
            routeLineList.add(baseRouteLine);
        }
        baseRouteResult.setRouteLineInfos(routeLineList);

        return baseRouteResult;
    }

    /**
     * 初始化引导信息回调.
     */
    private void initNaviInfoCallback() {
        mGuidanceObserver = new IGuidanceObserver() {

            @Override
            public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
                if (null == naviETAInfo) {
                    Logger.e(TAG, "NaviETAInfo is null, can't process tbt");
                    return;
                }
                mBaseTurnInfo = GsonUtils.convertToT(naviETAInfo, BaseTurnInfo.class);
                mBaseTurnInfo.setCurRoadClass(naviETAInfo.getCurRoadClass());
                mBaseTurnInfo.setSRManeuverID(convertTurnIDType(naviETAInfo.getCurManeuverID()));
                if (naviETAInfo.NaviInfoData != null
                        && naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag) != null
                        && naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag).segmentRemain != null) {
                    mBaseTurnInfo.setNextDist(naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag).segmentRemain.dist);
                }
                Logger.d(TAG, "nextDist:" + mBaseTurnInfo.getNextDist());
                formatEtaInfo();
                dispatchTurnInfo();
            }

            @Override
            public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
                if (null != naviTmcInfo && null != naviTmcInfo.getLightBarDetail()) {
                    mTmcTotalDistance = naviTmcInfo.getLightBarDetail().getTotalDistance();
                    mTmcFinishDistance = naviTmcInfo.getLightBarDetail().getFinishDistance();
                }
            }

            @Override
            public void onCurrentRoadSpeed(final int speed) {
                Logger.d(TAG, "receiveSpeedLimit: " + speed);
                dispatchRoadSpeed(speed);
            }

            @Override
            public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdownEntity> lightInfoList) {
                Logger.d(TAG, "update countdown light");
                dispatchCountdownLightInfo(lightInfoList);
            }

            @Override
            public void onNaviArrive(final long traceId, final int naviType) {
                mBaseTurnInfo = null;
                Logger.d(TAG, "onNaviArrival");
                closeScheduleTask();
                dispatchArrival();
            }

            @Override
            public void onNaviStop() {
                mBaseTurnInfo = null;
                closeScheduleTask();
                dispatchNaviStop();
            }

            @Override
            public void onCloseNavi(final boolean isNaviClose) {
                if (isNaviClose) {
                    dispatchNaviManualStop();
                } else {
                    Logger.d(TAG, "非关闭导航信号，不对外发送");
                }
            }
        };
    }

    /**
     * 打断开始导航五分钟后通知的任务
     */
    private void closeScheduleTask() {
        if (!ConvertUtils.isEmpty(mGuideStatusHolder)) {
            Logger.d(TAG, "closeScheduleTask");
            mGuideStatusHolder.cancel(true);
            mGuideStatusHolder = null;
        }
    }

    /**
     * 格式化ETA信息.
     */
    private void formatEtaInfo() {
        //传入格式化之后的剩余时间、剩余距离、预计到达
        final String[] etaDistance = ConvertUtils.formatDistanceArray(
                AppCache.getInstance().getMContext(), mBaseTurnInfo.getRemainDist());
        final StringBuilder builder = new StringBuilder();
        if (etaDistance.length > 0) {
            builder.append(etaDistance[0]);
            if (etaDistance.length > 1) {
                builder.append(etaDistance[1]);
            }
        }
        final String formatDist = builder.toString();
        mBaseTurnInfo.setFormatDist(formatDist);
        builder.setLength(0);

        if (0 >= mBaseTurnInfo.getDriveDist()) {
            mBaseTurnInfo.setDriveDist(mTmcFinishDistance);
        }
        mBaseTurnInfo.setTotalDist(mTmcTotalDistance);
        final long totalTime = TimeUtils.getInstance().getCurrentSecondS() + mBaseTurnInfo.getRemainTime();
        mBaseTurnInfo.setArriveTime(totalTime);

        final int remainTime = mBaseTurnInfo.getRemainTime();
        final String formatTime = TimeUtils.switchHourAndMimuteFromSecond(
                AppCache.getInstance().getMContext(), remainTime);
        mBaseTurnInfo.setFormatTime(formatTime);

        final String arrivalTime = TimeUtils.getArriveTime(
                AppCache.getInstance().getMContext(), remainTime);
        mBaseTurnInfo.setFormatArrive(arrivalTime);
        final String arrivalDay = TimeUtils.getArriveDay(remainTime);
        mBaseTurnInfo.setFormatDay(arrivalDay);
    }

    /**
     * 分发NaviInfo.
     */
    private void dispatchTurnInfo() {
        if (mInCallback) {
            Logger.e(TAG, "already in broadcast, can't process tbt");
            return;
        }

        try {
            mInCallback = true;
            Logger.d(TAG, "onNaviInfo inCallback");
            final int count = mNaviAutoCallbackList.beginBroadcast();
            final String turnInfo = GsonUtils.toJson(mBaseTurnInfo);
            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onTurnInfoChange(turnInfo);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch EtaInfo error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeNavigationList();
        }
    }

    /**
     * 分发当前道路限速信息.
     *
     * @param speed 当前道路限速值，km/h.
     */
    private void dispatchRoadSpeed(final int speed) {
        try {
            Logger.d(TAG, "onCurrentRoadSpeed inCallback");
            final int count = mSpeedCallbackList.beginBroadcast();
            //当前定位车速
            int curSpeed = 0;
            if (null != mLocationInfo) {
                curSpeed = (int) mLocationInfo.getSpeed();
            } else {
                final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                if (null != locInfoBean) {
                    curSpeed = (int) locInfoBean.getSpeed();
                }
            }
            Logger.d(TAG, "onCurrentRoadSpeed: curSpeed:" + curSpeed + ", speed:" + speed);
            for (int i = 0; i < count; i++) {
                final INaviAutoSpeedCallBack naviAutoSpeedCallback = mSpeedCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoSpeedCallback) {
                    try {
                        naviAutoSpeedCallback.onSpeedLimitChange(curSpeed, speed);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch naviStatusChane error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeSpeedStatusCallback();
        }
    }

    /**
     * 关闭mSpeedCallbackList.
     */
    private void closeSpeedStatusCallback() {
        try {
            mSpeedCallbackList.finishBroadcast();
        } catch (IllegalStateException finishException) {
            Logger.e(finishException.getMessage() + Arrays.toString(finishException.getStackTrace()));
        }
    }

    /**
     * 分发手动停止导航
     */
    private void dispatchNaviManualStop() {
        try {
            Logger.d(TAG, "dispatchNaviManualStop inCallback");
            final int count = mStatusCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoStatusCallback statusCallback = mStatusCallbackList.getRegisteredCallbackItem(i);
                if (null != statusCallback) {
                    try {
                        statusCallback.onNaviManualStop();
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch NaviManualStop error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeNaviStatusCallback();
        }
    }

    /**
     * 导航开始五分钟后通知
     */
    private void dispatchNaviStartDelay() {
        try {
            Logger.d(TAG, "5min pasted dispatchNaviStartDelay inCallback");
            final int count = mStatusCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoStatusCallback statusCallback = mStatusCallbackList.getRegisteredCallbackItem(i);
                if (null != statusCallback) {
                    try {
                        statusCallback.onNaviStartAfterFiveMinutes();
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch NaviStartDelay error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeNaviStatusCallback();
            closeScheduleTask();
        }
    }

    /**
     * 分发红绿灯倒计时信息.
     *
     * @param lightInfoList TrafficLightCountdownEntity
     */
    private void dispatchCountdownLightInfo(final ArrayList<TrafficLightCountdownEntity> lightInfoList) {
        if (null == lightInfoList || lightInfoList.isEmpty()) {
            Logger.w(TAG, "countdown light is empty");
            return;
        }

        try {
            Logger.d(TAG, "countdownLightInfo inCallback");
            final String lightInfoStr = GsonUtils.toJson(lightInfoList);
            final int count = mCountDownLightCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoCountDownLightCallback lightCallback = mCountDownLightCallbackList.getRegisteredCallbackItem(i);
                if (null != lightCallback) {
                    try {
                        lightCallback.onCountDownLightInfo(lightInfoStr);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch naviStatus or panel error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeCountDownLightCallback();
        }
    }

    /**
     * 关闭红绿灯远程回调列表.
     */
    private void closeCountDownLightCallback() {
        try {
            mCountDownLightCallbackList.finishBroadcast();
        } catch (IllegalStateException illegalStateException) {
            Logger.i(TAG, "closeCountDownLightCallback error");
        }
    }

    /**
     * 分发引导到达目的地信息信息.
     */
    private void dispatchArrival() {

        try {
            Logger.d(TAG, "onNaviArrival inCallback");
            final int count = mCountDownLightCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoCountDownLightCallback countDownLightCallback = mCountDownLightCallbackList.getBroadcastItem(i);
                if (null != countDownLightCallback) {
                    try {
                        countDownLightCallback.onNaviArrival();
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch onNaviArrival error:" + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeCountDownLightCallback();
        }
    }

    /**
     * 分发导航停止信息.
     */
    private void dispatchNaviStop() {
        try {
            Logger.d(TAG, "onNaviStop inCallback");
            final int count = mCountDownLightCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoCountDownLightCallback countDownLightCallback = mCountDownLightCallbackList.getBroadcastItem(i);
                if (null != countDownLightCallback) {
                    try {
                        countDownLightCallback.onNaviStop();
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch onNaviArrival error:" + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeCountDownLightCallback();
        }
    }

    /**
     * 将内部定义的Map类型转为对外的int.
     *
     * @param mapTypeId MapTypeId.
     * @return int。定义在Constant中的int常量.
     */
    private int getOutMapType(final MapType mapTypeId) {
        return switch (mapTypeId) {
            case MAIN_SCREEN_MAIN_MAP -> INaviConstant.MapType.MAIN;
            case LAUNCHER_DESK_MAP -> INaviConstant.MapType.LAUNCHER_DESK;
            case WIDGET_MAP -> INaviConstant.MapType.WIDGET;
            case HUD_MAP -> INaviConstant.MapType.HUD_WIDGET;
            case CLUSTER_MAP -> INaviConstant.MapType.CLUSTER_WIDGET;
            case LAUNCHER_WIDGET_MAP -> INaviConstant.MapType.LAUNCHER_WIDGET;
            case REAR_SCREEN_MAP -> INaviConstant.MapType.REAR_SCREEN_MAP;
        };
    }

    /**
     * 初始化设置项改变回调.
     */
    private void initSettingCallback() {
        mSettingChangeCallback = new SettingPackage.SettingChangeCallback() {
            @Override
            public void onSettingChanged(final String key, final String value) {
                if (SettingController.KEY_SETTING_VOICE_MUTE.equals(key)) {
                    switch (value) {
                        case SettingController.VALUE_VOICE_MUTE_ON:
                            //声音打开
                            dispatchNaviBroadcastStatus(true);
                            break;
                        case SettingController.VALUE_VOICE_MUTE_OFF:
                            //声音关闭
                            dispatchNaviBroadcastStatus(false);
                            break;
                        default:
                            break;
                    }
                }
            }
        };
    }

    /**
     * 对外分发导航播报状态
     *
     * @param open true-有播报  false-静音.
     */
    private void dispatchNaviBroadcastStatus(final boolean open) {
        try {
            Logger.d(TAG, "5min pasted dispatchNaviStartDelay inCallback");
            final int count = mStatusCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                final INaviAutoStatusCallback statusCallback = mStatusCallbackList.getRegisteredCallbackItem(i);
                if (null != statusCallback) {
                    try {
                        statusCallback.onNaviBroadcastStatus(open);
                    } catch (RemoteException exception) {
                        Logger.e(TAG, "dispatch NaviStartDelay error: " + exception.getMessage());
                    }
                }
            }
        } catch (IllegalStateException | ArrayIndexOutOfBoundsException exception) {
            Logger.e(exception.getMessage() + Arrays.toString(exception.getStackTrace()));
        } finally {
            closeNaviStatusCallback();
        }
    }

    @Override
    public void addNaviAutoApiCallback(final String pkgName, final INaviAutoApiCallback naviAutoApiCallback) {
        mNaviAutoCallbackList.register(naviAutoApiCallback, pkgName);
    }

    @Override
    public void removeNaviAutoApiCallback(final String pkgName, final INaviAutoApiCallback naviAutoApiCallback) {
        mNaviAutoCallbackList.unregister(naviAutoApiCallback);
    }

    @Override
    public void addNaviAutoLocationCallback(final String pkgName, final INaviAutoLocationCallback naviAutoLocationCallback) {
        if (null != naviAutoLocationCallback) {
            mLocationCallbackList.register(naviAutoLocationCallback, pkgName);
        }
    }

    @Override
    public void removeNaviAutoLocationCallback(final String pkgName, final INaviAutoLocationCallback naviAutoLocationCallback) {
        if (null != naviAutoLocationCallback) {
            mLocationCallbackList.unregister(naviAutoLocationCallback);
        }
    }

    @Override
    public void addNaviAutoRouteCallback(final String pkgName, final INaviAutoRouteCallback naviAutoRouteCallback) {
        if (null != naviAutoRouteCallback) {
            mRouteCallbackList.register(naviAutoRouteCallback, pkgName);
        }
    }

    @Override
    public void removeNaviAutoRouteCallback(final String pkgName, final INaviAutoRouteCallback naviAutoRouteCallback) {
        if (null != naviAutoRouteCallback) {
            mRouteCallbackList.unregister(naviAutoRouteCallback);
        }
    }

    @Override
    public void addNaviAutoSearchCallback(final String pkgName, final INaviAutoSearchCallback naviAutoSearchCallback) {
        if (null != naviAutoSearchCallback) {
            mSearchCallbackList.register(naviAutoSearchCallback, pkgName);
        }
    }

    @Override
    public void removeNaviAutoSearchCallback(final String pkgName, final INaviAutoSearchCallback naviAutoSearchCallback) {
        if (null != naviAutoSearchCallback) {
            mSearchCallbackList.unregister(naviAutoSearchCallback);
        }
    }

    @Override
    public void addNaviAutoStatusCallback(final String pkgName, final INaviAutoStatusCallback naviAutoStatusCallback) {
        if (null != naviAutoStatusCallback) {
            mStatusCallbackList.register(naviAutoStatusCallback, pkgName);
        }
    }

    @Override
    public void removeNaviAutoStatusCallback(final String pkgName, final INaviAutoStatusCallback naviAutoStatusCallback) {
        if (null != naviAutoStatusCallback) {
            mStatusCallbackList.unregister(naviAutoStatusCallback);
        }
    }

    @Override
    public void addNaviAutoSpeedCallBack(final String pkgName, final INaviAutoSpeedCallBack naviAutoSpeedCallBack) {
        if (null != naviAutoSpeedCallBack) {
            mSpeedCallbackList.register(naviAutoSpeedCallBack, pkgName);
        }
    }

    @Override
    public void removeNaviAutoSpeedCallBack(final String pkgName, final INaviAutoSpeedCallBack naviAutoSpeedCallBack) {
        if (null != naviAutoSpeedCallBack) {
            mSpeedCallbackList.unregister(naviAutoSpeedCallBack);
        }
    }

    @Override
    public void addNaviAutoPoiCallBack(String pkgName, INaviAutoPoiCallBack naviAutoPoiCallBack) {
        if (null != naviAutoPoiCallBack) {
            mPoiCallbackList.register(naviAutoPoiCallBack);
        }
    }

    @Override
    public void removeNaviAutoPoiCallBack(String pkgName, INaviAutoPoiCallBack naviAutoPoiCallBack) {
        if (null != naviAutoPoiCallBack) {
            mPoiCallbackList.unregister(naviAutoPoiCallBack);
        }
    }

    @Override
    public void addNaviAutoCountDownLightCallback(final String pkgName, final INaviAutoCountDownLightCallback countDownLightCallback) {
        if (null != countDownLightCallback) {
            mCountDownLightCallbackList.register(countDownLightCallback, pkgName);
        }
    }

    @Override
    public void removeNaviAutoCountDownLightCallback(final String pkgName, final INaviAutoCountDownLightCallback countDownLightCallback) {
        if (null != countDownLightCallback) {
            mCountDownLightCallbackList.unregister(countDownLightCallback);
        }
    }

    @Override
    public void openMap(final String clientPkg) {
        final boolean isNaviDesk = FloatViewManager.getInstance().isNaviDeskBg();
        if (Logger.openLog) {
            Logger.i(TAG, clientPkg, " openMap, isNaviDesk", isNaviDesk);
        }
        ProcessManager.restartProcess(AppCache.getInstance().getMContext(), isNaviDesk);
    }

    //直接获取当前位置信息
    @Override
    public String getCurrentLocation(final String clientPkg) {
        Logger.i(TAG, clientPkg + " getCurrentLocation");
        String locationInfo = "";
        try {
            if (null != mLocationInfo) {
                locationInfo = GsonUtils.toJson(mLocationInfo);
            } else {
                final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                locationInfo = GsonUtils.toJson(locInfoBean);
                mLocationInfo = GsonUtils.fromJson(locationInfo, BaseLocationInfo.class);
                final GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                initDistrict(geoPoint);
            }
        } catch (NullPointerException | ClassCastException e) {
            Logger.e(TAG, "getCurrentLocation error: " + e.getMessage());
        }
        return locationInfo;
    }

    //获取行政区划信息
    @Override
    public String getDistrictDetailInfo(final String clientPkg) {
        String districtInfo = null;
        if (null != mDistrictInfo) {
            districtInfo = GsonUtils.toJson(mDistrictInfo);
        }
        if (null == districtInfo || districtInfo.trim().isEmpty()) {
            Logger.w(TAG, "current DistrictInfo is empty");
            if (null == mLocationInfo) {
                getCurrentLocation(TAG);
            } else {
                final GeoPoint geoPoint = new GeoPoint(mLocationInfo.getLongitude(), mLocationInfo.getLatitude());
                mGeoSearchInterval = 0;
                initDistrict(geoPoint);
            }
        }
        return districtInfo;
    }

    @Override
    public void jumpToSearchPage(final String clientPkg, final String keyword) {
        Logger.d(TAG, clientPkg + " jumpSearchPage: " + keyword);
        if (null == keyword || keyword.isEmpty()) {
            Logger.e(TAG, "keyword is empty");
            return;
        }

        mSearchKeyword = keyword;
        final boolean appForeGroundStatus = ProcessManager.isAppInForeground();
        if (appForeGroundStatus) {
            //App已经打开 ，打开地图并通过Package回调打开对应界面
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH);
            bundle.putString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, mSearchKeyword);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
        } else {
            //App未打开状态，打开地图并通过ExportIntentParam保存数据
            processJumpPage(INaviConstant.OpenIntentPage.SEARCH_PAGE, keyword, null);
        }
    }

    @Override
    public int requestReverseGeoSearch(final String clientPkg, final BaseGeoPoint baseGeoPoint) {
        if (null == baseGeoPoint || null == clientPkg || !mReversePkgMap.containsKey(clientPkg)) {
            Logger.e(TAG, "point is empty, can't reverseSearch");
            return -1;
        }

        final Long temp = mReversePkgMap.getOrDefault(clientPkg, 0L);
        final long lastCallMills = null != temp ? temp : 0L;
        final long currentMillis = System.currentTimeMillis();
        Logger.i(TAG, clientPkg, "reverseGeoSearch", lastCallMills, currentMillis);

        if (GALLERY_CLIENT.equals(clientPkg) || currentMillis - lastCallMills > REVERSE_INTERVAL) {
            //如果是图库应用或距上一次调用间隔大于30s
            mReversePkgMap.replace(clientPkg, currentMillis);
            final GeoPoint geoPoint = new GeoPoint(baseGeoPoint.getLon(), baseGeoPoint.getLat());
            mGeoSearchId = SearchPackage.getInstance().geoSearch(geoPoint, true);
            return mGeoSearchId;
        } else {
            return -1;
        }
    }

    @Override
    public void nearbySearch(final String pkgName, final String keyword, final int pageIndex) {
        Logger.i(TAG, pkgName + ": nearbySearch keyword: " + keyword + ", pageIndex: " + pageIndex);
        if (null == keyword || keyword.isEmpty()) {
            return;
        }

        int index = pageIndex;
        if (index < 1) {
            index = 1;
        }

        mSearchKeyword = keyword;
        mCommonSearchId = SearchPackage.getInstance().aroundSearch(index, keyword,
                SearchPackage.getInstance().getCurrentLocation(), "5000", true);
    }

    @Override
    public void searchAndNavi(final String clientPkg, final String address) {
        Logger.i(TAG, clientPkg + ", searchAndNavi:" + address);
        if (null == address || address.isEmpty()) {
            return;
        }

        mRouteRequestAfterSearch = true;
        SearchPackage.getInstance().silentKeywordSearch(1, address);
    }

    @Override
    public void cancelAllSearchRequest(final String clientPkg) {
        Logger.i(TAG, clientPkg + " cancelAllSearch");
        SearchPackage.getInstance().abortSearch();
    }

    @Override
    public void routePlan(final String clientPkg, final BaseSearchPoi baseSearchPoi) {
        if (null == baseSearchPoi || null == baseSearchPoi.getPoint()) {
            Logger.e(TAG, "destination is empty, can't not planRoute");
            return;
        }

        if (ConvertUtils.isEmpty(baseSearchPoi.getName())) {
            Logger.i(TAG, "无名称，使用默认名称");
            baseSearchPoi.setName(DestName);
        }

        final BaseGeoPoint geoPoint = baseSearchPoi.getPoint();
        Logger.i(TAG, clientPkg + ", routePlan, destination: " + geoPoint);
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(baseSearchPoi.getPid());
        poiInfoEntity.setName(baseSearchPoi.getName());
        poiInfoEntity.setAddress(baseSearchPoi.getAddress());
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        final GeoPoint endPoint = new GeoPoint();
        endPoint.setLat(geoPoint.getLat());
        endPoint.setLon(geoPoint.getLon());
        poiInfoEntity.setPoint(endPoint);

        final boolean appForeGroundStatus = ProcessManager.isAppInForeground();
        if (appForeGroundStatus) {
            final RouteSpeechRequestParam requestParam = new RouteSpeechRequestParam();
            requestParam.setMEndPoiInfoEntity(poiInfoEntity);
            requestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            if (isNaviStatus(INNER_CLIENT)) {
                //当前为导航态，更换目的地直接发起快速导航
                RoutePackage.getInstance().requestRouteFromSpeech(requestParam);
            } else {
                //打开算路界面
                final Bundle bundle = new Bundle();
                bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
                bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, requestParam);
                MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            }
        } else {
            processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", poiInfoEntity);
        }
    }

    @Override
    public void startNavi(final String clientPkg) {
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Logger.i(TAG, clientPkg + " startNavigation: " + curStatus);
        if (NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
            processJumpPage(INaviConstant.OpenIntentPage.START_NAVIGATION, "", null);
        } else {
            Logger.e(TAG, "not in SELECT_ROUTE");
        }
    }

    /**
     * 跳转到对应界面.
     *
     * @param pageIntent int，区分目标页面，见INaviConstant.OpenIntentPage.
     * @param keyword    String，关键字搜索参数.
     * @param poiInfo    PoiInfoEntity，路线规划终点.
     */
    private void processJumpPage(final int pageIntent, final String keyword,
                                 final PoiInfoEntity poiInfo) {
        if (null != AppCache.getInstance().getMContext()) {
            try {
                Logger.d(TAG, "processJumpPage");
                ExportIntentParam.setIntentPage(pageIntent);
                if (!ConvertUtils.isEmpty(keyword)) {
                    ExportIntentParam.setKeyword(keyword);
                }
                if (!ConvertUtils.isEmpty(poiInfo)) {
                    ExportIntentParam.setPoiInfo(poiInfo);
                }
                openMap(INNER_CLIENT);
            } catch (NullPointerException exception) {
                Logger.e(TAG, "jumpPage error: " + exception.getMessage());
            }
        }
    }

    @Override
    public boolean isNaviStatus(final String clientPkg) {
        boolean inNavigation = false;
        final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            final String curStatus = naviStatusPackage.getCurrentNaviStatus();
            Logger.d(TAG, "isNaviStatus: " + curStatus);
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
                inNavigation = true;
            }
        }

        return inNavigation;
    }

    @Override
    public int getGuidePanelStatus(final String clientPkg) {
        if (isNaviStatus(TAG)) {
            return INaviConstant.GuidePanelStatus.COMMON_NAVIGATION;
        }
        return INaviConstant.GuidePanelStatus.NOT_IN_NAVIGATION;
    }

    @Override
    public String getTBTInfo(final String pkgName) {
        Logger.i(TAG, pkgName + " getTBTInfo");
        String turnInfoStr = "";
        if (isNaviStatus(pkgName) && null != mBaseTurnInfo) {
            turnInfoStr = GsonUtils.toJson(mBaseTurnInfo);
        }

        return turnInfoStr;
    }

    @Override
    public String getNaviType(final String pkgName) {
        Logger.i(TAG, pkgName + " getNaviType");
        String naviType = NaviStatus.NaviStatusType.NO_STATUS;
        final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            final String curStatus = naviStatusPackage.getCurrentNaviStatus();
            Logger.d(TAG, "getNaviType: " + curStatus);
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
                naviType = curStatus;
            }
        }
        return naviType;
    }

    /**
     * 结束引导信息远程回调.
     */
    private void closeNavigationList() {
        if (null != mNaviAutoCallbackList) {
            try {
                mInCallback = false;
                mNaviAutoCallbackList.finishBroadcast();
            } catch (IllegalStateException illegalStateException) {
                Logger.e(TAG, "finishNavigationBroadcast error: " + illegalStateException.getMessage());
            }
        }
    }

    /**
     * 打开/关闭SR TBT面板.
     *
     * @param pkgName client package name.
     * @param open    true:打开  false:关闭.
     */
    @Override
    public void openSrTbt(final String pkgName, final boolean open) {
        Logger.d(TAG, pkgName + "open: " + open);
        if (open && !isNaviStatus(INNER_CLIENT)) {
            Logger.w(TAG, "not in navigation, can not show sr tbt");
            return;
        }

        try {
            SRFloatWindowService.getInstance().showOrHideFloatView(open);
        } catch (IllegalStateException | IllegalArgumentException |
                 NullPointerException exception) {
            Logger.w(TAG, "controlSrTbt error: " + exception.getMessage());
        }
    }

    /**
     * 停止导航.
     *
     * @param pkgName client package name.
     * @return true:执行成功   false:执行失败.
     */
    @Override
    public boolean stopNavi(final String pkgName) {
        if (Logger.openLog) {
            Logger.d(TAG, pkgName, "stopNavigation");
        }
        boolean result = false;
        if (isNaviStatus(INNER_CLIENT)) {
            result = NaviPackage.getInstance().stopNavigation(true);
        }
        return result;
    }

    @Override
    public void backHome(final String pkgName) {
        if (isNaviStatus(INNER_CLIENT)) {
            return;
        }

        Logger.d(TAG, pkgName + "backHome");
        processJumpPage(INaviConstant.OpenIntentPage.GO_HOME, "", null);
    }

    @Override
    public void goCompany(final String pkgName) {
        if (isNaviStatus(INNER_CLIENT)) {
            return;
        }

        Logger.d(TAG, pkgName + "goCompany");
        processJumpPage(INaviConstant.OpenIntentPage.GO_COMPANY, "", null);
    }

    @Override
    public void openBasicSearch(final String pkgName) {
        if (isNaviStatus(INNER_CLIENT)) {
            return;
        }

        if (!isNaviStatus(INNER_CLIENT)) {
            BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
            if (RoutePackage.getInstance().isRouteState()) {
                RoutePackage.getInstance().clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
            }
            if (!ConvertUtils.isNull(baseActivity)) {
                baseActivity.closeAllFragment();
            }
        }

        Logger.d(TAG, pkgName + "basicSearch");
        processJumpPage(INaviConstant.OpenIntentPage.SEARCH_PAGE, "", null);
    }

    @Override
    public boolean getNaviBroadcastStatus(final String pkgName) {
        return SettingPackage.getInstance().getConfigKeyMute() == 0;
    }

    @Override
    public void toggleNaviBroadcast(final String pkgName, final boolean open) {
        final int muteStatus = SettingPackage.getInstance().getConfigKeyMute();
        Logger.d(TAG, "toggleNaviMute param: " + open + ", current:" + muteStatus);
        if ((open && muteStatus == 0) || (!open && muteStatus == 1)) {
            return;
        }

        NaviPackage.getInstance().setMute(!open);
    }

    @Override
    public void clickPassBySearch(final String pkgName) {
        if (!isNaviStatus(INNER_CLIENT)) {
            Logger.i(TAG, "not in navigation status");
            return;
        }

        Logger.d(TAG, "open PassBy search");
        openMap(INNER_CLIENT);
        NaviPackage.getInstance().exportOpenPassBy(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    /**
     * 转向箭头id转换.
     *
     * @param type TBT类型.
     * @return hudTBT类型.
     */
    private static int convertTurnIDType(final int type) {
        return switch (type) {
            case 0x2 -> 7;
            case 0x3 -> 3;
            case 0x4 -> 8;
            case 0x5 -> 2;
            case 0x6 -> 6;
            case 0x7 -> 4;
            case 0x8 -> 5;
            case 0x9 -> 1;
            case 0xA -> 25;
            case 0xB -> 9;
            case 0xC -> 10;
            case 0xD -> 0;// 服务区
            case 0xE -> 31;
            case 0xF -> 24;
            case 0x15 -> 75;
            case 0x16 -> 71;
            case 0x17 -> 69;
            case 0x18 -> 73;
            case 0x41 -> 11;
            case 0x42 -> 12;
            default -> 0;
        };
    }
}
