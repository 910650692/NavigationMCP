package com.fy.navi.exportservice.binderimpl;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.RemoteCallbackList;
import android.os.RemoteException;
import android.text.TextUtils;
import android.util.Log;

import com.android.utils.gson.GsonUtils;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.mapservice.bean.common.BaseCityInfo;
import com.fy.navi.mapservice.bean.common.BaseDistrictInfo;
import com.fy.navi.mapservice.bean.common.BaseGeoPoint;
import com.fy.navi.mapservice.bean.common.BaseLocationInfo;
import com.fy.navi.mapservice.bean.common.BaseRouteResult;
import com.fy.navi.mapservice.bean.common.BaseSearchPoi;
import com.fy.navi.mapservice.bean.common.BaseSearchResult;
import com.fy.navi.mapservice.bean.common.BaseTurnInfo;
import com.fy.navi.mapservice.common.INaviAutoApiBinder;
import com.fy.navi.mapservice.common.INaviAutoApiCallback;
import com.fy.navi.mapservice.util.ExportConvertUtil;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.DrBean;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocStatus;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.search.CityInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.ui.base.BaseActivity;
import com.fy.navi.ui.base.StackManager;

import java.util.Objects;
import java.util.Timer;
import java.util.TimerTask;


public class NaviAutoApiBinder extends INaviAutoApiBinder.Stub {

    private static final String TAG = NaviAutoApiBinder.class.getSimpleName();
    private final RemoteCallbackList<INaviAutoApiCallback> mNaviAutoCallbackList = new RemoteCallbackList<>();
    /*--------------------------------各个Package对应的回调-----------------------------------------*/
    private IPositionPackageCallback mPositionCallback;
    private SearchResultCallback mSearchResultCallback;
    private NaviStatusCallback mNaviStatusCallback;
    private IRouteResultObserver mRouteResultObserver;
    private IGuidanceObserver mGuidanceObserver;

    private boolean mInCallback;
    private BaseTurnInfo mBaseTurnInfo = null;

    //收到定位改变后发起逆地理搜索获取DistrictInfo
    private int mDistrictSearchId;
    private BaseLocationInfo mLocationInfo;
    private BaseDistrictInfo mDistrictInfo = null;
    private Timer mCountDownTimer;
    private GeoSearchIntervalTask mGeoIntervalTask;
    private int mGeoSearchInterval = 0;

    //当前搜索taskId
    private int mCommonSearchId = -1;
    //收到搜索结果后是否发起算路，对应searchAndNavi接口的需求，
    private boolean mRouteRequestAfterSearch = false;
    private String mSearchKeyword = ""; //关键字

    //当前引导面板状态
    private int mGuidePanelStatus;

    public NaviAutoApiBinder() {
        initPositionCallback();
        initSearchCallback();
        initNaviStatusCallback();
        initRouteCallback();
        initNaviInfoCallback();
        PositionPackage.getInstance().registerCallBack(mPositionCallback);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        SearchPackage.getInstance().registerCallBack("NaviAutoApiBinder",mSearchResultCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, mRouteResultObserver);
        NaviPackage.getInstance().registerObserver(TAG, mGuidanceObserver);
        mGuidePanelStatus = getGuidePanelStatus(TAG);
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
                    Log.e(TAG, "locationInfo is null");
                    return;
                }

                //对外分发位置信息改变
                final String locationData = GsonUtils.toJson(locationInfo);
                mLocationInfo = GsonUtils.fromJson(locationData, BaseLocationInfo.class);
                Log.d(TAG, "onLocationInfo: " + locationData);
                dispatchLocationInfo();

                //收到定位消息后通过逆地理搜索获取DistrictInfo和最近Poi详细信息
                Log.d(TAG, "onLocationInfo: mGeoSearchInterval = " + mGeoSearchInterval);
                final GeoPoint geoPoint = new GeoPoint(locationInfo.getLongitude(), locationInfo.getLatitude());
                initDistrict(geoPoint);
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
     * 初始化搜索回调.
     */
    private void initSearchCallback() {
        mSearchResultCallback = new SearchResultCallback() {
            @Override
            public void onSearchResult(final int taskId, final int errorCode, final String message,
                                       final SearchResultEntity searchResultEntity) {
                final boolean searchSuccess = null != searchResultEntity && null != searchResultEntity.getPoiList()
                        && !searchResultEntity.getPoiList().isEmpty();
                Log.i(TAG, "onSearchResult " + searchSuccess);

                PoiInfoEntity firstResult = null;
                if (searchSuccess) {
                    firstResult = searchResultEntity.getPoiList().get(0);
                }
                //逆地理解析获取行政区域信息，不对外分发
                if (mDistrictSearchId == taskId) {
                    Log.d(TAG, "onSearchResult, mDistrictSearchId == taskId, success: " + searchSuccess);
                    mDistrictSearchId = -1;
                    if (searchSuccess && null != firstResult) {
                        updateDistrictAndLocation(firstResult);
                    }
                    return;
                }

                String keyword = null;
                if (null != searchResultEntity) {
                    keyword = searchResultEntity.getKeyword();
                }
                if (taskId == mCommonSearchId) {
                    Log.d(TAG, "onSearchResult: taskId == mCommonSearchId");
                    if (searchSuccess) {
                        String poiMsg = "empty poi";
                        if (null != firstResult) {
                            poiMsg = (firstResult.getName() + " ,address: " + firstResult.getAddress() + " ,pId: " + firstResult.getPid());
                        }
                        Log.d(TAG, "reverseSearchResult: " + poiMsg);
                        dispatchReverseSearch(mCommonSearchId, firstResult);
                    } else {
                        dispatchSearchFailed(false, errorCode);
                    }
                    mCommonSearchId = -1;
                } else if (Objects.equals(keyword, mSearchKeyword)) {
                    mSearchKeyword = "";
                    if (searchSuccess) {
                        dispatchSearchSuccess(searchResultEntity);
                    } else {
                        dispatchSearchFailed(false, errorCode);
                    }
                }
            }

            @Override
            public void onSilentSearchResult(final int  taskId, final int errorCode, final String message,
                                             final SearchResultEntity searchResultEntity) {
                if (mRouteRequestAfterSearch) {
                    Log.d(TAG, "onSilentSearchResult: mRouteRequestAfterSearch = true");
                    mRouteRequestAfterSearch = false;
                    final boolean success = null != searchResultEntity && null != searchResultEntity.getPoiList()
                            && !searchResultEntity.getPoiList().isEmpty();
                    Log.d(TAG, "onSilentSearchResult success: " + success);
                    if (success) {
                        final PoiInfoEntity poiInfo = searchResultEntity.getPoiList().get(0);
                        if (null != poiInfo) {
                            Log.d(TAG, "onSilentSearchResult: null != poiInfo");
                            processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", poiInfo);
                        }
                    } else {
                        dispatchSearchFailed(true, errorCode);
                    }
                }
            }
        };
    }

    /**
     * 初始化Map状态回调.
     */
    private void initNaviStatusCallback() {
        mNaviStatusCallback = naviStatus -> {
            int guidePanelStatus = INaviConstant.GuidePanelStatus.NOT_IN_NAVIGATION;
            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(naviStatus)) {
                guidePanelStatus = INaviConstant.GuidePanelStatus.COMMON_NAVIGATION;
            }
            boolean guidePanelChanged = false;
            if (mGuidePanelStatus != guidePanelStatus) {
                mGuidePanelStatus = guidePanelStatus;
                guidePanelChanged = true;
            }

            if (mInCallback) {
                Log.e(TAG, "already in broadcast, can't process naviStatusChange");
                return;
            }

            try {
                Log.d(TAG, "onNaviStatusChange inCallback " + naviStatus);
                mInCallback = true;
                final int count = mNaviAutoCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onNaviStatusChange(naviStatus);
                            if (guidePanelChanged) {
                                naviAutoApiCallback.onPanelData(mGuidePanelStatus);
                            }
                        } catch (RemoteException exception) {
                            Log.e(TAG, "dispatch naviStatus or panel error: " + exception.getMessage());
                        }
                    }
                }
            } finally {
                mNaviAutoCallbackList.finishBroadcast();
                mInCallback = false;
            }
        };
    }

    /**
     * 初始化路线规划接口回调.
     */
    private void initRouteCallback() {
        mRouteResultObserver = new IRouteResultObserver() {

            @Override
            public void onRouteResult(final RequestRouteResult requestRouteResult) {
                if (mInCallback) {
                    Log.e(TAG, "already in broadcast, can't process routeResult");
                    return;
                }

                try {
                    mInCallback = true;
                    Log.d(TAG, "onRouteResult inCallback");
                    final int count = mNaviAutoCallbackList.beginBroadcast();
                    final BaseRouteResult baseRouteResult = GsonUtils.convertToT(requestRouteResult, BaseRouteResult.class);
                    final int mapType = getOutMapType(requestRouteResult.getMMapTypeId());
                    baseRouteResult.setMapId(mapType);
                    final String routeResultStr = GsonUtils.toJson(baseRouteResult);
                    Log.d(TAG, "onRouteResult: routeResultStr = " + routeResultStr);
                    for (int i = 0; i < count; i++) {
                        final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                        if (null != naviAutoApiCallback) {
                            try {
                                naviAutoApiCallback.onRoutePlanResult(routeResultStr);
                            } catch (RemoteException exception) {
                                Log.e(TAG, "dispatch routeResult error: " + exception.getMessage());
                            }
                        }
                    }
                } finally {
                    mNaviAutoCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

            @Override
            public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
                if (mInCallback) {
                    Log.e(TAG, "already in broadcast, can't process routeFailed");
                    return;
                }

                try {
                    mInCallback = true;
                    Log.d(TAG, "onRouteFail inCallback");
                    final int count = mNaviAutoCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                        if (null != naviAutoApiCallback) {
                            try {
                                naviAutoApiCallback.onRoutePlanFailed(-1, errorMsg);
                            } catch (RemoteException exception) {
                                Log.e(TAG, "dispatch routeFailed error: " + exception.getMessage());
                            }
                        }
                    }
                } finally {
                    mNaviAutoCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

        };
    }

    /**
     * 初始化引导信息回调.
     */
    private void initNaviInfoCallback() {
        mGuidanceObserver = new IGuidanceObserver() {

            @Override
            public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
                if (null == naviETAInfo) {
                    Log.e(TAG, "NaviETAInfo is null, can't process tbt");
                    return;
                }

                mBaseTurnInfo = GsonUtils.convertToT(naviETAInfo, BaseTurnInfo.class);
                if (mInCallback) {
                    Log.e(TAG, "already in broadcast, can't process tbt");
                    return;
                }

                try {
                    mInCallback = true;
                    Log.d(TAG, "onNaviInfo inCallback");
                    final int count = mNaviAutoCallbackList.beginBroadcast();
                    final String turnInfo = GsonUtils.toJson(mBaseTurnInfo);
                    Log.d(TAG, "onNaviInfo: turnInfo + " + turnInfo);
                    for (int i = 0; i < count; i++) {
                        final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                        if (null != naviAutoApiCallback) {
                            try {
                                naviAutoApiCallback.onTurnInfoChange(turnInfo);
                            } catch (RemoteException exception) {
                                Log.e(TAG, "dispatch EtaInfo error: " + exception.getMessage());
                            }
                        }
                    }
                } finally {
                    mNaviAutoCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

            @Override
            public void onCurrentRoadSpeed(final int speed) {
                Log.d(TAG, "receiveSpeedLimit: " + speed);
                if (mInCallback) {
                    Log.e(TAG, "already in broadcast, can't process roadSpeed");
                    return;
                }

                try {
                    mInCallback = true;
                    Log.d(TAG, "onCurrentRoadSpeed inCallback");
                    final int count = mNaviAutoCallbackList.beginBroadcast();
                    //当前道路限速
                    int curSpeed = 0;
                    if (null != mLocationInfo) {
                        curSpeed = (int) mLocationInfo.getSpeed();
                    } else {
                        final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                        if (null != locInfoBean) {
                            curSpeed = (int) locInfoBean.getSpeed();
                        }
                    }
                    Log.d(TAG, "onCurrentRoadSpeed: curSpeed = " + curSpeed);
                    for (int i = 0; i < count; i++) {
                        final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                        if (null != naviAutoApiCallback) {
                            try {
                                naviAutoApiCallback.onSpeedLimitChange(curSpeed, speed);
                            } catch (RemoteException exception) {
                                Log.e(TAG, "dispatch naviStatusChane error: " + exception.getMessage());
                            }
                        }
                    }
                } finally {
                    mNaviAutoCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

            @Override
            public void onNaviArrive(final long traceId, final int naviType) {
                mBaseTurnInfo = null;
            }

            @Override
            public void onNaviStop() {
                mBaseTurnInfo = null;
            }
        };
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
            Log.d(TAG, "getDistrictInfo by point");
            mGeoSearchInterval = 20;
            mCountDownTimer = new Timer();
            mGeoIntervalTask = new GeoSearchIntervalTask();
            mCountDownTimer.schedule(mGeoIntervalTask, 1000, 1000);
            mDistrictSearchId = searchPackage.geoSearch(geoPoint);
        }
    }

    //收到定位信息后，逆地理搜索倒计时Task
    private final class GeoSearchIntervalTask extends TimerTask {
        @Override
        public void run() {
            countDownInterval();
        }
    }

    /**
     * 获取行政区划信息间隔倒计时.
     */
    private void countDownInterval() {
        mGeoSearchInterval--;
        if (mGeoSearchInterval == 0) {
            if (null != mCountDownTimer) {
                mCountDownTimer.cancel();
            }
            mGeoIntervalTask = null;
        }
    }

    /**
     * 分发定位信息.
     */
    private void dispatchLocationInfo() {
        if (null == mLocationInfo) {
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process locationInfo");
            return;
        }

        try {
            Log.d(TAG, "onLocationInfoChange in broadcast");
            mInCallback = true;
            final int count = mNaviAutoCallbackList.beginBroadcast();
            final String locationData = GsonUtils.toJson(mLocationInfo);
            Log.d(TAG, "dispatchLocationInfo: locationData = " + locationData);
            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onLocationInfoChange(locationData);
                    } catch (RemoteException exception) {
                        Log.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    /**
     * 分发行政区域信息.
     */
    private void dispatchDistrictInfo() {
        Log.d(TAG, "dispatchDistrictInfo");
        if (null == mDistrictInfo) {
            Log.d(TAG, "DistrictInfo is null");
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process districtInfo");
            return;
        }

        try {
            mInCallback = true;
            final int count = mNaviAutoCallbackList.beginBroadcast();
            final String districtData = GsonUtils.toJson(mDistrictInfo);
            Log.d(TAG, "dispatchDistrictInfo: districtData" + districtData);
            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onDistrictInfoChange(districtData);
                    } catch (RemoteException exception) {
                        Log.e(TAG, "dispatch districtInfo error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    /**
     * 根据逆地理搜索结果更新行政区域和位置信息.
     *
     * @param poiInfo PoiInfoEntity，逆地理搜索结果.
     */
    private void updateDistrictAndLocation(final PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Log.e(TAG, "poiInfo is null");
            return;
        }
        if (null != mLocationInfo) {
            mLocationInfo.setName(poiInfo.getName());
            mLocationInfo.setAddress(poiInfo.getAddress());
        }

        final CityInfo cityInfo = poiInfo.getCityInfo();
        Log.d(TAG, "updateDistrictAndLocation: " + cityInfo);
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
     * 分发搜索失败回调.
     *
     * @param silent boolean，是否静默搜索.
     * @param errorCode 错误码.
     */
    private void dispatchSearchFailed(final boolean silent, final int errorCode) {
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process searchFail");
            return;
        }

        try {
            mInCallback = true;
            final int count = mNaviAutoCallbackList.beginBroadcast();
            Log.d(TAG, "dispatchSearchFailed: errorCode = " + errorCode);
            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onSearchFailed(silent, errorCode);
                    } catch (RemoteException exception) {
                        Log.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    /**
     * 分发搜索成功回调.
     *
     * @param searchResultEntity SearchResultEntity，搜索结果实体类.
     */
    private void dispatchSearchSuccess(final SearchResultEntity searchResultEntity) {
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process searchSuccess");
            return;
        }

        try {
            mInCallback = true;
            Log.d(TAG, "onSearchSuccess inCallback");
            final int count = mNaviAutoCallbackList.beginBroadcast();
            final BaseSearchResult baseSearchResult = GsonUtils.convertToT(searchResultEntity, BaseSearchResult.class);
            final String searchResultStr = GsonUtils.toJson(baseSearchResult);
            Log.d(TAG, "dispatchSearchSuccess: searchResultStr = " + searchResultStr);

            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onSearchResult(true, searchResultStr);
                    } catch (RemoteException exception) {
                        Log.e(TAG, "dispatch searchSuccess error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    /**
     * 处理逆地址搜索回调结果.
     *
     * @param taskId 搜索接口返回的唯一任务标识.
     * @param poiInfo 搜索结果.
     */
    private void dispatchReverseSearch(final int taskId, final PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Log.e(TAG, "reverse info empty");
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process reverseResult");
            return;
        }

        try {
            mInCallback = true;
            Log.d(TAG, "onReverseSearch inCallback");
            final int count = mNaviAutoCallbackList.beginBroadcast();
            final BaseSearchPoi baseSearchPoi = GsonUtils.convertToT(poiInfo, BaseSearchPoi.class);
            final BaseCityInfo cityInfo = baseSearchPoi.getCityInfo();
            if (null != cityInfo) {
                final int cityCode = cityInfo.getCityCode();
                final int bdCode = ExportConvertUtil.getInstance().cityIdConvert(cityCode);
                cityInfo.setCityCode(bdCode);
            }
            final String singlePoiStr = GsonUtils.toJson(baseSearchPoi);
            Log.d(TAG, "dispatchReverseSearch: singlePoiStr = " + singlePoiStr);
            for (int i = 0; i < count; i++) {
                final INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onReverseGeoSearchResult(taskId, singlePoiStr);
                    } catch (RemoteException exception) {
                        Log.e(TAG, "dispatch reverseSearch error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
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
            case LAUNCHER_WIDGET_MAP -> INaviConstant.MapType.LAUNCHER_WIDGET;
        };
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
    public void openMap(final String clientPkg) {
        Log.i(TAG, clientPkg + " openMap");
        if (null != AppContext.getInstance().getMContext()) {
            try {
                final String appPkgName = AppContext.getInstance().getMContext().getPackageName();
                final PackageManager packageManager = AppContext.getInstance().getMContext().getPackageManager();
                final Intent launcherIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                if (null != launcherIntent) {
                    launcherIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    AppContext.getInstance().getMContext().startActivity(launcherIntent);
                } else {
                    Log.e(TAG, "can't find map hmi");
                }
            } catch (ActivityNotFoundException exception) {
                Log.e(TAG, "open map error: " + exception.getMessage());
            }
        }
    }

    //直接获取当前位置信息
    @Override
    public String getCurrentLocation(final String clientPkg) {
        Log.i(TAG, clientPkg + " getCurrentLocation");
        final String locationInfo;
        if (null != mLocationInfo) {
            locationInfo = GsonUtils.toJson(mLocationInfo);
        } else {
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            locationInfo = GsonUtils.toJson(locInfoBean);
            mLocationInfo = GsonUtils.fromJson(locationInfo, BaseLocationInfo.class);
            final GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
            initDistrict(geoPoint);
        }

        return locationInfo;
    }

    //获取行政区划信息
    @Override
    public String getDistrictDetailInfo(final String clientPkg) {
        if (null != mDistrictInfo) {
            return GsonUtils.toJson(mDistrictInfo);
        } else {
            Log.e(TAG, "current DistrictInfo is empty");
            if (null == mLocationInfo) {
                getCurrentLocation(TAG);
            }
            if (null != mLocationInfo) {
                final GeoPoint geoPoint = new GeoPoint(mLocationInfo.getLongitude(), mLocationInfo.getLatitude());
                initDistrict(geoPoint);
            }
        }
        return "";
    }

    @Override
    public void jumpToSearchPage(final String clientPkg, final String keyword) {
        Log.d(TAG, clientPkg + " jumpSearchPage: " + keyword);
        if (null == keyword || keyword.isEmpty()) {
            Log.e(TAG, "keyword is empty");
            return;
        }

        processJumpPage(INaviConstant.OpenIntentPage.SEARCH_PAGE, keyword, null);
    }

    @Override
    public int requestReverseGeoSearch(final String clientPkg, final BaseGeoPoint baseGeoPoint) {
        if (null == baseGeoPoint) {
            Log.e(TAG, "point is empty, can't reverseSearch");
            return -1;
        }
        Log.i(TAG, clientPkg + " reverseGeoSearch: " + baseGeoPoint);

        final GeoPoint geoPoint = new GeoPoint(baseGeoPoint.getLon(), baseGeoPoint.getLat());
        mCommonSearchId = SearchPackage.getInstance().geoSearch(geoPoint);
        return mCommonSearchId;
    }

    @Override
    public void searchAndNavi(final String clientPkg, final String address) {
        Log.i(TAG, clientPkg + ", searchAndNavi:" + address);
        if (null == address || address.isEmpty()) {
            return;
        }

        mRouteRequestAfterSearch = true;
        SearchPackage.getInstance().silentKeywordSearch(1, address);
    }

    @Override
    public void cancelAllSearchRequest(final String clientPkg) {
        Log.i(TAG, clientPkg + " cancelAllSearch");
        SearchPackage.getInstance().abortSearch();
    }

    @Override
    public void routePlan(final String clientPkg, final BaseSearchPoi baseSearchPoi) {
        if (null == baseSearchPoi || null == baseSearchPoi.getPoint()) {
            Log.e(TAG, "destination is empty, can't not planRoute");
            return;
        }

        final BaseGeoPoint geoPoint = baseSearchPoi.getPoint();
        Log.i(TAG, clientPkg + ", routePlan, destination: " + geoPoint);
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(baseSearchPoi.getPid());
        poiInfoEntity.setName(baseSearchPoi.getName());
        poiInfoEntity.setAddress(baseSearchPoi.getAddress());
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        final GeoPoint endPoint = new GeoPoint();
        endPoint.setLat(geoPoint.getLat());
        endPoint.setLon(geoPoint.getLon());
        poiInfoEntity.setPoint(endPoint);
        processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", poiInfoEntity);
    }

    @Override
    public void startNavi(final String clientPkg) {
        final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Log.i(TAG, clientPkg + " startNavigation: " + curStatus);
        if (NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
            processJumpPage(INaviConstant.OpenIntentPage.START_NAVIGATION, "", null);
        } else {
            Log.e(TAG, "not in SELECT_ROUTE");
        }
    }

    /**
     * 跳转到对应界面.
     *
     * @param pageIntent int，区分目标页面，见INaviConstant.OpenIntentPage.
     * @param keyword String，关键字搜索参数.
     * @param poiInfo PoiInfoEntity，路线规划终点.
     */
    private void processJumpPage(final int pageIntent, final String keyword,
                                 final PoiInfoEntity poiInfo) {
        if (null != AppContext.getInstance().getMContext()) {
            try {
                Log.d(TAG, "processJumpPage");
                final BaseActivity baseActivity = StackManager.getInstance()
                        .getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
                Intent targetIntent;
                if (null == baseActivity) {
                    final String appPkgName = AppContext.getInstance().getMContext().getPackageName();
                    final PackageManager packageManager = AppContext.getInstance().getMContext().getPackageManager();
                    targetIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                } else {
                    targetIntent= new Intent(AppContext.getInstance().getMContext(), MapActivity.class);
                }
                if (null != targetIntent) {
                    Log.d(TAG, "processJumpPage: null != targetIntent");
                    targetIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    targetIntent.putExtra(INaviConstant.PAGE_EXTRA, pageIntent);
                    switch (pageIntent) {
                        case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                            Log.d(TAG, "processJumpPage: SEARCH_PAGE");
                            mSearchKeyword = keyword;
                            targetIntent.putExtra(INaviConstant.SEARCH_KEYWORD_EXTRA, keyword);
                            break;
                        case INaviConstant.OpenIntentPage.ROUTE_PAGE:
                            Log.d(TAG, "processJumpPage: ROUTE_PAGE");
                            targetIntent.putExtra(INaviConstant.ROUTE_END_POI, poiInfo);
                            break;
                        case INaviConstant.OpenIntentPage.START_NAVIGATION:
                            Log.d(TAG, "processJumpPage: START_NAVIGATION");
                            break;
                        default:
                            targetIntent = null;
                            break;
                    }
                    if (null != targetIntent) {
                        AppContext.getInstance().getMContext().startActivity(targetIntent);
                    }
                }
            } catch (NullPointerException exception) {
                Log.e(TAG, "jumpPage error: " + exception.getMessage());
            } catch (ActivityNotFoundException activityNotFoundException) {
                Log.e(TAG, "jumpPage not found: " + activityNotFoundException.getMessage());
            }
        }
    }

    @Override
    public boolean isNaviStatus(final String clientPkg) {
        boolean inNavigation = false;
        final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            final String curStatus = naviStatusPackage.getCurrentNaviStatus();
            Log.d(TAG, "isNaviStatus: " + curStatus);
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
        Log.i(TAG, pkgName + " getTBTInfo");
        String turnInfoStr = "";
        if (isNaviStatus(pkgName) && null != mBaseTurnInfo) {
            turnInfoStr = GsonUtils.toJson(mBaseTurnInfo);
        }

        return turnInfoStr;
    }

    @Override
    public String getNaviType(final String pkgName) {
        Log.i(TAG, pkgName + " getNaviType");
        String naviType = NaviStatus.NaviStatusType.NO_STATUS;
        final NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            final String curStatus = naviStatusPackage.getCurrentNaviStatus();
            Log.d(TAG, "getNaviType: " + curStatus);
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
                naviType = curStatus;
            }
        }
        return naviType;
    }

}
