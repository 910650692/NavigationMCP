package com.fy.navi.exportservice.binderimpl;

import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.RemoteCallbackList;
import android.os.RemoteException;
import android.text.TextUtils;
import android.util.Log;

import com.android.utils.gson.GsonUtils;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.mapservice.bean.common.BaseDistrictInfo;
import com.fy.navi.mapservice.bean.common.BaseGeoPoint;
import com.fy.navi.mapservice.bean.common.BaseLocationInfo;
import com.fy.navi.mapservice.bean.common.BaseRouteResult;
import com.fy.navi.mapservice.bean.common.BaseSearchPoi;
import com.fy.navi.mapservice.bean.common.BaseSearchResult;
import com.fy.navi.mapservice.bean.common.BaseTurnInfo;
import com.fy.navi.mapservice.common.INaviAutoApiBinder;
import com.fy.navi.mapservice.common.INaviAutoApiCallback;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
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

import kotlinx.coroutines.scheduling.TasksKt;


public class NaviAutoApiBinder extends INaviAutoApiBinder.Stub {

    private static final String TAG = NaviAutoApiBinder.class.getSimpleName();
    private RemoteCallbackList<INaviAutoApiCallback> mNaviAutoCallbackList = new RemoteCallbackList<>();
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
        PositionPackage.getInstance().registerCallBack(mPositionCallback);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        SearchPackage.getInstance().registerCallBack("NaviAutoApiBinder",mSearchResultCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, mRouteResultObserver);
        NaviPackage.getInstance().registerObserver(TAG, mGuidanceObserver);
        mGuidePanelStatus = getGuidePanelStatus("innerDefault");
    }

    //定位信息回调
    private final IPositionPackageCallback mPositionCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            if (null == locationInfo) {
                Log.e(TAG, "locationInfo is null");
                return;
            }

            //对外分发位置信息改变
            String locationData = GsonUtils.toJson(locationInfo);
            mLocationInfo = GsonUtils.fromJson(locationData, BaseLocationInfo.class);
            dispatchLocationInfo();

            //收到定位消息后通过逆地理搜索获取DistrictInfo和最近Poi详细信息
            if (mGeoSearchInterval <= 0) {
                GeoPoint geoPoint = new GeoPoint(locationInfo.getLongitude(), locationInfo.getLatitude());
                initDistrict(geoPoint);
            }
        }

        @Override
        public void onLocationStatus(LocStatus locStatus) {}

        @Override
        public void onDrInfo(DrBean drInfo) {}
    };

    private void initDistrict(GeoPoint geoPoint) {
        SearchPackage searchPackage = SearchPackage.getInstance();
        if (null != searchPackage) {
            mGeoSearchInterval = 20;
            mCountDownTimer = new Timer();
            mGeoIntervalTask = new GeoSearchIntervalTask();
            mCountDownTimer.schedule(mGeoIntervalTask, 1000, 1000);
            mDistrictSearchId = searchPackage.geoSearch(geoPoint);
        }
    }

    //收到定位信息后，逆地理搜索倒计时Task
    private class GeoSearchIntervalTask extends TimerTask {
        @Override
        public void run() {
            countDownInterval();
        }
    }

    private void countDownInterval() {
        mGeoSearchInterval--;
        if (mGeoSearchInterval == 0) {
            if (null != mCountDownTimer) {
                mCountDownTimer.cancel();
            }
            mGeoIntervalTask = null;
        }
    }

    //分发定位信息
    private void dispatchLocationInfo() {
        if (null == mLocationInfo) {
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process locationInfo");
            return;
        }

        try {
            mInCallback = true;
            int count = mNaviAutoCallbackList.beginBroadcast();
            String locationData = GsonUtils.toJson(mLocationInfo);
            for (int i = 0; i < count; i++) {
                INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onLocationInfoChange(locationData);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    //分发行政区域信息
    private void dispatchDistrictInfo() {
        if (null == mDistrictInfo) {
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process districtInfo");
            return;
        }

        try {
            mInCallback = true;
            int count = mNaviAutoCallbackList.beginBroadcast();
            String districtData = GsonUtils.toJson(mDistrictInfo);
            for (int i = 0; i < count; i++) {
                INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onDistrictInfoChange(districtData);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch districtInfo error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    //根据逆地理搜索结果更新行政区域和位置信息
    private void updateDistrictAndLocation(PoiInfoEntity poiInfo) {
        if (null == poiInfo) {
            Log.e(TAG, "poiInfo is null");
            return;
        }
        if (null != mLocationInfo) {
            mLocationInfo.setName(poiInfo.getName());
            mLocationInfo.setAddress(poiInfo.getAddress());
        }

        CityInfo cityInfo = poiInfo.getCityInfo();
        if (null != cityInfo) {
            if (mDistrictInfo == null) {
                mDistrictInfo = new BaseDistrictInfo();
            }
            mDistrictInfo.setProvince(cityInfo.getProvince());
            mDistrictInfo.setProvinceId(cityInfo.getProvinceAdCode());
            mDistrictInfo.setCity(cityInfo.getCityName());
            mDistrictInfo.setCityId(cityInfo.getCityCode());
            mDistrictInfo.setDistrict(cityInfo.getDistrict());
            mDistrictInfo.setDistrictId(cityInfo.getDistrictAdCode());
            dispatchDistrictInfo();
        }
    }

    //搜索结果回调
    private final SearchResultCallback mSearchResultCallback = new SearchResultCallback() {
        @Override

        public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            boolean searchSuccess = true;
            if (null == searchResultEntity || null == searchResultEntity.getPoiList()
                    || searchResultEntity.getPoiList().isEmpty()) {
                searchSuccess = false;
            }

            //逆地理解析获取行政区域信息，不对外分发
            if (mDistrictSearchId == taskId) {
                if (searchSuccess) {
                    PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                    if (null != poiInfoEntity) {
                        updateDistrictAndLocation(poiInfoEntity);
                    }
                }
                mDistrictSearchId = -1;
                return;
            }

            String keyword = null;
            if (null != searchResultEntity) {
                keyword = searchResultEntity.getKeyword();
            }
            if (taskId == mCommonSearchId) {
                if (searchSuccess) {
                    PoiInfoEntity poiInfoEntity = searchResultEntity.getPoiList().get(0);
                    dispatchReverseSearch(mCommonSearchId, poiInfoEntity);
                } else {
                    dispatchSearchFailed(false, errorCode);
                }
                mCommonSearchId = -1;
            } else if (Objects.equals(keyword, mSearchKeyword)) {
                mSearchKeyword = "";
                if (searchSuccess) {
                    dispatchSearchSuccess(false, searchResultEntity);
                } else {
                    dispatchSearchFailed(false, errorCode);
                }
            }
        }

        @Override
        public void onSilentSearchResult(int  taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            if (mRouteRequestAfterSearch) {
                mRouteRequestAfterSearch = false;
                boolean success = true;
                if (null == searchResultEntity || null == searchResultEntity.getPoiList()
                        || searchResultEntity.getPoiList().isEmpty()) {
                    success = false;
                }
                if (success) {
                    PoiInfoEntity poiInfo = searchResultEntity.getPoiList().get(0);
                    if (null != poiInfo) {
                        processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", poiInfo);
                    }
                }
            }
        }
    };

    private void dispatchSearchFailed(boolean silent, int errorCode) {
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process searchFail");
            return;
        }

        try {
            mInCallback = true;
            int count = mNaviAutoCallbackList.beginBroadcast();
            for (int i = 0; i < count; i++) {
                INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onSearchFailed(silent, errorCode);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchFailed error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    private void dispatchSearchSuccess(boolean silent, SearchResultEntity searchResultEntity) {
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process searchSuccess");
            return;
        }

        try {
            mInCallback = true;
            Log.d(TAG, "onSearchSuccess inCallback");
            int count = mNaviAutoCallbackList.beginBroadcast();
            BaseSearchResult baseSearchResult = GsonUtils.convertToT(searchResultEntity, BaseSearchResult.class);
            String searchResultStr = GsonUtils.toJson(baseSearchResult);
            for (int i = 0; i < count; i++) {
                INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onSearchResult(silent, searchResultStr);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchSuccess error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    private void dispatchReverseSearch(int taskId, PoiInfoEntity poiInfo) {
        if (null == poiInfo || TextUtils.isEmpty(poiInfo.getPid())) {
            return;
        }
        if (mInCallback) {
            Log.e(TAG, "already in broadcast, can't process searchSuccess");
            return;
        }

        try {
            mInCallback = true;
            Log.d(TAG, "onReverseSearch inCallback");
            int count = mNaviAutoCallbackList.beginBroadcast();
            BaseSearchPoi baseSearchPoi = GsonUtils.convertToT(poiInfo, BaseSearchPoi.class);
            String singlePoiStr = GsonUtils.toJson(baseSearchPoi);
            for (int i = 0; i < count; i++) {
                INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                if (null != naviAutoApiCallback) {
                    try {
                        naviAutoApiCallback.onReverseGeoSearchResult(taskId, singlePoiStr);
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch searchSuccess error: " + exception.getMessage());
                    }
                }
            }
        } finally {
            mNaviAutoCallbackList.finishBroadcast();
            mInCallback = false;
        }
    }

    //算路结果回调
    private final IRouteResultObserver mRouteResultObserver = new IRouteResultObserver() {

        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            if (mInCallback) {
                Log.e(TAG, "already in broadcast, can't process routeResult");
                return;
            }

            try {
                mInCallback = true;
                Log.d(TAG, "onRouteResult inCallback");
                int count = mNaviAutoCallbackList.beginBroadcast();
                BaseRouteResult baseRouteResult = GsonUtils.convertToT(requestRouteResult, BaseRouteResult.class);
                int mapType = getOutMapType(requestRouteResult.getMapTypeId());
                baseRouteResult.setMapId(mapType);
                String routeResultStr = GsonUtils.toJson(baseRouteResult);

                for (int i = 0; i < count; i++) {
                    INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onRoutePlanResult(routeResultStr);
                        } catch (Exception exception) {
                            Log.e(TAG, "dispatch searchSuccess error: " + exception.getMessage());
                        }
                    }
                }
            } finally {
                mNaviAutoCallbackList.finishBroadcast();
                mInCallback = false;
            }
        }

        @Override
        public void onRouteFail(MapTypeId mapTypeId, String errorMsg) {
            if (mInCallback) {
                Log.e(TAG, "already in broadcast, can't process routeFailed");
                return;
            }

            try {
                mInCallback = true;
                Log.d(TAG, "onRouteFail inCallback");
                int count = mNaviAutoCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onRoutePlanFailed(-1, errorMsg);
                        } catch (Exception exception) {
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

    private int getOutMapType(MapTypeId mapTypeId) {
        int mapType;
        switch (mapTypeId) {
            case MAIN_SCREEN_MAIN_MAP:
                mapType = INaviConstant.MapType.Main;
                break;
            case LAUNCHER_DESK_MAP:
                mapType = INaviConstant.MapType.LauncherDesk;
                break;
            case LAUNCHER_WIDGET_MAP:
                mapType = INaviConstant.MapType.LauncherWidget;
                break;
            default:
                mapType = INaviConstant.MapType.UNKNOWN;
                break;
        }

        return mapType;
    }

    //地图状态改变
    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
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
                Log.d(TAG, "onNaviStatusChange inCallback");
                mInCallback = true;
                int count = mNaviAutoCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onNaviStatusChange(naviStatus);
                            if (guidePanelChanged) {
                                naviAutoApiCallback.onPanelData(mGuidePanelStatus);
                            }
                        } catch (Exception exception) {
                            Log.e(TAG, "dispatch naviStatusChane error: " + exception.getMessage());
                        }
                    }
                }
            } finally {
                mNaviAutoCallbackList.finishBroadcast();
                mInCallback = false;
            }
        }
    };


    //引导信息回调
    private final IGuidanceObserver mGuidanceObserver = new IGuidanceObserver() {

        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
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
                int count = mNaviAutoCallbackList.beginBroadcast();
                String turnInfo = GsonUtils.toJson(mBaseTurnInfo);
                for (int i = 0; i < count; i++) {
                    INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onTurnInfoChange(turnInfo);
                        } catch (Exception exception) {
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
        public void onCurrentRoadSpeed(int speed) {
            Log.d(TAG, "receiveSpeedLimit: " + speed);
            if (mInCallback) {
                Log.e(TAG, "already in broadcast, can't process roadSpeed");
                return;
            }

            try {
                mInCallback = true;
                Log.d(TAG, "onCurrentRoadSpeed inCallback");
                int count = mNaviAutoCallbackList.beginBroadcast();
                //当前道路限速
                int curSpeed = 0;
                if (null != mLocationInfo) {
                    curSpeed = (int) mLocationInfo.getSpeed();
                } else {
                    LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
                    if (null != locInfoBean) {
                        curSpeed = (int) locInfoBean.getSpeed();
                    }
                }
                for (int i = 0; i < count; i++) {
                    INaviAutoApiCallback naviAutoApiCallback = mNaviAutoCallbackList.getRegisteredCallbackItem(i);
                    if (null != naviAutoApiCallback) {
                        try {
                            naviAutoApiCallback.onSpeedLimitChange(curSpeed, speed);
                        } catch (Exception exception) {
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
        public void onNaviArrive(long traceId, int naviType) {
            mBaseTurnInfo = null;
        }

        @Override
        public void onNaviStop() {
            mBaseTurnInfo = null;
        }
    };


    @Override
    public void addNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback) throws RemoteException {
        mNaviAutoCallbackList.register(naviAutoApiCallback, pkgName);
    }

    @Override
    public void removeNaviAutoApiCallback(String pkgName, INaviAutoApiCallback naviAutoApiCallback) throws RemoteException {
        mNaviAutoCallbackList.unregister(naviAutoApiCallback);
    }

    @Override
    public void openMap(String clientPkg) {
        Log.i(TAG, clientPkg + " openMap");
        if (null != AppContext.mContext) {
            try {
                String appPkgName = AppContext.mContext.getPackageName();
                PackageManager packageManager = AppContext.mContext.getPackageManager();
                Intent launcherIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                if (null != launcherIntent) {
                    launcherIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    AppContext.mContext.startActivity(launcherIntent);
                } else {
                    Log.e(TAG, "can't find map hmi");
                }
            } catch (Exception exception) {
                Log.e(TAG, "open map error: " + exception.getMessage());
            }
        }
    }

    //直接获取当前位置信息
    @Override
    public String getCurrentLocation(String clientPkg) {
        Log.i(TAG, clientPkg + " getCurrentLocation");
        String locationInfo = "";
        if (null != mLocationInfo) {
            locationInfo = GsonUtils.toJson(mLocationInfo);
        } else {
            LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            locationInfo = GsonUtils.toJson(locInfoBean);
            mLocationInfo = GsonUtils.fromJson(locationInfo, BaseLocationInfo.class);
            GeoPoint geoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
            initDistrict(geoPoint);
        }

        return locationInfo;
    }

    //获取行政区划信息
    @Override
    public String getDistrictDetailInfo(String clientPkg) {
        if (null != mDistrictInfo) {
            return GsonUtils.toJson(mDistrictInfo);
        }
        return "";
    }

    @Override
    public void jumpToSearchPage(String clientPkg, String keyword) {
        Log.d(TAG, clientPkg + " jumpSearchPage: " + keyword);
        if (null == keyword || keyword.isEmpty()) {
            Log.e(TAG, "keyword is empty");
            return;
        }

        processJumpPage(INaviConstant.OpenIntentPage.SEARCH_PAGE, keyword, null);
    }

    @Override
    public int requestReverseGeoSearch(String clientPkg, BaseGeoPoint baseGeoPoint) {
        if (null == baseGeoPoint) {
            Log.e(TAG, "point is empty, can't reverseSearch");
            return -1;
        }
        Log.i(TAG, clientPkg + " reverseGeoSearch: " + baseGeoPoint);

        GeoPoint geoPoint = new GeoPoint(baseGeoPoint.getLon(), baseGeoPoint.getLat());
        mCommonSearchId = SearchPackage.getInstance().geoSearch(geoPoint);
        return mCommonSearchId;
    }

    @Override
    public void searchAndNavi(String clientPkg, String address) {
        Log.i(TAG, clientPkg + ", searchAndNavi:" + address);
        if (null == address || address.isEmpty()) {
            return;
        }

        SearchPackage searchPackage = SearchPackage.getInstance();
        if (null != searchPackage) {
            mRouteRequestAfterSearch = true;
            searchPackage.silentKeywordSearch(1, address);
        }
    }

    @Override
    public void cancelAllSearchRequest(String clientPkg) {
        Log.i(TAG, clientPkg + " cancelAllSearch");
        SearchPackage searchPackage = SearchPackage.getInstance();
        if (null != searchPackage) {
            searchPackage.abortSearch();
        }
    }

    @Override
    public void routePlan(String clientPkg, BaseSearchPoi baseSearchPoi) {
        if (null == baseSearchPoi || null == baseSearchPoi.getPoint()) {
            Log.e(TAG, "destination is empty, can't not planRoute");
            return;
        }

        BaseGeoPoint geoPoint = baseSearchPoi.getPoint();
        Log.i(TAG, clientPkg + ", routePlan, destination: " + geoPoint);
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(baseSearchPoi.getPid());
        poiInfoEntity.setName(baseSearchPoi.getName());
        poiInfoEntity.setAddress(baseSearchPoi.getAddress());
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        GeoPoint endPoint = new GeoPoint();
        endPoint.setLat(geoPoint.getLat());
        endPoint.setLon(geoPoint.getLon());
        poiInfoEntity.setPoint(endPoint);
        processJumpPage(INaviConstant.OpenIntentPage.ROUTE_PAGE, "", poiInfoEntity);
    }

    @Override
    public void startNavi(String clientPkg) {
        Log.i(TAG, clientPkg + " startNavigation");
        String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
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
    private void processJumpPage(int pageIntent, String keyword, PoiInfoEntity poiInfo) {
        if (null != AppContext.mContext) {
            try {
                BaseActivity baseActivity = StackManager.getInstance()
                        .getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name());
                Intent targetIntent;
                if (null == baseActivity) {
                    String appPkgName = AppContext.mContext.getPackageName();
                    PackageManager packageManager = AppContext.mContext.getPackageManager();
                    targetIntent = packageManager.getLaunchIntentForPackage(appPkgName);
                } else {
                    targetIntent= new Intent(AppContext.mContext, MapActivity.class);
                }
                if (null != targetIntent) {
                    targetIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                    targetIntent.putExtra(INaviConstant.PAGE_EXTRA, pageIntent);
                    switch (pageIntent) {
                        case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                            mSearchKeyword = keyword;
                            targetIntent.putExtra(INaviConstant.SEARCH_KEYWORD_EXTRA, keyword);
                            break;
                        case INaviConstant.OpenIntentPage.ROUTE_PAGE:
                            targetIntent.putExtra(INaviConstant.ROUTE_END_POI, poiInfo);
                            break;
                    }
                    AppContext.mContext.startActivity(targetIntent);
                }
            } catch (Exception exception) {
                Log.e(TAG, "jum error: " + exception.getMessage());
            }
        }
    }

    @Override
    public boolean isNaviStatus(String clientPkg) {
        boolean inNavigation = false;
        NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            String curStatus = naviStatusPackage.getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
                inNavigation = true;
            }
        }

        return inNavigation;
    }

    @Override
    public int getGuidePanelStatus(String clientPkg) {
        if (isNaviStatus("innerDefault")) {
            return INaviConstant.GuidePanelStatus.COMMON_NAVIGATION;
        }
        return INaviConstant.GuidePanelStatus.NOT_IN_NAVIGATION;
    }

    @Override
    public String getTBTInfo(String pkgName) {
        Log.i(TAG, pkgName + " getTBTInfo");
        String turnInfoStr = "";
        if (isNaviStatus(pkgName) && null != mBaseTurnInfo) {
            turnInfoStr = GsonUtils.toJson(mBaseTurnInfo);
        }

        return turnInfoStr;
    }

    @Override
    public String getNaviType(String pkgName) {
        Log.i(TAG, pkgName + " getNaviType");
        String naviType = NaviStatus.NaviStatusType.NO_STATUS;
        NaviStatusPackage naviStatusPackage = NaviStatusPackage.getInstance();
        if (null != naviStatusPackage) {
            String curStatus = naviStatusPackage.getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NAVING.equals(curStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(curStatus)) {
                naviType = curStatus;
            }
        }
        return naviType;
    }

}
