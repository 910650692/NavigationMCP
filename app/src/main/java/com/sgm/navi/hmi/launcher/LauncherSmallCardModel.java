package com.sgm.navi.hmi.launcher;

import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.NaviService;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.navistatus.INaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapTypeManager;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseModel;

public class LauncherSmallCardModel extends BaseModel<BaseLauncherSmallCardViewModel> implements StartService.ISdkInitCallback, IMapPackageCallback, IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver {
    private static final String TAG = "LauncherSmallCardModel";
    private MapPackage mapPackage;
    private LayerPackage mLayerPackage;
    private CommonManager mCommonManager;
    private RoutePackage mRoutePackage;
    private NavistatusAdapter mNavistatusAdapter;
    private NaviPackage mNaviPackage;
    private PositionPackage mPositionPackage;
    private CruisePackage mCruisePackage;
    private boolean mMapIsAttachedWindow = false;

    public LauncherSmallCardModel() {
        StartService.getInstance().registerSdkCallback(TAG, this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "start navi Service");
        Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mapPackage != null) {
            mapPackage.unRegisterCallback(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
            mapPackage.unBindMapView(mViewModel.getMapView());
            mRoutePackage.unRegisterRouteObserver(getMapId().name());
            mNavistatusAdapter.unRegisterCallback(this);
            mCruisePackage.unregisterObserver(getMapId().name());
        }
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.d(TAG, "引擎初始化成功");
        ThreadManager.getInstance().postUi(this::loadMapView);
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "引擎初始化失败");
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.d(TAG, "底图渲染成功");
        Logger.d(TAG, "onMapLoadSuccess:" + mapTypeId.name(), "mMapIsAttachedWindow:" + mMapIsAttachedWindow);
        if (mMapIsAttachedWindow) {
            return;
        }
        if (mapTypeId == getMapId()) {
            // ·暂定500米，后续根据实车效果调整。
            mapPackage.setZoomLevel(mapTypeId, 13);
            final int width = (int) mViewModel.getMapView().getMapViewWidth();
            final int height = (int) mViewModel.getMapView().getMapViewHeight();
            Logger.d(TAG, "width:" + width, "height:" + height);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                    mPositionPackage.getLastCarLocation().getLatitude()));
            mapPackage.goToCarPosition(mapTypeId);
            // ·位置：中心点位于底部往上三分之一处，见上图示意。1.1-18
            mapPackage.setMapCenterInScreen(mapTypeId, width / 2, height);
            // 根据主屏的车标模式设置小卡车标模式
            mLayerPackage.setDefaultCarMode(mapTypeId);
            mMapIsAttachedWindow = true;
        }
    }

    @Override
    public void onMapClickPoi(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi");
        if (mapTypeId == getMapId()) {
            mViewModel.startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onReversePoiClick(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick");
        if (mapTypeId == getMapId()) {
            mViewModel.startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        mViewModel.onNaviStatusChanged(naviStatus);
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
//        mRoutePackage.showRouteLine(getMapId());
    }

    public String getCurrentNaviStatus() {
        if (mNavistatusAdapter != null) {
            return mNavistatusAdapter.getCurrentNaviStatus();
        } else {
            return NaviStatus.NaviStatusType.NO_STATUS;
        }
    }

    @Override
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        mViewModel.onNaviInfo(naviInfoBean);
    }

    @Override
    public void onNaviArrive(final long traceId, final int naviType) {
        Logger.i(TAG, "onNaviArrive", "traceId:" + traceId, "naviType:" + naviType);
        IGuidanceObserver.super.onNaviArrive(traceId, naviType);
        mViewModel.naviArriveOrStop();
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        Logger.i(TAG, "onNaviStop");
        mViewModel.naviArriveOrStop();
        clearRouteLine();
    }

    private void clearRouteLine() {
        mRoutePackage.clearRouteLine(getMapId());
    }

    public void stopNavi() {
        mNaviPackage.stopNavigation(true);
    }

    @Override
    public void onUpdateCruiseInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(isShowLane, laneInfoEntity);
        // 巡航-车道信息
        mViewModel.updateCruiseLanInfo(isShowLane, laneInfoEntity);
    }

    @Override
    public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onShowCruiseCameraExt(cruiseInfoEntity);
        // 巡航下的电子眼信息
        mViewModel.updateCruiseCameraInfo(cruiseInfoEntity);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        IGuidanceObserver.super.onUpdateTMCLightBar(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo);
    }

    public void loadMapView() {
        Logger.d(TAG, "loadMapView", "mMapIsAttachedWindow:" + mMapIsAttachedWindow);
        if (mMapIsAttachedWindow) {
            Logger.i(TAG, "mapSurfaceView 已绑定,无需重复绑定！");
            return;
        }
        if (mapPackage == null) {
            Logger.d(TAG, "create map_package", "id:" + (getMapId()));
            mapPackage = MapPackage.getInstance();
            mLayerPackage = LayerPackage.getInstance();
            mRoutePackage = RoutePackage.getInstance();
            mNavistatusAdapter = NavistatusAdapter.getInstance();
            mRoutePackage.registerRouteObserver(getMapId().name(), this);
            mapPackage.registerCallback(getMapId(), this);
            mNavistatusAdapter.registerCallback(this);
            mNaviPackage = NaviPackage.getInstance();
            mNaviPackage.registerObserver(getMapId().name(), this);
            mCommonManager = CommonManager.getInstance();
            mCommonManager.init();
            mPositionPackage = PositionPackage.getInstance();
            mCruisePackage = CruisePackage.getInstance();
            mCruisePackage.registerObserver(getMapId().name(), this);
        }
        mapPackage.bindMapView(mViewModel.getMapView());
    }

    private MapType getMapId() {
        return mViewModel.getMapView().provideMapTypeId();
    }
}