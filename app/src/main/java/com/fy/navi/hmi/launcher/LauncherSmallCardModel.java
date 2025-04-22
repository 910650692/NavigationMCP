package com.fy.navi.hmi.launcher;

import android.view.MotionEvent;

import com.android.utils.log.Logger;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

public class LauncherSmallCardModel extends BaseModel<BaseLauncherSmallCardViewModel> implements IMapPackageCallback,
        IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver {
    private static final String TAG = "LauncherSmallCardModel";
    private MapPackage mapPackage;
    private LayerPackage mLayerPackage;
    private CommonManager mCommonManager;
    private RoutePackage mRoutePackage;
    private NavistatusAdapter mNavistatusAdapter;
    private NaviPackage mNaviPackage;
    private PositionPackage mPositionPackage;
    public LauncherSmallCardModel() {

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
        if (mapPackage != null) {
            mapPackage.unRegisterCallback(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
            mapPackage.unBindMapView(mViewModel.getMapView());
            mRoutePackage.unRegisterRouteObserver(getMapId().name());
            mNavistatusAdapter.unRegisterCallback(this);
        }
    }

    /***
     * 把高德底图绑定到自己的视图上
     */
    public void loadMapView() {
        Logger.d(TAG, "loadMapView");
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
        }
        mapPackage.initMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapCenterChanged(final MapType mapTypeId, final double lon, final double lat) {
        Logger.i(TAG, "onMapCenterChanged:" + lon + "_" + lat);
    }

    @Override
    public void onMapLevelChanged(final MapType mapTypeId, final float mapLevel) {
        Logger.i(TAG, "onMapLevelChanged", "mapLevel:" + mapLevel);
    }

    @Override
    public void onMapClickBlank(final MapType mapTypeId, final float px, final float py) {

    }

    @Override
    public void onMapClickLabel(final MapType mapTypeId, final ArrayList<MapLabelItemBean> itemBeans) {

    }

    @Override
    public void onMapMove(final MapType mapTypeId, final long px, final long py, final boolean moveEnd) {

    }

    @Override
    public void onMapScaleChanged(final MapType mapTypeId, final int currentScale) {

    }

    @Override
    public void onMapInitSuccess(final MapType mapTypeId, final boolean success) {
        Logger.d(TAG, "onMapInitSuccess:" + mapTypeId.name());
    }

    @Override
    public void onMapLoadSuccess(final MapType mapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess:" + mapTypeId.name());
        if (mapTypeId == MapType.LAUNCHER_WIDGET_MAP) {
            mLayerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.goToCarPosition(mapTypeId);
            mapPackage.setZoomLevel(mapTypeId, 13);

            // TODO 车标位置设置不正确，需要高德SDK支持分屏后再修改或者测试
            final int width = (int) mViewModel.getMapView().getMapViewWidth();
            final int height = (int) mViewModel.getMapView().getMapViewHeight();
            Logger.d(TAG, "width:" + width, "height:" + height);
            mapPackage.setMapCenterInScreen(mapTypeId, width / 2, height / 2);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                    mPositionPackage.getLastCarLocation().getLatitude()));
//            mapModelHelp.restoreSetting();
        }
    }

    @Override
    public void onMapTouchEvent(final MapType mapTypeId, final MotionEvent touchEvent) {

    }

    @Override
    public void onMapClickPoi(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi");
        if (mapTypeId == getMapId()) {
            LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onReversePoiClick(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick");
        if (mapTypeId == getMapId()) {
            LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
        mViewModel.onNaviStatusChanged(naviStatus);
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
    }

    /**
     * 设置底图中心点相对于屏幕偏移量.
     *
     * @param frameLayoutWidth fragment的宽度
     */
    public void setMapCenterInScreen(final int frameLayoutWidth) {
        Logger.i(TAG, "setMapCenterInScreen:" + frameLayoutWidth);
    }

    /**
     * 恢复底图中心点在屏幕上的位置.
     *
     * @param view
     */
    public void resetMapCenterInScreen(final IBaseScreenMapView view) {
        final int left = (int) (view.getMapViewWidth() / 2);
        final int top = (int) (view.getMapViewHeight() * 3/ 5);
        mapPackage.setMapCenterInScreen(getMapId(), left, top);
        mapPackage.goToCarPosition(view.provideMapTypeId());
        Logger.i(TAG, "resetMapCenterInScreen", "left:" + left, "top:" + top);
    }

    private MapType getMapId() {
        return mViewModel.getMapView().provideMapTypeId();
    }

    /***
     *
     * @return 获取当前导航态
     */
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
    }

    /***
     * 结束导航
     */
    public void stopNavi() {
        mRoutePackage.clearRouteLine(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        mNaviPackage.stopNavigation();
    }

    @Override
    public void onUpdateCruiseInfo(final CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(cruiseInfoEntity);
        // 巡航-电子眼信息
        mViewModel.updateCruiseCameraInfo(cruiseInfoEntity);
    }

    @Override
    public void onUpdateCruiseInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(isShowLane, laneInfoEntity);
        // 巡航-车道信息
        mViewModel.updateCruiseLanInfo(laneInfoEntity);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        IGuidanceObserver.super.onUpdateTMCLightBar(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo);
    }
}