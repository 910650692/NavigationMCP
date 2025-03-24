package com.fy.navi.hmi.launcher;

import android.view.MotionEvent;
import android.view.View;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.map.MapModelHelp;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapTypeId;
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

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class LauncherSmallCardModel extends BaseModel<BaseLauncherSmallCardViewModel> implements IMapPackageCallback,
        IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver {
    private static final String TAG = "LauncherSmallCardModel";
    private MapPackage mapPackage;
    private LayerPackage layerPackage;
    private CommonManager commonManager;
    private RoutePackage routePackage;
    private NavistatusAdapter navistatusAdapter;
    private NaviPackage naviPackage;
    private NaviEtaInfo mNaviEtaInfo;
    private MapModelHelp mapModelHelp;
    public LauncherSmallCardModel() {

    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
        if (mapPackage != null) {
            mapPackage.unRegisterCallback(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
            mapPackage.unBindMapView(mViewModel.getMapView());
            routePackage.unRegisterRouteObserver(getMapId().name());
            navistatusAdapter.unRegisterCallback(this);
        }
    }

    public void loadMapView() {
        Logger.d(TAG, "loadMapView");
        if (mapPackage == null) {
            Logger.d(TAG, "create map_package", "id:" + (getMapId()));
            mapPackage = MapPackage.getInstance();
            layerPackage = LayerPackage.getInstance();
            routePackage = RoutePackage.getInstance();
            navistatusAdapter = NavistatusAdapter.getInstance();
            routePackage.registerRouteObserver(getMapId().name(), this);
            mapPackage.registerCallback(getMapId(), this);
            navistatusAdapter.registerCallback(this);
            naviPackage = NaviPackage.getInstance();
            naviPackage.registerObserver(getMapId().name(), this);
            commonManager = CommonManager.getInstance();
            commonManager.init();
        }
        mapPackage.initMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapCenterChanged(MapTypeId mapTypeId, double lon, double lat) {
        Logger.i(TAG, "onMapCenterChanged:" + lon + "_" + lat);
    }

    @Override
    public void onMapLevelChanged(MapTypeId mapTypeId, float mapLevel) {
        Logger.i(TAG, "onMapLevelChanged", "mapLevel:"+ mapLevel);
    }

    @Override
    public void onMapClickBlank(MapTypeId mapTypeId, float px, float py) {

    }

    @Override
    public void onMapClickLabel(MapTypeId mapTypeId, ArrayList<MapLabelItemBean> pLabels) {

    }

    @Override
    public void onMapMove(MapTypeId mapTypeId, long px, long py, boolean moveEnd) {

    }

    @Override
    public void onMapScaleChanged(MapTypeId mapTypeId, int currentScale) {

    }

    @Override
    public void onMapInitSuccess(MapTypeId mapTypeId, boolean success) {
        Logger.d(TAG, "onMapInitSuccess:" + mapTypeId.name());
    }

    @Override
    public void onMapLoadSuccess(MapTypeId mapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess:" + mapTypeId.name());
        if (mapTypeId == MapTypeId.LAUNCHER_WIDGET_MAP) {
            mapModelHelp = new MapModelHelp(mapTypeId);
            layerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.goToCarPosition(mapTypeId);
            mapPackage.setZoomLevel(mapTypeId, 13);

            // TODO 车标位置设置不正确，需要高德SDK支持分屏后再修改或者测试
            int width = (int) mViewModel.getMapView().getMapViewWidth();
            int height = (int) mViewModel.getMapView().getMapViewHeight();
            Logger.d(TAG, "width:" + width, "height:" + height);
            mapPackage.setMapCenterInScreen(mapTypeId, width / 2, height / 2);
//            mapModelHelp.restoreSetting();
        }
    }

    @Override
    public void onMapTouchEvent(MapTypeId mapTypeId, MotionEvent touchEvent) {

    }

    @Override
    public void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi");
        if (mapTypeId == getMapId()) {
            LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onReversePoiClick(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick");
        if (mapTypeId == getMapId()) {
            LauncherManager.getInstance().startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onNaviStatusChange(String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
        mViewModel.onNaviStatusChanged(naviStatus);
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
    }

    /**
     * 设置底图中心点相对于屏幕偏移量.
     *
     * @param frameLayoutWidth fragment的宽度
     */
    public void setMapCenterInScreen(int frameLayoutWidth) {
        Logger.i(TAG, "setMapCenterInScreen:" + frameLayoutWidth);
//        mapPackage.setMapCenterInScreen(getMapId(), 200, 200);
    }

    /**
     * 恢复底图中心点在屏幕上的位置.
     */
    public void resetMapCenterInScreen(IBaseScreenMapView view) {
        int left = (int) (view.getMapViewWidth() / 2);
        int top = (int) (view.getMapViewHeight() / 3);
        mapPackage.setMapCenterInScreen(getMapId(), left, top);
        mapPackage.goToCarPosition(view.provideMapTypeId());
        Logger.i(TAG, "resetMapCenterInScreen", "left:" + left, "top:" + top);
    }

    private MapTypeId getMapId() {
        return mViewModel.getMapView().provideMapTypeId();
    }

    public String getCurrentNaviStatus() {
        if (navistatusAdapter != null)
            return navistatusAdapter.getCurrentNaviStatus();
        else return NaviStatus.NaviStatusType.NO_STATUS;
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        mNaviEtaInfo = naviInfoBean;
        mViewModel.onNaviInfo(naviInfoBean);
    }

    @Override
    public void onNaviArrive(long traceId, int naviType) {
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

    public void stopNavi() {
        routePackage.clearRouteLine(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId));
        naviPackage.stopNavigation();
    }

    @Override
    public void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(cruiseInfoEntity);
        // 巡航-电子眼信息
        mViewModel.updateCruiseCameraInfo(cruiseInfoEntity);
    }

    @Override
    public void onUpdateCruiseInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(isShowLane, laneInfoEntity);
        // 巡航-车道信息
        mViewModel.updateCruiseLanInfo(laneInfoEntity);
    }

    @Override
    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        IGuidanceObserver.super.onUpdateTMCLightBar(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo);
    }
}