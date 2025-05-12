package com.fy.navi.hmi.cluster;

import android.view.MotionEvent;

import com.android.utils.log.Logger;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class ClusterModel extends BaseModel<BaseClusterViewModel> implements IMapPackageCallback,
IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, SettingPackage.SettingChangeCallback {
    private static final String TAG = "ClusterModel";
    private MapPackage mapPackage;
    private LayerPackage mLayerPackage;
    private CommonManager mCommonManager;
    private RoutePackage mRoutePackage;
    private NavistatusAdapter mNavistatusAdapter;
    private NaviPackage mNaviPackage;
    private PositionPackage mPositionPackage;
    private CruisePackage mCruisePackage;
    private SettingPackage mSettingPackage;
    private boolean mMapIsAttachedWindow = false;
    public ClusterModel() {

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
            mCruisePackage.unregisterObserver(getMapId().name());
        }
    }

    /***
     * 把高德底图绑定到自己的视图上
     */
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
            mSettingPackage = SettingPackage.getInstance();
            mLayerPackage.setPassGray(getMapId(), true);
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
        Logger.d(TAG, "onMapLoadSuccess:" + mapTypeId.name(), "mMapIsAttachedWindow:" + mMapIsAttachedWindow);
        if (mMapIsAttachedWindow) {
            return;
        }
        if (mapTypeId == MapType.CLUSTER_MAP) {
            // ·暂定500米，后续根据实车效果调整。
            mapPackage.setZoomLevel(mapTypeId, 13);
            final int width = (int) mViewModel.getMapView().getMapViewWidth();
            final int height = (int) mViewModel.getMapView().getMapViewHeight();
            Logger.d(TAG, "width:" + width, "height:" + height);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                    mPositionPackage.getLastCarLocation().getLatitude()));
            mapPackage.goToCarPosition(mapTypeId);
            // 根据主屏的车标模式设置车标模式
            mLayerPackage.setCarMode(mapTypeId, mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            mSettingPackage.setSettingChangeCallback(mapTypeId.name(), this);
            mMapIsAttachedWindow = true;
            //mapPackage.switchMapModeCluster(MapType.CLUSTER_MAP, MapMode.UP_3D);
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
            mLayerPackage.setCarMode(MapType.CLUSTER_MAP, mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        }
    }

    @Override
    public void onMapTouchEvent(final MapType mapTypeId, final MotionEvent touchEvent) {

    }

    @Override
    public void onMapClickPoi(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi");
    }

    @Override
    public void onReversePoiClick(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick");
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
        mViewModel.onNaviStatusChanged(naviStatus);
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
        mRoutePackage.showRouteLine(getMapId());
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
        for (MapType mapType: MapType.values()) {
            mRoutePackage.clearRouteLine(mapType);
        }
    }

    /***
     * 结束导航
     */
    public void stopNavi() {
        mNaviPackage.stopNavigation();
    }

    @Override
    public void onUpdateCruiseInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(isShowLane, laneInfoEntity);

    }

    @Override
    public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onShowCruiseCameraExt(cruiseInfoEntity);

    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        IGuidanceObserver.super.onUpdateTMCLightBar(naviTmcInfo);
    }
    @Override
    public void onSettingChanged(String key, String value) {
        SettingPackage.SettingChangeCallback.super.onSettingChanged(key, value);
        Logger.d(TAG, "onSettingChanged:" + key + ":" + value);
        boolean mapViewTextSize = mSettingPackage.getMapViewTextSize();
        if (mapViewTextSize){
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
        }else {
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
        }
        mLayerPackage.setCarMode(MapType.CLUSTER_MAP, mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
    }
}
