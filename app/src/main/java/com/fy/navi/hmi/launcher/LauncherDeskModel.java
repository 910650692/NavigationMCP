package com.fy.navi.hmi.launcher;

import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.StartService;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.ui.base.BaseModel;

import java.util.ArrayList;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class LauncherDeskModel extends BaseModel<BaseLauncherDeskViewModel> implements StartService.ISdkInitCallback, IMapPackageCallback {
    private static final String TAG = "LauncherDeskModel";
    private MapPackage mapPackage;
    private boolean mapLoadStatus = false;

    public LauncherDeskModel() {
        StartService.getInstance().registerSdkCallback(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        if (!ConvertUtils.isNull(mapPackage)) {
            mapPackage.unRegisterCallback(getMapId(), this);
            mapPackage.unBindMapView(mViewModel.getMapView());
        }
    }

    @Override
    public void onSdkInitSuccess() {
        if(mapLoadStatus) return;
        mapPackage.initMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {

    }

    @Override
    public void onMapLevelChanged(MapType mapTypeId, float mapLevel) {

    }

    @Override
    public void onMapClickBlank(MapType mapTypeId, float px, float py) {

    }

    @Override
    public void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels) {

    }

    @Override
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
//        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
    }

    @Override
    public void onMapInitSuccess(MapType mapTypeId, boolean success) {

    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.i(TAG, "onMapLoadSuccess", "mapTypeId:" + mapTypeId.name());
        mapLoadStatus = true;
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {

    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi", "mapTypeId:" + mapTypeId);
        /*if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {
            Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
            bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
            bundle.putInt(INaviConstant.PAGE_EXTRA, INaviConstant.OpenIntentPage.POI_DETAIL_PAGE);
//            LauncherManager.getInstance().startMapActivity(RouteUtils.HOST_OPEN_POI_DETAIL);
        }*/
    }

    @Override
    public void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick", "mapTypeId:" + mapTypeId);
    }

    @Override
    public void onMapModeChange(MapType mapTypeId, MapMode mapMode) {

    }


    @Override
    public void onNaviStatusChange(String naviStatus) {
    }

    public void loadMapView() {
        if (ConvertUtils.isNull(mapPackage)) {
            mapPackage = MapPackage.getInstance();
            mapPackage.registerCallback(getMapId(), this);
        }
        mapPackage.initMapView(mViewModel.getMapView());
    }

    private MapType getMapId() {
        return MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId);
    }
}