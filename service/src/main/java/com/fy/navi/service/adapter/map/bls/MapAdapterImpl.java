package com.fy.navi.service.adapter.map.bls;

import android.os.Looper;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.model.InitMapParam;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.IMapApi;
import com.fy.navi.service.adapter.map.IsEnterPreviewCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapAdapterImpl implements IMapApi {
    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private MapService mMapService;

    private MapSurfaceViewManager mapSurfaceViewManager;

    public MapAdapterImpl() {
        mapSurfaceViewManager = new MapSurfaceViewManager();
        mMapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        ConvertUtils.isNullRequire(mMapService, "Init MapService load fail");
        ServiceMgr.getServiceMgrInstance().setUiLooper(0, Looper.getMainLooper());
        int resultCode = mMapService.initMap(initMapParam());
        Logger.i(TAG, "MapService init result code -> " + resultCode);
    }

    @Override
    public boolean init(MapTypeId mapTypeId) {
        if (mMapService.isInit() == ServiceInitStatus.ServiceInitDone) {
            return mapSurfaceViewManager.init(mMapService, mapTypeId);
        } else {
            return false;
        }
    }

    @Override
    public void unBindMapView(IBaseScreenMapView mapView) {
        mapView.unBindMapView(mapSurfaceViewManager.getMapSurfaceView(mapView.provideMapTypeId()));
    }

    @Override
    public void initMapView(IBaseScreenMapView mapView) {
        MapSurfaceViewSizeParams mapSurfaceViewSizeParams = new MapSurfaceViewSizeParams(mapView.getMapViewX(), mapView.getMapViewY(),
                mapView.getMapViewWidth(), mapView.getMapViewHeight(),
                mapView.getScreenWidth(), mapView.getScreenHeight(),
                mapView.getScreenDensityDpi());
        MapSurfaceViewImp mapSurfaceViewImp = mapSurfaceViewManager.getMapSurfaceView(mapView.provideMapTypeId());
        mapSurfaceViewImp.updateMapSurfaceViewSizeParams(mapSurfaceViewSizeParams);
        mapView.bindMapView(mapSurfaceViewImp);
    }

    @Override
    public void unInitMapView(IBaseScreenMapView mapSurfaceView) {
        mapSurfaceViewManager.unInitMapView(mapSurfaceView.provideMapTypeId());
    }

    @Override
    public void registerCallback(MapTypeId mapTypeId, IMapAdapterCallback callback) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).registerCallback(callback);
    }

    @Override
    public void unRegisterCallback(MapTypeId mapTypeId, IMapAdapterCallback callback) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).unRegisterCallback(callback);
    }

    @Override
    public void unitMapService() {
        mapSurfaceViewManager.clearAllMapSurfaceView();
        mMapService.unitMap();
        mMapService = null;
    }


    @Override
    public float getCurrentZoomLevel(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getCurrentZoomLevel();
    }

    @Override
    public int getCurrentScale(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getCurrentScale();
    }

    @Override
    public void setZoomLevel(MapTypeId mapTypeId, float level) {
        if (AutoMapConstant.MAP_ZOOM_LEVEL_MIN > level || level > AutoMapConstant.MAP_ZOOM_LEVEL_MAX)
            return;
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setZoomLevel(level);
    }

    @Override
    public void setMapCenterInScreen(MapTypeId mapTypeId, int x, int y) {
        Logger.d(TAG, "map left: " + x, "map top: " + y);
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setMapCenterInScreen(x, y);
    }

    @Override
    public void setMapCenter(MapTypeId mapTypeId, GeoPoint geoPoint) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setMapCenter(geoPoint);
    }

    @Override
    public boolean setTrafficStates(MapTypeId mapTypeId, boolean isOpen) {
       return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setTrafficStates(isOpen);
    }

    @Override
    public void setCustomLabelTypeVisible(MapTypeId mapTypeId, ArrayList<Integer> typeList, boolean visible) {
         mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setCustomLabelTypeVisable(typeList, visible);
    }

    @Override
    public void setMapViewTextSize(MapTypeId mapTypeId, float f) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setMapViewTextSize(f);
    }

    @Override
    public MapMode getCurrentMapMode(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getMapMode();
    }

    @Override
    public void setMapMode(MapTypeId mapTypeId, MapMode mapMode) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setMapMode(mapMode);
    }

    @Override
    public void setMapStateStyle(MapTypeId mapTypeId, MapStateStyle mapStateStyle) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setMapStyle(mapStateStyle);
    }

    @Override
    public void goToCarPosition(MapTypeId mapTypeId, boolean bAnimation, boolean changeLevel) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).goToCarPosition(bAnimation, changeLevel);
    }

    @Override
    public GeoPoint mapToLonLat(MapTypeId mapTypeId, double mapX, double mapY) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).mapToLonLat(mapX, mapY);
    }

    @Override
    public MapSurfaceViewSizeParams getMapSurfaceParam(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getMapSurfaceViewSizeParams();
    }

    @Override
    public void showPreview(MapTypeId mapTypeId, PreviewParams previewParams) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).showPreview(previewParams);
    }

    @Override
    public void exitPreview(MapTypeId mapTypeId) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).exitPreview();
    }

    @Override
    public void updateUiStyle(MapTypeId mapTypeId, int uiMode) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).updateUiStyle(uiMode);
    }

    @Override
    public String getMapBound(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getMapBound();
    }

    private InitMapParam initMapParam() {
        InitMapParam initMapParam = new InitMapParam();
        /*** 地图数据路径绝对地址 **/
        initMapParam.dataPath = GBLCacheFilePath.MAP_DATA_DIR;
        /*** 基本数据路径地址URL **/
        initMapParam.basePath = GBLCacheFilePath.MAP_BASE_PATH;
        /*** 配置引擎样式文件MapAssert的绝对地址 **/
        initMapParam.assetPath = GBLCacheFilePath.MAP_ASSET_DIR;
        return initMapParam;
    }

    @Override
    public void set3DBuilding(MapTypeId mapTypeId, boolean isVisible) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).update3DBuildingSwitch(isVisible);
    }

    @Override
    public void setPitchAngle(MapTypeId mapTypeId, float pitch, boolean isAnimation, boolean isSync) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).setPitchAngle(pitch, isAnimation, isSync);
    }

    @Override
    public void addIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).addIsEnterPreviewCallback(callback);
    }

    @Override
    public boolean getIsEnterPreview(MapTypeId mapTypeId) {
        return mapSurfaceViewManager.getMapSurfaceView(mapTypeId).getIsPreview();
    }

    @Override
    public void removeIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mapSurfaceViewManager.getMapSurfaceView(mapTypeId).removeIsEnterPreviewCallback(callback);
    }
}
