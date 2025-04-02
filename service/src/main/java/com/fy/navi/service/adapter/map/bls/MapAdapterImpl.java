package com.fy.navi.service.adapter.map.bls;

import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.IMapApi;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.MapType;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapAdapterImpl implements IMapApi {
    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private MapViewPoolManager mapViewPoolManager;

    public MapAdapterImpl() {
        mapViewPoolManager = MapViewPoolManager.getInstance();
    }

    @Override
    public boolean initMapService(MapType mapTypeId) {
        return mapViewPoolManager.init(mapTypeId);
    }

    @Override
    public void unBindMapView(IBaseScreenMapView mapView) {

    }

    @Override
    public void bindMapView(IBaseScreenMapView mapView) {
        MapViewParams mapViewParams = new MapViewParams(mapView.getMapViewX(), mapView.getMapViewY(),
                mapView.getMapViewWidth(), mapView.getMapViewHeight(),
                mapView.getScreenWidth(), mapView.getScreenHeight(),
                mapView.getScreenDensityDpi());
        MapViewImpl mapSurfaceViewImp = mapViewPoolManager.get(mapView.provideMapTypeId(), mapViewParams);
        mapView.bindMapView(mapSurfaceViewImp);
    }

    @Override
    public void unitMapService(MapType mapTypeId) {

    }

    @Override
    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mapViewPoolManager.get(mapTypeId).registerCallback(callback);
    }

    @Override
    public void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mapViewPoolManager.get(mapTypeId).unRegisterCallback(callback);
    }


    @Override
    public float getCurrentZoomLevel(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).getCurrentZoomLevel();
    }

    @Override
    public int getCurrentScale(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).getCurrentScale();
    }

    @Override
    public void setZoomLevel(MapType mapTypeId, float level) {
        if (AutoMapConstant.MAP_ZOOM_LEVEL_MIN > level || level > AutoMapConstant.MAP_ZOOM_LEVEL_MAX)
            return;
        mapViewPoolManager.get(mapTypeId).setZoomLevel(level);
    }

    @Override
    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        Logger.d(TAG, "map left: " + x, "map top: " + y);
        mapViewPoolManager.get(mapTypeId).setMapCenterInScreen(x, y);
    }

    @Override
    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mapViewPoolManager.get(mapTypeId).setMapCenter(geoPoint);
    }

    @Override
    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        return mapViewPoolManager.get(mapTypeId).setTrafficStates(isOpen);
    }

    @Override
    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        mapViewPoolManager.get(mapTypeId).setCustomLabelTypeVisable(typeList, visible);
    }

    @Override
    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mapViewPoolManager.get(mapTypeId).setMapViewTextSize(f);
    }

    @Override
    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).getMapMode();
    }

    @Override
    public boolean setMapMode(MapType mapTypeId, MapMode mapMode) {
       return mapViewPoolManager.get(mapTypeId).setMapMode(mapMode);
    }

    @Override
    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        mapViewPoolManager.get(mapTypeId).setMapStyle(mapStateStyle);
    }

    @Override
    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        mapViewPoolManager.get(mapTypeId).goToCarPosition(bAnimation, changeLevel);
    }

    @Override
    public GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY) {
        return mapViewPoolManager.get(mapTypeId).mapToLonLat(mapX, mapY);
    }

    @Override
    public MapViewParams getMapSurfaceParam(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).getMapViewParams();
    }

    @Override
    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        mapViewPoolManager.get(mapTypeId).showPreview(previewParams);
    }

    @Override
    public void exitPreview(MapType mapTypeId) {
        mapViewPoolManager.get(mapTypeId).exitPreview();
    }

    @Override
    public void updateUiStyle(MapType mapTypeId, int uiMode) {
        mapViewPoolManager.get(mapTypeId).updateUiStyle(uiMode);
    }

    @Override
    public String getMapBound(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).getMapBound();
    }

    @Override
    public void set3DBuilding(MapType mapTypeId, boolean isVisible) {
        mapViewPoolManager.get(mapTypeId).update3DBuildingSwitch(isVisible);
    }

    @Override
    public boolean getIsEnterPreview(MapType mapTypeId) {
        return mapViewPoolManager.get(mapTypeId).isPreview();
    }

}
