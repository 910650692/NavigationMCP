package com.fy.navi.service.adapter.map;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapAdapter {
    private static final String GAODE_API_PKG = MapAdapter.class.getPackage().getName();
    private static final String GAODE_API_CLS = "MapAdapterImpl";
    private IMapApi mIMapApi;

    public static MapAdapter getInstance() {
        return Helper.ea;
    }

    public boolean init(MapTypeId mapTypeId) {
        return mIMapApi.init(mapTypeId);
    }

    public void unBindMapView(IBaseScreenMapView mapView) {
        mIMapApi.unBindMapView(mapView);
    }

    private static final class Helper {
        private static final MapAdapter ea = new MapAdapter();
    }

    private MapAdapter() {
        mIMapApi = (IMapApi) AdapterConfig.getObject(GAODE_API_PKG, GAODE_API_CLS);
    }

    public void registerCallback(MapTypeId mapTypeId, IMapAdapterCallback callback) {
        mIMapApi.registerCallback(mapTypeId, callback);
    }

    public void initMapView(IBaseScreenMapView mapSurfaceView) {
        mIMapApi.initMapView(mapSurfaceView);
    }

    public void unInitMapView(IBaseScreenMapView mapSurfaceView) {
        mIMapApi.unInitMapView(mapSurfaceView);
    }

    public void unInitMapService() {
        mIMapApi.unitMapService();
    }

    public void reduceLevel(MapTypeId mapTypeId) {
        mIMapApi.setZoomLevel(mapTypeId, mIMapApi.getCurrentZoomLevel(mapTypeId) - AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG);
    }

    public void setZoomLevel(MapTypeId mapTypeId, float level) {
        mIMapApi.setZoomLevel(mapTypeId, level);
    }

    public float getZoomLevel(MapTypeId mapTypeId) {
        return mIMapApi.getCurrentZoomLevel(mapTypeId);
    }

    public void amplifyLevel(MapTypeId mapTypeId) {
        mIMapApi.setZoomLevel(mapTypeId, mIMapApi.getCurrentZoomLevel(mapTypeId) + AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG);
    }

    public void setMapCenterInScreen(MapTypeId mapTypeId, int x, int y) {
        mIMapApi.setMapCenterInScreen(mapTypeId, x, y);
    }

    public void setMapCenter(MapTypeId mapTypeId, GeoPoint geoPoint) {
        mIMapApi.setMapCenter(mapTypeId, geoPoint);
    }

    public boolean setTrafficStates(MapTypeId mapTypeId, boolean isOpen) {
       return mIMapApi.setTrafficStates(mapTypeId, isOpen);
    }

    public void setCustomLabelTypeVisible(MapTypeId mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        mIMapApi.setCustomLabelTypeVisible(mapTypeId, typeList, visible);
    }

    public void setMapViewTextSize(MapTypeId mapTypeId, float f) {
        mIMapApi.setMapViewTextSize(mapTypeId, f);
    }

    public void switchMapMode(MapTypeId mapTypeId) {
        MapMode mapMode = mIMapApi.getCurrentMapMode(mapTypeId);
        mapMode = switch (mapMode) {
            case UP_2D -> MapMode.UP_3D;
            case UP_3D -> MapMode.NORTH_2D;
            case NORTH_2D -> MapMode.UP_2D;
        };
        mIMapApi.setMapMode(mapTypeId, mapMode);
    }

    public MapMode getCurrentMapMode(MapTypeId mapTypeId) {
        return mIMapApi.getCurrentMapMode(mapTypeId);
    }

    public void switchMapMode(MapTypeId mapTypeId, MapMode mapMode) {
        mIMapApi.setMapMode(mapTypeId, mapMode);
    }

    public void setMapStateStyle(MapTypeId mapTypeId, MapStateStyle mapStateStyle) {
        mIMapApi.setMapStateStyle(mapTypeId, mapStateStyle);
    }

    public void goToCarPosition(MapTypeId mapTypeId, boolean bAnimation, boolean changeLevel) {
        mIMapApi.goToCarPosition(mapTypeId, bAnimation, changeLevel);
    }

    public GeoPoint mapToLonLat(MapTypeId mapTypeId, double mapX, double mapY) {
        return mIMapApi.mapToLonLat(mapTypeId, mapX, mapY);
    }

    public MapSurfaceViewSizeParams getMapSurfaceParam(MapTypeId mapTypeId) {
        return mIMapApi.getMapSurfaceParam(mapTypeId);
    }

    public void showPreview(MapTypeId mapTypeId, PreviewParams previewParams) {
        mIMapApi.showPreview(mapTypeId, previewParams);
    }

    public void exitPreview(MapTypeId mapTypeId) {
        mIMapApi.exitPreview(mapTypeId);
    }

    public void updateUiStyle(MapTypeId mapTypeId, int uiMode) {
        mIMapApi.updateUiStyle(mapTypeId, uiMode);
    }

    // 搜索需要的对角线参数
    public String getMapBound(MapTypeId mapTypeId) {
        return mIMapApi.getMapBound(mapTypeId);
    }

    // 设置3D建筑是否显示
    public void set3DBuilding(MapTypeId mapTypeId, boolean isOpen) {
        mIMapApi.set3DBuilding(mapTypeId, isOpen);
    }

    public float getCurrentZoomLevel(MapTypeId mapTypeId) {
        return mIMapApi.getCurrentZoomLevel(mapTypeId);
    }

    public int getCurrentZoomScale(MapTypeId mapTypeId) {
        return mIMapApi.getCurrentScale(mapTypeId);
    }

    public void setPitchAngle(MapTypeId mapTypeId, float pitch, boolean isAnimation, boolean isSync) {
        mIMapApi.setPitchAngle(mapTypeId, pitch, isAnimation, isSync);
    }

    public void addIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mIMapApi.addIsEnterPreviewCallback(mapTypeId, callback);
    }

    public void removeIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mIMapApi.removeIsEnterPreviewCallback(mapTypeId, callback);
    }

    public boolean getIsEnterPreview(MapTypeId mapTypeId) {
        return mIMapApi.getIsEnterPreview(mapTypeId);
    }
}
