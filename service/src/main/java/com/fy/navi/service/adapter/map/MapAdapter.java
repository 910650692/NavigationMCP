package com.fy.navi.service.adapter.map;

import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.setting.SettingAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.mfc.MfcController;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapAdapter {
    private static final String GAODE_API_PKG = MapAdapter.class.getPackage().getName();
    private static final String GAODE_API_CLS = "MapAdapterImpl";
    private IMapApi mIMapApi;

    private SettingAdapter mSettingAdapter;

    public static MapAdapter getInstance() {
        return Holder.INSTANCE;
    }

    private static final class Holder {
        private static final MapAdapter INSTANCE = new MapAdapter();
    }

    private MapAdapter() {
        mIMapApi = (IMapApi) AdapterConfig.getObject(GAODE_API_PKG, GAODE_API_CLS);
        mSettingAdapter = SettingAdapter.getInstance();
    }


    public boolean init(MapType mapTypeId) {
        return mIMapApi.initMapService(mapTypeId);
    }

    public void unBindMapView(IBaseScreenMapView mapView) {
        mIMapApi.unBindMapView(mapView);
    }

    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mIMapApi.registerCallback(mapTypeId, callback);
    }

    public void initMapView(IBaseScreenMapView mapSurfaceView) {
        mIMapApi.bindMapView(mapSurfaceView);
    }

    public void unInitMapService(MapType mapTypeId) {
        mIMapApi.unitMapService(mapTypeId);
    }

    public void reduceLevel(MapType mapTypeId) {
        mIMapApi.setZoomLevel(mapTypeId, mIMapApi.getCurrentZoomLevel(mapTypeId) - AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG);
    }

    public void setZoomLevel(MapType mapTypeId, float level) {
        mIMapApi.setZoomLevel(mapTypeId, level);
    }

    public float getZoomLevel(MapType mapTypeId) {
        return mIMapApi.getCurrentZoomLevel(mapTypeId);
    }

    public void amplifyLevel(MapType mapTypeId) {
        mIMapApi.setZoomLevel(mapTypeId, mIMapApi.getCurrentZoomLevel(mapTypeId) + AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG);
    }

    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mIMapApi.setMapCenterInScreen(mapTypeId, x, y);
    }

    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mIMapApi.setMapCenter(mapTypeId, geoPoint);
    }

    public GeoPoint getMapCenter(MapType mapTypeId) {
       return mIMapApi.getMapCenter(mapTypeId);
    }

    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        return mIMapApi.setTrafficStates(mapTypeId, isOpen);
    }

    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        mIMapApi.setCustomLabelTypeVisible(mapTypeId, typeList, visible);
    }

    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mIMapApi.setMapViewTextSize(mapTypeId, f);
    }

    public boolean switchMapMode(MapType mapTypeId) {
        MapMode mapMode = mIMapApi.getCurrentMapMode(mapTypeId);
        mapMode = switch (mapMode) {
            case UP_2D -> MapMode.UP_3D;
            case UP_3D -> MapMode.NORTH_2D;
            case NORTH_2D -> MapMode.UP_2D;
        };
        switchMapMode(mapMode);
        return mIMapApi.setMapMode(mapTypeId, mapMode);
    }

    private void switchMapMode(MapMode mapMode) {
        switch (mapMode) {
            case UP_2D:
                mSettingAdapter.setConfigKeyMapviewMode(0);
                break;
            case UP_3D:
                mSettingAdapter.setConfigKeyMapviewMode(2);
                break;
            case NORTH_2D:
                mSettingAdapter.setConfigKeyMapviewMode(1);
                break;
        }
    }

    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mIMapApi.getCurrentMapMode(mapTypeId);
    }

    public boolean switchMapMode(MapType mapTypeId, MapMode mapMode) {
        switchMapMode(mapMode);
        return mIMapApi.setMapMode(mapTypeId, mapMode);
    }

    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        mIMapApi.setMapStateStyle(mapTypeId, mapStateStyle);
    }

    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        mIMapApi.goToCarPosition(mapTypeId, bAnimation, changeLevel);
    }

    public void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance) {
        mIMapApi.mfcMoveMap(mapTypeId,mfcController, moveDistance);
    }

    public GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY) {
        return mIMapApi.mapToLonLat(mapTypeId, mapX, mapY);
    }

    public MapViewParams getMapSurfaceParam(MapType mapTypeId) {
        return mIMapApi.getMapSurfaceParam(mapTypeId);
    }

    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        mIMapApi.showPreview(mapTypeId, previewParams);
    }

    public void exitPreview(MapType mapTypeId) {
        mIMapApi.exitPreview(mapTypeId);
    }

    public void updateUiStyle(MapType mapTypeId, int uiMode) {
        mIMapApi.updateUiStyle(mapTypeId, uiMode);
    }

    // 搜索需要的对角线参数
    public String getMapBound(MapType mapTypeId) {
        return mIMapApi.getMapBound(mapTypeId);
    }

    // 设置3D建筑是否显示
    public void set3DBuilding(MapType mapTypeId, boolean isOpen) {
        mIMapApi.set3DBuilding(mapTypeId, isOpen);
    }

    public float getCurrentZoomLevel(MapType mapTypeId) {
        return mIMapApi.getCurrentZoomLevel(mapTypeId);
    }

    public int getCurrentZoomScale(MapType mapTypeId) {
        return mIMapApi.getCurrentScale(mapTypeId);
    }

}
