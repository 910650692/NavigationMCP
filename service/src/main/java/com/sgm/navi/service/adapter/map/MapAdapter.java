package com.sgm.navi.service.adapter.map;

import android.graphics.Rect;

import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.adapter.setting.SettingAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapViewParams;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.PointDataInfo;
import com.android.utils.theme.ThemeType;
import com.sgm.navi.service.define.mfc.MfcController;

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
    }

    public void initMapService() {
        mIMapApi.initMapService();
        mSettingAdapter = SettingAdapter.getInstance();
    }

    public void unInitMapService() {
        mSettingAdapter = null;
    }

    public boolean createMapView(MapType mapTypeId) {
       return mIMapApi.createMapView(mapTypeId);
    }

    public void bindMapView(IBaseScreenMapView mapSurfaceView) {
        mIMapApi.bindMapView(mapSurfaceView);
    }

    public void changeMapViewParams(IBaseScreenMapView mapSurfaceView) {
        mIMapApi.changeMapViewParams(mapSurfaceView);
    }

    public void unBindMapView(IBaseScreenMapView mapView) {
        mIMapApi.unBindMapView(mapView);
    }

    public void destroyMapView(MapType mapTypeId) {
        mIMapApi.destroyMapView(mapTypeId);
    }

    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mIMapApi.registerCallback(mapTypeId, callback);
    }

    public void unregisterCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mIMapApi.unRegisterCallback(mapTypeId, callback);
    }
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return mIMapApi.calcStraightDistance(startPoint, endPoint);
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

    /* 判断当前mapview是否存在 */
    public boolean isMapViewExist(MapType mapTypeId) {
        return mIMapApi.isMapViewExist(mapTypeId);
    }

    public void amplifyLevel(MapType mapTypeId) {
        mIMapApi.setZoomLevel(mapTypeId, mIMapApi.getCurrentZoomLevel(mapTypeId) + AutoMapConstant.MAP_ZOOM_LEVEL_CHANGE_FLAG);
    }

    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mIMapApi.setMapCenterInScreen(mapTypeId, x, y);
    }
    /**
     * 设置Hud地图中线点在屏幕中的位置
     */
    public void setHudMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mIMapApi.setHudMapCenterInScreen(mapTypeId, x, y);
    }

    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mIMapApi.setMapCenter(mapTypeId, geoPoint);
    }

    public GeoPoint getMapCenter(MapType mapTypeId) {
       return mIMapApi.getMapCenter(mapTypeId);
    }

    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        boolean isOpenTraffic =  mIMapApi.setTrafficStates(mapTypeId, isOpen);
        if (isOpenTraffic) {
            mSettingAdapter.setConfigKeyRoadEvent(isOpen);
        }
        return isOpenTraffic;
    }

    public boolean setTrafficStatesWithoutNetwork(MapType mapTypeId, boolean isOpen) {
        boolean isOpenTraffic =  mIMapApi.setTrafficStates(mapTypeId, isOpen);
        return isOpenTraffic;
    }

    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        mIMapApi.setCustomLabelTypeVisible(mapTypeId, typeList, visible);
    }

    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mIMapApi.setMapViewTextSize(mapTypeId, f);
    }

    public void isSplitScreen(MapType mapTypeId, boolean isSplit) {
        mIMapApi.isSplitScreen(mapTypeId, isSplit);
    }

    /**
     * 用于循环切换底图视角
     * @param mapTypeId current MapType
     * @return true: success/false: fail
     */
    public boolean switchMapMode(MapType mapTypeId) {
        MapMode mapMode = mIMapApi.getCurrentMapMode(mapTypeId);
        mapMode = switch (mapMode) {
            case UP_2D -> {
                mSettingAdapter.setConfigKeyMapviewMode(2);
                yield MapMode.UP_3D;
            }
            case UP_3D -> {
                mSettingAdapter.setConfigKeyMapviewMode(1);
                yield MapMode.NORTH_2D;
            }
            case NORTH_2D -> {
                mSettingAdapter.setConfigKeyMapviewMode(0);
                yield MapMode.UP_2D;
            }
        };
        return mIMapApi.setMapMode(mapTypeId, mapMode);
    }
    public void switchHudMapMode(MapType mapTypeId, MapMode mapMode) {
        mIMapApi.setMapHudMode(mapTypeId, mapMode);
    }

    public void setLockMapPinchZoom(MapType mapTypeId, boolean isLock) {
        mIMapApi.setLockMapPinchZoom(mapTypeId, isLock);
    }

    public void setLockMapMove(MapType mapTypeId, boolean isLock) {
        mIMapApi.setLockMapMove(mapTypeId, isLock);
    }

    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mIMapApi.getCurrentMapMode(mapTypeId);
    }

    public boolean setMapMode(MapType mapTypeId, MapMode mapMode, boolean isSave) {
        if(isSave){
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

    public PointDataInfo lonLatToScreen(MapType mapTypeId,double lon, double lat, double z) {
        return mIMapApi.lonLatToScreen(mapTypeId, lon, lat,z);
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

    public void exitPreview(MapType mapTypeId, boolean bCenter) {
        mIMapApi.exitPreview(mapTypeId, bCenter);
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType type) {
        mIMapApi.updateUiStyle(mapTypeId, type);
        mSettingAdapter.setConfigKeyDayNightMode(type);
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

    public void setMapLabelClickable(MapType mapTypeId,boolean enable){
        mIMapApi.setMapLabelClickable(mapTypeId,enable);
    }

    public void resetTickCount(MapType mapTypeId, int tickCount) {
        mIMapApi.resetTickCount(mapTypeId, tickCount);
    }

    public void openOrCloseScreenshot(MapType mapTypeId, boolean isOpen) {
        mIMapApi.openOrCloseScreenshot(mapTypeId, isOpen);
    }

    public void updateScreenshotRect(MapType mapTypeId, Rect rect) {
        mIMapApi.updateScreenshotRect(mapTypeId, rect);
    }
}
