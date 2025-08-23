package com.sgm.navi.service.adapter.map.bls;

import android.graphics.Rect;

import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.adapter.map.IMapApi;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapViewParams;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.PointDataInfo;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.mfc.MfcController;
import com.android.utils.ScreenTypeUtils;

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
    public void initMapService() {
        mapViewPoolManager.initMapService();
    }

    @Override
    public void unInitMapService() {
        mapViewPoolManager.unInitMapService();
    }

    @Override
    public boolean createMapView(MapType mapTypeId) {
        return mapViewPoolManager.createMapView(mapTypeId);
    }

    @Override
    public void bindMapView(IBaseScreenMapView mapView) {
        int mapViewWidth;
        int mapViewHeight;
        int screenWidth;
        int screenHeight;
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()
                && mapView.provideMapTypeId() == MapType.MAIN_SCREEN_MAIN_MAP) {
            mapViewWidth = ScreenTypeUtils.getInstance().getCarWidthOneThree();
            mapViewHeight = ScreenTypeUtils.getInstance().getCarHeight();
            screenWidth = ScreenTypeUtils.getInstance().getCarWidthOneThree();
            screenHeight =  ScreenTypeUtils.getInstance().getCarHeight();
        } else if (ScreenTypeUtils.getInstance().isTwoThirdScreen()
                && mapView.provideMapTypeId() == MapType.MAIN_SCREEN_MAIN_MAP){
            mapViewWidth = ScreenTypeUtils.getInstance().getCarWidthTwoThree();
            mapViewHeight =  ScreenTypeUtils.getInstance().getCarHeight();
            screenWidth = ScreenTypeUtils.getInstance().getCarWidthTwoThree();
            screenHeight =  ScreenTypeUtils.getInstance().getCarHeight();
        } else {
            mapViewWidth = (int) mapView.getMapViewWidth();
            mapViewHeight = (int) mapView.getMapViewHeight();
            screenWidth = (int) mapView.getScreenWidth();
            screenHeight = (int) mapView.getScreenHeight();
        }
        Logger.d("screen_change_used", mapViewWidth,mapViewHeight,screenWidth,screenHeight);
        MapViewParams mapViewParams = new MapViewParams(mapView.getMapViewX(), mapView.getMapViewY(),
                mapViewWidth, mapViewHeight, screenWidth, screenHeight,
                mapView.getScreenDensityDpi(), mapView.isOpenScreen());
        MapViewImpl mapSurfaceViewImp = mapViewPoolManager.getMapViewImpl(mapView.provideMapTypeId());
        mapSurfaceViewImp.changeMapViewParams(mapViewParams);
        mapView.bindMapView(mapSurfaceViewImp);
    }

    @Override
    public void changeMapViewParams(IBaseScreenMapView mapView) {
        int mapViewWidth;
        int mapViewHeight;
        int screenWidth;
        int screenHeight;
        if (ScreenTypeUtils.getInstance().isOneThirdScreen() && mapView.provideMapTypeId() == MapType.MAIN_SCREEN_MAIN_MAP) {
            mapViewWidth = ScreenTypeUtils.getInstance().getCarWidthOneThree();
            mapViewHeight = ScreenTypeUtils.getInstance().getCarHeight();
            screenWidth = ScreenTypeUtils.getInstance().getCarWidthOneThree();
            screenHeight =  ScreenTypeUtils.getInstance().getCarHeight();
        } else if (ScreenTypeUtils.getInstance().isTwoThirdScreen() && mapView.provideMapTypeId() == MapType.MAIN_SCREEN_MAIN_MAP){
            mapViewWidth = ScreenTypeUtils.getInstance().getCarWidthTwoThree();
            mapViewHeight =  ScreenTypeUtils.getInstance().getCarHeight();
            screenWidth = ScreenTypeUtils.getInstance().getCarWidthTwoThree();
            screenHeight =  ScreenTypeUtils.getInstance().getCarHeight();
        } else {
            mapViewWidth = (int) mapView.getMapViewWidth();
            mapViewHeight = (int) mapView.getMapViewHeight();
            screenWidth = (int) mapView.getScreenWidth();
            screenHeight = (int) mapView.getScreenHeight();
        }
        Logger.d("screen_change_used", mapViewWidth,mapViewHeight,screenWidth,screenHeight);
        MapViewParams mapViewParams = new MapViewParams(mapView.getMapViewX(), mapView.getMapViewY(),mapViewWidth
                , mapViewHeight, screenWidth, screenHeight, mapView.getScreenDensityDpi(), mapView.isOpenScreen());
        MapViewImpl mapSurfaceViewImp = mapViewPoolManager.getMapViewImpl(mapView.provideMapTypeId());
        mapSurfaceViewImp.changeMapViewParams(mapViewParams);
    }

    @Override
    public void unBindMapView(IBaseScreenMapView mapView) {
        MapViewImpl mapSurfaceViewImp = mapViewPoolManager.getMapViewImpl(mapView.provideMapTypeId());
        mapView.unBindMapView(mapSurfaceViewImp);
    }

    @Override
    public void destroyMapView(MapType mapTypeId) {
        mapViewPoolManager.destroyMapView(mapTypeId);
    }

    @Override
    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mapViewPoolManager.registerCallback(mapTypeId, callback);
    }

    @Override
    public void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        mapViewPoolManager.unRegisterCallback(mapTypeId, callback);
    }

    @Override
    public float getCurrentZoomLevel(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getCurrentZoomLevel();
    }

    /* 判断当前mapview是否存在 */
    @Override
    public boolean isMapViewExist(MapType mapTypeId) {
        return mapViewPoolManager.isMapViewExist(mapTypeId);
    }

    @Override
    public int getCurrentScale(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getCurrentScale();
    }

    @Override
    public void setZoomLevel(MapType mapTypeId, float levelValue) {
        float level = (float) Math.floor(levelValue);
        if (AutoMapConstant.MAP_ZOOM_LEVEL_MIN > level || level > AutoMapConstant.MAP_ZOOM_LEVEL_MAX)
            return;
        mapViewPoolManager.getMapViewImpl(mapTypeId).setZoomLevel(level);
    }

    @Override
    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        Logger.d(TAG, "map left: " + x, "map top: " + y);
        mapViewPoolManager.getMapViewImpl(mapTypeId).setMapCenterInScreen(x, y);
    }

    /**
     * 设置Hud地图中线点在屏幕中的位置
     */
    @Override
    public void setHudMapCenterInScreen(MapType mapTypeId, int x, int y) {
        Logger.d(TAG, "map left: ", x, " map top: ", y);
        mapViewPoolManager.getMapViewImpl(mapTypeId).setHudMapCenterInScreen(x, y);
    }

    @Override
    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setMapCenter(geoPoint);
    }

    @Override
    public GeoPoint getMapCenter(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getMapCenter();
    }

    @Override
    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).setTrafficStates(isOpen);
    }

    @Override
    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setCustomLabelTypeVisable(typeList, visible);
    }

    @Override
    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setMapViewTextSize(f);
    }

    public void isSplitScreen(MapType mapTypeId, boolean isSplit) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).isSplitScreen(isSplit);
    }

    @Override
    public void setLockMapPinchZoom(MapType mapTypeId, boolean isLock) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setLockMapPinchZoom(isLock);
    }

    @Override
    public void setLockMapMove(MapType mapTypeId, boolean isLock) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setLockMapMove(isLock);
    }

    @Override
    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getMapMode();
    }

    @Override
    public boolean setMapMode(MapType mapTypeId, MapMode mapMode) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).setMapMode(mapMode);
    }

    @Override
    public boolean setMapModeWithLevel(MapType mapTypeId, MapMode mapMode, float level) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).setMapMode(mapMode, level);
    }

    @Override
    public void setMapHudMode(MapType mapTypeId, MapMode mapMode) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setHUDMapMode(mapMode);
    }

    @Override
    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setMapStyle(mapStateStyle);
    }

    @Override
    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).goToCarPosition(bAnimation, changeLevel);
    }

    @Override
    public void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).mfcMoveMap(mfcController, moveDistance);
    }

    @Override
    public GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).mapToLonLat(mapX, mapY);
    }

    @Override
    public PointDataInfo lonLatToScreen(MapType mapTypeId, double lon, double lat, double z) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).lonLatToScreen(lon, lat, z);
    }

    @Override
    public MapViewParams getMapSurfaceParam(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getMapViewParams();
    }

    @Override
    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).showPreview(previewParams);
    }

    @Override
    public void exitPreview(MapType mapTypeId) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).exitPreview();
    }

    @Override
    public void exitPreview(MapType mapTypeId, boolean bCenter) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).exitPreview(bCenter);
    }

    @Override
    public void updateUiStyle(MapType mapTypeId, ThemeType uiMode) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).updateUiStyle(uiMode);
    }

    @Override
    public String getMapBound(MapType mapTypeId) {
        return mapViewPoolManager.getMapViewImpl(mapTypeId).getMapBound();
    }

    @Override
    public void set3DBuilding(MapType mapTypeId, boolean isVisible) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).update3DBuildingSwitch(isVisible);
    }

    @Override
    public void setMapLabelClickable(MapType mapTypeId, boolean enable) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setMapLabelClickable(enable);
    }

    @Override
    public void resetTickCount(MapType mapTypeId, int tickCount) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).resetTickCount(tickCount);
    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return BizLayerUtil.calcDistanceBetweenPoints(new Coord2DDouble(startPoint.getLon(), startPoint.getLat()),
                new Coord2DDouble(endPoint.getLon(), endPoint.getLat()));
    }

    @Override
    public void openOrCloseScreenshot(MapType mapTypeId, boolean isOpen) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).openOrCloseScreenshot(isOpen);
    }

    @Override
    public void updateScreenshotRect(MapType mapTypeId, Rect rect) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).updateScreenshotRect(rect);
    }

    @Override
    public void setPitchAngle(MapType mapTypeId,float pitchAngle) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).setPitchAngle(pitchAngle);
    }

    @Override
    public int getScaleLineLength(MapType mapType) {
        return mapViewPoolManager.getMapViewImpl(mapType).getScaleLineLength();
    }

    @Override
    public void updateLayerStyle(MapType mapTypeId) {
        mapViewPoolManager.getMapViewImpl(mapTypeId).updateLayerStyle();
    }
}
