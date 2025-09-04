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
        if (mapSurfaceViewImp != null) {
            mapSurfaceViewImp.changeMapViewParams(mapViewParams);
            mapView.bindMapView(mapSurfaceViewImp);
        }
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
        if (mapSurfaceViewImp != null) {
            mapSurfaceViewImp.changeMapViewParams(mapViewParams);
        }
    }

    @Override
    public void unBindMapView(IBaseScreenMapView mapView) {
        MapViewImpl mapSurfaceViewImp = mapViewPoolManager.getMapViewImpl(mapView.provideMapTypeId());
        if (mapSurfaceViewImp != null) {
            mapView.unBindMapView(mapSurfaceViewImp);
        }
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
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.getCurrentZoomLevel();
        }
        return 0;
    }

    /* 判断当前mapview是否存在 */
    @Override
    public boolean isMapViewExist(MapType mapTypeId) {
        return mapViewPoolManager.isMapViewExist(mapTypeId);
    }

    @Override
    public int getCurrentScale(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.getCurrentScale();
        }
        return 0;
    }

    @Override
    public void setZoomLevel(MapType mapTypeId, float levelValue) {
        float level = (float) Math.floor(levelValue);
        if (AutoMapConstant.MAP_ZOOM_LEVEL_MIN > level || level > AutoMapConstant.MAP_ZOOM_LEVEL_MAX)
            return;
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setZoomLevel(level);
        }
    }

    @Override
    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            Logger.d(TAG, "map left: " + x, "map top: " + y);
            mapViewImpl.setMapCenterInScreen(x, y);
        }
    }

    /**
     * 设置Hud地图中线点在屏幕中的位置
     */
    @Override
    public void setHudMapCenterInScreen(MapType mapTypeId, int x, int y) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            Logger.d(TAG, "map left: ", x, " map top: ", y);
            mapViewImpl.setHudMapCenterInScreen(x, y);
        }
    }

    @Override
    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setMapCenter(geoPoint);
        }
    }

    @Override
    public GeoPoint getMapCenter(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.getMapCenter();
        }
        return new GeoPoint();
    }

    @Override
    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.setTrafficStates(isOpen);
        }
        return false;
    }

    @Override
    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setCustomLabelTypeVisable(typeList, visible);
        }
    }

    @Override
    public void setMapViewTextSize(MapType mapTypeId, float f) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setMapViewTextSize(f);
        }
    }

    public void isSplitScreen(MapType mapTypeId, boolean isSplit) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.isSplitScreen(isSplit);
        }
    }

    @Override
    public void setLockMapPinchZoom(MapType mapTypeId, boolean isLock) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setLockMapPinchZoom(isLock);
        }
    }

    @Override
    public void setLockMapMove(MapType mapTypeId, boolean isLock) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setLockMapMove(isLock);
        }
    }

    @Override
    public MapMode getCurrentMapMode(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.getMapMode();
        }
        return MapMode.UP_2D;
    }

    @Override
    public boolean setMapMode(MapType mapTypeId, MapMode mapMode) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.setMapMode(mapMode);
        }
        return false;
    }

    @Override
    public boolean setMapModeWithLevel(MapType mapTypeId, MapMode mapMode, float level) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.setMapMode(mapMode, level);
        }
        return false;
    }

    @Override
    public void setMapHudMode(MapType mapTypeId, MapMode mapMode) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setHUDMapMode(mapMode);
        }
    }

    @Override
    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setMapStyle(mapStateStyle);
        }
    }

    @Override
    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.goToCarPosition(bAnimation, changeLevel);
        }
    }

    @Override
    public void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.mfcMoveMap(mfcController, moveDistance);
        }
    }

    @Override
    public GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.mapToLonLat(mapX, mapY);
        }
        return new GeoPoint();
    }

    @Override
    public PointDataInfo lonLatToScreen(MapType mapTypeId, double lon, double lat, double z) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.lonLatToScreen(lon, lat, z);
        }
        return new PointDataInfo();
    }

    @Override
    public MapViewParams getMapSurfaceParam(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.getMapViewParams();
        }
        return new MapViewParams();
    }

    @Override
    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.showPreview(previewParams);
        }
    }

    @Override
    public void exitPreview(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.exitPreview();
        }
    }

    @Override
    public void exitPreview(MapType mapTypeId, boolean bCenter) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.exitPreview(bCenter);
        }
    }

    @Override
    public void updateUiStyle(MapType mapTypeId, ThemeType uiMode) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.updateUiStyle(uiMode);
        }
    }

    @Override
    public String getMapBound(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            return mapViewImpl.getMapBound();
        }
        return "";
    }

    @Override
    public void set3DBuilding(MapType mapTypeId, boolean isVisible) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.update3DBuildingSwitch(isVisible);
        }
    }

    @Override
    public void setMapLabelClickable(MapType mapTypeId, boolean enable) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setMapLabelClickable(enable);
        }
    }

    @Override
    public void resetTickCount(MapType mapTypeId, int tickCount) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.resetTickCount(tickCount);
        }
    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return BizLayerUtil.calcDistanceBetweenPoints(new Coord2DDouble(startPoint.getLon(), startPoint.getLat()),
                new Coord2DDouble(endPoint.getLon(), endPoint.getLat()));
    }

    @Override
    public void openOrCloseScreenshot(MapType mapTypeId, boolean isOpen) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.openOrCloseScreenshot(isOpen);
        }
    }

    @Override
    public void updateScreenshotRect(MapType mapTypeId, Rect rect) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.updateScreenshotRect(rect);
        }
    }

    @Override
    public void setPitchAngle(MapType mapTypeId,float pitchAngle) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.setPitchAngle(pitchAngle);
        }
    }

    @Override
    public int getScaleLineLength(MapType mapType) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapType);
        if (mapViewImpl != null) {
            return mapViewImpl.getScaleLineLength();
        }
        return 0;
    }

    @Override
    public void updateLayerStyle(MapType mapTypeId) {
        MapViewImpl mapViewImpl = mapViewPoolManager.getMapViewImpl(mapTypeId);
        if (mapViewImpl != null) {
            mapViewImpl.updateLayerStyle();
        }
    }
}
