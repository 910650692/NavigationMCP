package com.fy.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.MapVisibleAreaDataManager;
import com.fy.navi.service.define.map.MapVisibleAreaInfo;
import com.fy.navi.service.define.map.MapVisibleAreaType;
import com.fy.navi.service.define.map.PointDataInfo;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.mfc.MfcController;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.search.PoiInfoEntity;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class MapPackage implements IMapAdapterCallback, INaviStatusCallback, ILayerAdapterCallBack {
    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;
    private MapAdapter mMapAdapter;
    private LayerAdapter layerAdapter;
    private PositionAdapter mPositionAdapter;
    private NavistatusAdapter mNavistatusAdapter;
    private final Hashtable<MapType, List<IMapPackageCallback>> callbackTables = new Hashtable<>();

    private static final class Helper {
        private static final MapPackage ep = new MapPackage();
    }

    public static MapPackage getInstance() {
        return Helper.ep;
    }

    private MapPackage() {
        mMapAdapter = MapAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mNavistatusAdapter.registerCallback(this);
    }

    public void initMapService() {
        mPositionAdapter = PositionAdapter.getInstance();
        layerAdapter = LayerAdapter.getInstance();
        mMapAdapter.initMapService();
    }

    public boolean createMapView(MapType mapTypeId) {
        boolean createMapViewResult = mMapAdapter.createMapView(mapTypeId);
        mMapAdapter.registerCallback(mapTypeId, this);
        return createMapViewResult;
    }

    public void changeMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.changeMapView(mapSurfaceView);
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.bindMapView(mapSurfaceView);
    }

    public void unBindMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.unBindMapView(mapSurfaceView);
        mMapAdapter.unregisterCallback(mapSurfaceView.provideMapTypeId(), this);
    }

    public void destroyMapView(MapType mapType) {
        mMapAdapter.destroyMapView(mapType);
    }

    public void registerCallback(MapType mapTypeId, IMapPackageCallback callback) {
        if (!callbackTables.containsKey(mapTypeId) || callbackTables.get(mapTypeId) == null) {
            callbackTables.put(mapTypeId, new CopyOnWriteArrayList<>());
        }
        if (!callbackTables.get(mapTypeId).contains(callback)) {
            callbackTables.get(mapTypeId).add(callback);
        }
    }

    public void unRegisterCallback(MapType mapTypeId, IMapPackageCallback observer) {
        if (callbackTables.get(mapTypeId) != null) {
            callbackTables.get(mapTypeId).remove(observer);
        }
    }

    public void unInitMapService() {
        mMapAdapter.unInitMapService();
    }

    public void reduceLevel(MapType mapTypeId) {
        mMapAdapter.reduceLevel(mapTypeId);
    }

    public void setZoomLevel(MapType mapTypeId, float level) {
        mMapAdapter.setZoomLevel(mapTypeId, level);
    }

    public float getZoomLevel(MapType mapTypeId) {
        return mMapAdapter.getZoomLevel(mapTypeId);
    }

    public void amplifyLevel(MapType mapTypeId) {
        mMapAdapter.amplifyLevel(mapTypeId);
    }

    public void setMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mMapAdapter.setMapCenterInScreen(mapTypeId, x, y);
    }
    /**
     * 设置Hud地图中线点在屏幕中的位置
     */
    public void setHudMapCenterInScreen(MapType mapTypeId, int x, int y) {
        mMapAdapter.setHudMapCenterInScreen(mapTypeId, x, y);
    }


    public void changMapCenterInScreen(MapType mapTypeId, MapVisibleAreaType mapVisibleAreaType) {
        MapVisibleAreaInfo mapVisibleAreaInfo = MapVisibleAreaDataManager.getInstance().getDataByKey(mapVisibleAreaType);
        Logger.d("MapViewModelonCreate4"+  mapVisibleAreaInfo.getMleftscreenoffer()+"--"+mapVisibleAreaInfo.getMtopscreenoffer());
        if (!ConvertUtils.isEmpty(mapVisibleAreaInfo)) {
            mMapAdapter.setMapCenterInScreen(mapTypeId, mapVisibleAreaInfo.getMleftscreenoffer(), mapVisibleAreaInfo.getMtopscreenoffer());
        }
    }

    public void setMapCenter(MapType mapTypeId, GeoPoint geoPoint) {
        mMapAdapter.setMapCenter(mapTypeId, geoPoint);
    }

    public GeoPoint getMapCenter(MapType mapTypeId) {
        return mMapAdapter.getMapCenter(mapTypeId);
    }

    public boolean isCarLocation(MapType mapTypeId, double maxDistance) {
        GeoPoint mapCenter = getMapCenter(mapTypeId);
        LocInfoBean locInfoBean = mPositionAdapter.getLastCarLocation();
        if (!ConvertUtils.isEmpty(mapCenter) && !ConvertUtils.isEmpty(locInfoBean)) {
            GeoPoint carLocInfo = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
            double distance = layerAdapter.calcStraightDistance(mapCenter, carLocInfo);
            BigDecimal num1 = new BigDecimal(distance);
            BigDecimal num2 = new BigDecimal(maxDistance);
            //判断num1是否大于num2
            int result = num1.compareTo(num2);
            return result > 0;
        }
        return false;
    }

    public void setMapViewTextSize(MapType mapTypeId, float f) {
        mMapAdapter.setMapViewTextSize(mapTypeId, f);
    }

    public boolean switchMapMode(MapType mapTypeId) {
        return mMapAdapter.switchMapMode(mapTypeId);
    }

    public MapMode getCurrentMapMode(MapType mapTypeId) {
        return mMapAdapter.getCurrentMapMode(mapTypeId);
    }

    public boolean switchMapMode(MapType mapTypeId, MapMode mapMode, boolean isSave) {
        return mMapAdapter.setMapMode(mapTypeId, mapMode, isSave);
    }

    public void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle) {
        mMapAdapter.setMapStateStyle(mapTypeId, mapStateStyle);
    }

    public void goToCarPosition(MapType mapTypeId) {
        mMapAdapter.goToCarPosition(mapTypeId, false, true);
    }

    public void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel) {
        mMapAdapter.goToCarPosition(mapTypeId, bAnimation, changeLevel);
    }

    public void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance) {
        mMapAdapter.mfcMoveMap(mapTypeId, mfcController, moveDistance);
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapTouchEvent(mapTypeId, null);
            }
        }
    }

    public void setMapLabelClickable(MapType mapTypeId,boolean enable){
        mMapAdapter.setMapLabelClickable(mapTypeId,enable);
    }

    public GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY) {
        return mMapAdapter.mapToLonLat(mapTypeId, mapX, mapY);
    }

    public PointDataInfo lonLatToScreen(MapType mapTypeId, double lon, double lat, double z) {
        return mMapAdapter.lonLatToScreen(mapTypeId, lon, lat,z);
    }

    public MapViewParams getMapSurfaceParam(MapType mapTypeId) {
        return mMapAdapter.getMapSurfaceParam(mapTypeId);
    }

    public void showPreview(MapType mapTypeId, PreviewParams previewParams) {
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }

    public void exitPreview(MapType mapTypeId) {
        mMapAdapter.exitPreview(mapTypeId);
    }


    @Override
    public void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapCenterChanged(mapTypeId, lon, lat);
            }
        }
    }

    @Override
    public void onMapLevelChanged(MapType mapTypeId, float mapLevel) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onMapLevelChanged(mapTypeId, mapLevel);
                }
            }
        });
    }

    @Override
    public void onMapClickBlank(MapType mapTypeId, float px, float py) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapClickBlank(mapTypeId, px, py);
            }
        }
    }

    @Override
    public void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onMapClickLabel(mapTypeId, pLabels);
                }
            }
        });
    }

    @Override
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onMapMove(mapTypeId, px, py, moveEnd);
                }
            }
        });
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onMapScaleChanged(mapTypeId, currentScale);
                }
            }
        });
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        Logger.i(TAG, "lvww", "底图渲染成功", "mapTypeId:" + mapTypeId.name());
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapLoadSuccess(mapTypeId);
            }
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapTouchEvent(mapTypeId, touchEvent);
            }
        }
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onMapClickPoi(mapTypeId, poiInfo);
                }
            }
        });
    }

    @Override
    public void onOpenLayer(MapType mapTypeId, PoiInfoEntity poiInfo) {
        Logger.i(TAG, "onOpenLayer");
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onQueryTrafficEvent(mapTypeId, poiInfo);
                }
            }
        });
    }

    @Override
    public void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    callback.onReversePoiClick(mapTypeId, poiInfo);
                }
            }
        });
    }

    @Override
    public void onMapModeChange(MapType mapTypeId, MapMode mapMode) {
        List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (IMapPackageCallback callback : callbacks) {
            callback.onMapModeChange(mapTypeId, mapMode);
        }
    }

    @Override
    public void isEnterPreview(boolean isEnterPreview) {

    }

    @Override
    public void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
        List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
        if (ConvertUtils.isEmpty(callbacks)) return;
        callbacks.forEach(iEglScreenshotCallBack -> {
            iEglScreenshotCallBack.onEGLScreenshot(mapTypeId, bytes);
        });
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType type) {
        mMapAdapter.updateUiStyle(mapTypeId, type);
    }

    public void saveLastLocationInfo() {
        if (mPositionAdapter != null) {
            mPositionAdapter.saveLocStorage();
        }
    }

    public void set3DBuilding(MapType mapTypeId, boolean isShow) {
        mMapAdapter.set3DBuilding(mapTypeId, isShow);
    }

    public String getNaviStatus() {
        return mNavistatusAdapter.getCurrentNaviStatus();
    }

    // 获取当前缩放比例尺
    public float getCurrentZoomLevel(MapType mapTypeId) {
        return mMapAdapter.getCurrentZoomLevel(mapTypeId);
    }

    // 返回当前比例尺所表示的地理长度（单位：米）
    public int getCurrentZoomScale(MapType mapTypeId) {
        return mMapAdapter.getCurrentZoomScale(mapTypeId);
    }

    /***
     * 设置TMC
     * @param mapTypeId
     * @param isOpen
     * @return true means success
     */
    public boolean setTrafficStates(MapType mapTypeId, boolean isOpen) {
        return mMapAdapter.setTrafficStates(mapTypeId, isOpen);
    }

    /**
     * 地图POI分类控制显隐
     *
     * @param mapTypeId
     * @param typeList
     * @param isOpen
     */
    public void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean isOpen) {
        mMapAdapter.setCustomLabelTypeVisible(mapTypeId, typeList, isOpen);
    }

    /**
     * 语音打开HMI界面.
     *
     * @param mapTypeId MapTypeId，对应底图.
     * @param bundle    Bundle，传递打开界面后执行需要的参数.
     */
    public void voiceOpenHmiPage(MapType mapTypeId, Bundle bundle) {
        ThreadManager.getInstance().postUi(() -> {
            if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
                List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
                if (ConvertUtils.isEmpty(callbacks)) return;
                for (IMapPackageCallback callback : callbacks) {
                    if (null != callback) {
                        callback.onVoiceOpenPage(mapTypeId, bundle);
                    }
                }
            }
        });
    }

    @Override
    public void onNaviStatusChange(String naviStatus) {
        MapType mapTypeId = MapType.MAIN_SCREEN_MAIN_MAP;
        List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
        for (IMapPackageCallback iMapPackageCallback : callbacks) {
            iMapPackageCallback.onNaviStatusChange(naviStatus);
        }
    }
}
