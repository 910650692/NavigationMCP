package com.fy.navi.service.logicpaket.map;

import android.os.Bundle;
import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.aos.BlAosAdapter;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.IsEnterPreviewCallback;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.search.PoiInfoEntity;

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

    @Override
    public void onNaviStatusChange(String naviStatus) {
        MapTypeId mapTypeId = MapTypeId.MAIN_SCREEN_MAIN_MAP;
        List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
        for (IMapPackageCallback iMapPackageCallback : callbacks) {
            iMapPackageCallback.onNaviStatusChange(naviStatus);
        }
    }

    private static final class Helper {
        private static final MapPackage ep = new MapPackage();
    }

    private MapPackage() {
        mMapAdapter = MapAdapter.getInstance();
        mPositionAdapter = PositionAdapter.getInstance();
        layerAdapter = LayerAdapter.getInstance();
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        blAosAdapter = BlAosAdapter.getInstance();
        mNavistatusAdapter.registerCallback(this);
    }

    private MapAdapter mMapAdapter;
    private PositionAdapter mPositionAdapter;
    private LayerAdapter layerAdapter;
    private NavistatusAdapter mNavistatusAdapter;
    private BlAosAdapter blAosAdapter;
    private final Hashtable<MapTypeId, List<IMapPackageCallback>> callbackTables = new Hashtable<>();

    public boolean init(MapTypeId mapTypeId) {
        return mMapAdapter.init(mapTypeId);
    }


    public void initMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.initMapView(mapSurfaceView);
        mMapAdapter.registerCallback(mapSurfaceView.provideMapTypeId(), this);
    }

    public void unBindMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.unBindMapView(mapSurfaceView);
    }

    public void unInitMapView(IBaseScreenMapView mapSurfaceView) {
        mMapAdapter.unInitMapView(mapSurfaceView);
    }

    public void registerCallback(MapTypeId mapTypeId, IMapPackageCallback callback) {
        if (!callbackTables.containsKey(mapTypeId) || callbackTables.get(mapTypeId) == null) {
            callbackTables.put(mapTypeId, new CopyOnWriteArrayList<>());
        }
        if (!callbackTables.get(mapTypeId).contains(callback)) {
            callbackTables.get(mapTypeId).add(callback);
        }
    }

    public void unRegisterCallback(MapTypeId mapTypeId, IMapPackageCallback observer) {
        if (callbackTables.get(mapTypeId) != null) {
            callbackTables.get(mapTypeId).remove(observer);
        }
    }

    public void unInitMapService() {
        mMapAdapter.unInitMapService();
    }

    public void reduceLevel(MapTypeId mapTypeId) {
        mMapAdapter.reduceLevel(mapTypeId);
    }

    public void setZoomLevel(MapTypeId mapTypeId, float level) {
        mMapAdapter.setZoomLevel(mapTypeId, level);
    }

    public float getZoomLevel(MapTypeId mapTypeId) {
        return mMapAdapter.getZoomLevel(mapTypeId);
    }

    public void amplifyLevel(MapTypeId mapTypeId) {
        mMapAdapter.amplifyLevel(mapTypeId);
    }

    public void setMapCenterInScreen(MapTypeId mapTypeId, int x, int y) {
        mMapAdapter.setMapCenterInScreen(mapTypeId, x, y);
    }

    public void setMapCenter(MapTypeId mapTypeId, GeoPoint geoPoint) {
        mMapAdapter.setMapCenter(mapTypeId, geoPoint);
    }

    public void setMapViewTextSize(MapTypeId mapTypeId, float f) {
        mMapAdapter.setMapViewTextSize(mapTypeId, f);
    }

    public void switchMapMode(MapTypeId mapTypeId) {
        mMapAdapter.switchMapMode(mapTypeId);
    }

    public MapMode getCurrentMapMode(MapTypeId mapTypeId) {
        return mMapAdapter.getCurrentMapMode(mapTypeId);
    }

    public void switchMapMode(MapTypeId mapTypeId, MapMode mapMode) {
        mMapAdapter.switchMapMode(mapTypeId, mapMode);
    }

    public void setMapStateStyle(MapTypeId mapTypeId, MapStateStyle mapStateStyle) {
        mMapAdapter.setMapStateStyle(mapTypeId, mapStateStyle);
    }

    public void goToCarPosition(MapTypeId mapTypeId) {
        mMapAdapter.goToCarPosition(mapTypeId, false, true);
    }

    public void goToCarPosition(MapTypeId mapTypeId, boolean bAnimation, boolean changeLevel) {
        mMapAdapter.goToCarPosition(mapTypeId, bAnimation, changeLevel);
    }

    public GeoPoint mapToLonLat(MapTypeId mapTypeId, double mapX, double mapY) {
        return mMapAdapter.mapToLonLat(mapTypeId, mapX, mapY);
    }

    public MapSurfaceViewSizeParams getMapSurfaceParam(MapTypeId mapTypeId) {
        return mMapAdapter.getMapSurfaceParam(mapTypeId);
    }

    public void showPreview(MapTypeId mapTypeId, PreviewParams previewParams) {
        mMapAdapter.showPreview(mapTypeId, previewParams);
    }

    public void exitPreview(MapTypeId mapTypeId) {
        mMapAdapter.exitPreview(mapTypeId);
    }

    public void addIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mMapAdapter.addIsEnterPreviewCallback(mapTypeId, callback);
    }

    public void removeIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback) {
        mMapAdapter.removeIsEnterPreviewCallback(mapTypeId, callback);
    }

    public boolean getIsEnterPreview(MapTypeId mapTypeId) {
        return mMapAdapter.getIsEnterPreview(mapTypeId);
    }

    public static MapPackage getInstance() {
        return Helper.ep;
    }


    @Override
    public void onMapCenterChanged(MapTypeId mapTypeId, double lon, double lat) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapCenterChanged(mapTypeId, lon, lat);
            }
        }
    }

    @Override
    public void onMapLevelChanged(MapTypeId mapTypeId, float mapLevel) {
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
    public void onMapClickBlank(MapTypeId mapTypeId, float px, float py) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapClickBlank(mapTypeId, px, py);
            }
        }
    }

    @Override
    public void onMapClickLabel(MapTypeId mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
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
    public void onMapMove(MapTypeId mapTypeId, long px, long py, boolean moveEnd) {
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
    public void onMapScaleChanged(MapTypeId mapTypeId, int currentScale) {
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
    public void onMapInitSuccess(MapTypeId mapTypeId, boolean success) {
        Logger.d(TAG, "onMapInitSuccess");
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapInitSuccess(mapTypeId, success);
            }
        }
    }

    @Override
    public void onMapLoadSuccess(MapTypeId mapTypeId) {
        if (mapTypeId == MapTypeId.MAIN_SCREEN_MAIN_MAP) {
            layerAdapter.registerLayerClickObserver(MapTypeId.MAIN_SCREEN_MAIN_MAP, LayerType.SEARCH_LAYER, this);
            layerAdapter.registerLayerClickObserver(MapTypeId.MAIN_SCREEN_MAIN_MAP, LayerType.ROUTE_LAYER, this);
        }
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
    public void onMapTouchEvent(MapTypeId mapTypeId, MotionEvent touchEvent) {
        if (callbackTables.containsKey(mapTypeId) && callbackTables.get(mapTypeId) != null) {
            List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
            if (ConvertUtils.isEmpty(callbacks)) return;
            for (IMapPackageCallback callback : callbacks) {
                callback.onMapTouchEvent(mapTypeId, touchEvent);
            }
        }
    }

    @Override
    public void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
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
    public void onOpenLayer(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
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
    public void onReversePoiClick(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
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
    public void onMapModeChange(MapTypeId mapTypeId, MapMode mapMode) {
        List<IMapPackageCallback> callbacks = callbackTables.get(mapTypeId);
        if (ConvertUtils.isEmpty(callbacks)) return;
        for (IMapPackageCallback callback : callbacks) {
            callback.onMapModeChange(mapTypeId, mapMode);
        }
    }

    public void updateUiStyle(MapTypeId mapTypeId, int uiMode) {
        mMapAdapter.updateUiStyle(mapTypeId, uiMode);
        // 通知其它地方UI发生了变化
        Logger.d(TAG, "onUiModeChanged!");
        callbackTables.forEach((key, values) -> values.forEach(callBack -> {
            callBack.onUiModeChanged(uiMode);
        }));
    }

    public void saveLastLocationInfo() {
        if (mPositionAdapter != null) {
            mPositionAdapter.saveLocStorage();
        }
    }

    public void set3DBuilding(MapTypeId mapTypeId, boolean isShow) {
        mMapAdapter.set3DBuilding(mapTypeId, isShow);
    }

    public String getNaviStatus() {
        return mNavistatusAdapter.getCurrentNaviStatus();
    }

    // 获取当前缩放比例尺
    public float getCurrentZoomLevel(MapTypeId mapTypeId) {
        return mMapAdapter.getCurrentZoomLevel(mapTypeId);
    }

    // 返回当前比例尺所表示的地理长度（单位：米）
    public int getCurrentZoomScale(MapTypeId mapTypeId) {
        return mMapAdapter.getCurrentZoomScale(mapTypeId);
    }

    /***
     * 设置TMC
     * @param mapTypeId
     * @param isOpen
     * @return true means success
     */
    public boolean setTrafficStates(MapTypeId mapTypeId, boolean isOpen) {
        return mMapAdapter.setTrafficStates(mapTypeId, isOpen);
    }

    /**
     * 地图POI分类控制显隐
     *
     * @param mapTypeId
     * @param typeList
     * @param isOpen
     */
    public void setCustomLabelTypeVisible(MapTypeId mapTypeId, ArrayList<Integer> typeList, boolean isOpen) {
        mMapAdapter.setCustomLabelTypeVisible(mapTypeId, typeList, isOpen);
    }

    @Override
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        ILayerAdapterCallBack.super.onNotifyClick(mapTypeId, layer, pItem);
        callbackTables.forEach((key, values) -> values.forEach(callBack -> {
            callBack.onNotifyClick(key,layer, pItem);
        }));
    }

    /**
     * 语音打开HMI界面.
     * @param mapTypeId MapTypeId，对应底图.
     * @param bundle Bundle，传递打开界面后执行需要的参数.
     */
    public void voiceOpenSearchPage(MapTypeId mapTypeId, Bundle bundle) {
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

    public void setPitchAngle(MapTypeId mapTypeId, float pitch, boolean isAnimation, boolean isSync) {
        mMapAdapter.setPitchAngle(mapTypeId, pitch, isAnimation, isSync);
    }
}
