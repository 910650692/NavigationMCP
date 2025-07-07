package com.sgm.navi.service.adapter.map.bls;

import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.model.InitMapParam;
import com.autonavi.gbl.map.model.MapFontInfo;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.define.bean.MapLabelItemBean;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapScreenShotDataInfo;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

/**
 * 创建MapSurfaceView工厂
 */
public final class MapViewPoolManager implements IMapAdapterCallback {

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private final HashMap<MapType, MapViewImpl> mapViewPools = new HashMap<>();

    private final HashMap<MapType, List<IMapAdapterCallback>> callbacks = new HashMap<>();

    private AtomicReference<MapService> mapService = new AtomicReference<>();


    public static MapViewPoolManager getInstance() {
        return MapViewPoolManager.Holder.INSTANCE;
    }


    private static final class Holder {
        private static final MapViewPoolManager INSTANCE = new MapViewPoolManager();
    }

    private MapViewPoolManager() {

    }

    public synchronized void initMapService() {
        Logger.d(TAG, "初始化底图服务");
        InitMapParam initMapParam = new InitMapParam();
        initMapParam.systemParam.cpucorenum = Runtime.getRuntime().availableProcessors();
        initMapParam.systemParam.permitPreLoad = true;
        initMapParam.systemParam.platform = "android";
        initMapParam.systemParam.memory = 4;
        initMapParam.systemParam.memoryRation = 1.0F;
        /*** 基本数据路径地址URL **/
        initMapParam.basePath = GBLCacheFilePath.MAP_BASE_PATH;
        initMapParam.indoorPath = GBLCacheFilePath.MAP_INDOOR_PATH;

        /*** 地图数据路径绝对地址 **/
        initMapParam.dataPath = GBLCacheFilePath.MAP_DATA_DIR;
        /*** 配置引擎样式文件MapAssert的绝对地址 **/
        initMapParam.assetPath = GBLCacheFilePath.MAP_ASSET_DIR;
        //设置字体font_cn路径
        MapFontInfo mapFontInfo = new MapFontInfo();
        mapFontInfo.fontName = "font_cn";
        mapFontInfo.fontPath = GBLCacheFilePath.COPY_ASSETS_DIR + "font/font_cn.ttf";
        initMapParam.fontParam.overlayFontInfoList.add(mapFontInfo);
        //设置AmapNumber-Bold路径
        MapFontInfo mapFontInfo2 = new MapFontInfo();
        mapFontInfo2.fontName = "AmapNumber-Bold";
        mapFontInfo2.fontPath = GBLCacheFilePath.COPY_ASSETS_DIR + "font/AmapNumber-Bold.ttf";
        initMapParam.fontParam.overlayFontInfoList.add(mapFontInfo2);
        //设置Oswald-Regular路径
        MapFontInfo mapFontInfo3 = new MapFontInfo();
        mapFontInfo3.fontName = "Oswald-Regular";
        mapFontInfo3.fontPath = GBLCacheFilePath.COPY_ASSETS_DIR + "font/Oswald-Regular.ttf";
        initMapParam.fontParam.overlayFontInfoList.add(mapFontInfo3);
        //设置Roboto-Bold路径
        MapFontInfo mapFontInfo4 = new MapFontInfo();
        mapFontInfo4.fontName = "Roboto-Bold";
        mapFontInfo4.fontPath = GBLCacheFilePath.COPY_ASSETS_DIR + "font/Roboto-Bold.ttf";
        initMapParam.fontParam.overlayFontInfoList.add(mapFontInfo4);
        mapService.set((MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID));
        if (null != mapService.get()) {
            mapService.get().initMap(initMapParam);
        }
    }

    public void unInitMapService() {
        if (mapService.get() != null) {
            mapViewPools.clear();
            mapService.get().unitMap();
        }
    }

    public synchronized MapService getMapService() {
        if (mapService.get() == null) {
            Logger.e(TAG, "初始化底图服务");
            initMapService();
        }
        return mapService.get();
    }

    public boolean createMapView(MapType mapTypeId) {
        if (!mapViewPools.containsKey(mapTypeId)) {
            Logger.d(TAG, mapTypeId, "  创建底图 :");
            MapViewImpl mapView = new MapViewImpl(AppCache.getInstance().getMContext(), mapTypeId, getMapService());
            mapView.setCallbacks(this);
            mapViewPools.put(mapTypeId, mapView);
        }
        return mapViewPools.containsKey(mapTypeId);
    }

    public MapViewImpl getMapViewImpl(MapType mapTypeId) {
        if (!mapViewPools.containsKey(mapTypeId)) {
            Logger.e(TAG, "getMapViewImpl createMapView :", mapTypeId);
            createMapView(mapTypeId);
        }
        return ConvertUtils.isNullRequire(mapViewPools.get(mapTypeId), "获取对应的MapSurfaceViewImp失败 : " + mapTypeId);
    }

    public boolean isMapViewExist(MapType mapTypeId) {
        return mapViewPools.containsKey(mapTypeId);
    }

    public void registerCallback(MapType mapTypeId, IMapAdapterCallback observer) {
        if (!callbacks.containsKey(mapTypeId)) {
            callbacks.put(mapTypeId, new ArrayList<>());
        }
        if (!callbacks.get(mapTypeId).contains(observer)) {
            Logger.d(TAG, mapTypeId, " 注册回调 ：", observer.getClass().getSimpleName(), ";size =", callbacks.get(mapTypeId).size());
            callbacks.get(mapTypeId).add(observer);
        }
    }

    public void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback observer) {
        if (callbacks.get(mapTypeId).contains(observer)) {
            Logger.d(TAG, mapTypeId, " 移除回调 ：", observer.getClass().getSimpleName(), ";size =", callbacks.get(mapTypeId).size());
            callbacks.get(mapTypeId).remove(observer);
        }
    }

    public void destroyMapView(MapType mapTypeId) {
        if (mapViewPools.containsKey(mapTypeId)) {
            mapViewPools.get(mapTypeId).destroyMapView();
            callbacks.get(mapTypeId).clear();
            mapViewPools.remove(mapTypeId);
        }
    }


    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapLoadSuccess(mapTypeId);
                }
            });
        }
    }

    @Override
    public void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapCenterChanged(mapTypeId, lon, lat);
                }
            });
        }
    }

    @Override
    public void onMapLevelChanged(MapType mapTypeId, float mapLevel) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapLevelChanged(mapTypeId, mapLevel);
                }
            });
        }
    }

    @Override
    public void onMapClickBlank(MapType mapTypeId, float px, float py) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapClickBlank(mapTypeId, px, py);
                }
            });
        }
    }

    @Override
    public void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapClickLabel(mapTypeId, pLabels);
                }
            });
        }
    }

    @Override
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapMove(mapTypeId, px, py, moveEnd);
                }
            });
        }
    }

    @Override
    public void onMove(MapType mapTypeId, long px, long py) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMove(mapTypeId, px, py);
                }
            });
        }
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapScaleChanged(mapTypeId, currentScale);
                }
            });
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapTouchEvent(mapTypeId, touchEvent);
                }
            });
        }
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapClickPoi(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onOpenLayer(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onOpenLayer(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onReversePoiClick(mapTypeId, poiInfo);
                }
            });
        }
    }

    @Override
    public void onMapModeChange(MapType mapTypeId, MapMode mapMode) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onMapModeChange(mapTypeId, mapMode);
                }
            });
        }
    }

    @Override
    public void isEnterPreview(MapType mapTypeId, boolean isEnterPreview) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.isEnterPreview(mapTypeId, isEnterPreview);
                }
            });
        }
    }

    @Override
    public void onEGLScreenshot(MapType mapTypeId, byte[] bytes, MapScreenShotDataInfo info) {
        if (callbacks.containsKey(mapTypeId)) {
            Logger.d(TAG, mapTypeId, "==onEGLScreenshot", bytes.length);
            callbacks.get(mapTypeId).forEach(new Consumer<IMapAdapterCallback>() {
                @Override
                public void accept(IMapAdapterCallback callback) {
                    callback.onEGLScreenshot(mapTypeId, bytes, info);
                }
            });
        }
    }

}
