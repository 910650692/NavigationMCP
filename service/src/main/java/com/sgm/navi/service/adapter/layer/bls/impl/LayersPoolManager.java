package com.sgm.navi.service.adapter.layer.bls.impl;


import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.map.bls.MapViewPoolManager;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.PoiInfoEntity;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Consumer;

public final class LayersPoolManager implements ILayerAdapterCallBack {

    private String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private final HashMap<MapType, LayersPool> layersPools = new HashMap<>();

    private final HashMap<MapType, List<ILayerAdapterCallBack>> callbacks = new HashMap<>();

    private AtomicReference<BizControlService> bizControlService = new AtomicReference<>();

    private static final class Holder {
        private static final LayersPoolManager INSTANCE = new LayersPoolManager();
    }

    private LayersPoolManager() {

    }

    public static LayersPoolManager getInstance() {
        return LayersPoolManager.Holder.INSTANCE;
    }

    public boolean initLayerService() {
        if (bizControlService.get() == null) {
            bizControlService.set((BizControlService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BizControlSingleServiceID));
        }
        return true;
    }

    public boolean initLayer(MapType mapType) {
        if (!layersPools.containsKey(mapType)) {
            createLayerPool(mapType);
        }
        return layersPools.containsKey(mapType);
    }

    public boolean unInitLayer(MapType mapType) {
        layersPools.get(mapType).removeClickCallback();
        layersPools.remove(mapType);
        return false;
    }


    public void unInitLayerService() {
        layersPools.clear();
        bizControlService.get().unInit();
    }

    public BizControlService getBizControlService() {
        if (bizControlService.get() == null) {
            initLayerService();
        }
        return bizControlService.get();
    }

    public void addLayerClickCallback(MapType mapTypeId, ILayerAdapterCallBack observer) {
        if (!callbacks.containsKey(mapTypeId)) {
            callbacks.put(mapTypeId, new ArrayList<>());
        }
        if (!callbacks.get(mapTypeId).contains(observer)) {
            Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId + " 注册回调 ：" + observer.getClass().getSimpleName() + ";size =" + callbacks.get(mapTypeId).size());
            callbacks.get(mapTypeId).add(observer);
        }
    }

    public void removeClickCallback(MapType mapTypeId, ILayerAdapterCallBack observer) {
        if (callbacks.get(mapTypeId).contains(observer)) {
            Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId + " 移除回调 ：" + observer.getClass().getSimpleName() + ";size =" + callbacks.get(mapTypeId).size());
            callbacks.get(mapTypeId).remove(observer);
        }
    }


    public LayersPool getLayersPool(MapType mapTypeId) {
        if (!layersPools.containsKey(mapTypeId)) {
            Logger.e(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId, "获取图层失败 创建图层 ");
            createLayerPool(mapTypeId);
        }
        return layersPools.get(mapTypeId);
    }

    private void createLayerPool(MapType mapTypeId) {
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId, " 创建图层池");
        MapView mapView = MapViewPoolManager.getInstance().getMapViewImpl(mapTypeId).getMapview();
        if (mapView != null) {
            LayersPool layersPool = new LayersPool(getBizControlService(), mapView, AppCache.getInstance().getMContext(), mapTypeId);
            layersPools.put(mapTypeId, layersPool);
            layersPool.addLayerClickCallback(this);
        } else {
            Logger.e(MapDefaultFinalTag.INIT_SERVICE_TAG, "创建图层池失败 底图还没初始化  ", mapTypeId);
        }
    }

    @Override
    public void onSearchItemClick(MapType mapTypeId, LayerPointItemType type, int index) {
        if (callbacks.containsKey(mapTypeId)) {
            callbacks.get(mapTypeId).forEach(new Consumer<ILayerAdapterCallBack>() {
                @Override
                public void accept(ILayerAdapterCallBack callBack) {
                    Logger.e(TAG, "onSearchItemClick");
                    callBack.onSearchItemClick(mapTypeId, type, index);
                }
            });
        }
    }

    @Override
    public void onRouteItemClick(MapType mapType, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        if (callbacks.containsKey(mapType)) {
            callbacks.get(mapType).forEach(new Consumer<ILayerAdapterCallBack>() {
                @Override
                public void accept(ILayerAdapterCallBack callBack) {
                    Logger.e(TAG, "onRouteItemClick");
                    callBack.onRouteItemClick(mapType, type, result);
                }
            });
        }
    }

    @Override
    public void onFavoriteClick(MapType mapType, PoiInfoEntity poiInfo) {
        if (callbacks.containsKey(mapType)) {
            callbacks.get(mapType).forEach(new Consumer<ILayerAdapterCallBack>() {
                @Override
                public void accept(ILayerAdapterCallBack callBack) {
                    Logger.e(TAG, "onFavoriteClick");
                    callBack.onFavoriteClick(mapType, poiInfo);
                }
            });
        }
    }

    @Override
    public void onFlyLineMoveEnd(MapType mapType, GeoPoint descPoint) {
        if (callbacks.containsKey(mapType)) {
            callbacks.get(mapType).forEach(new Consumer<ILayerAdapterCallBack>() {
                @Override
                public void accept(ILayerAdapterCallBack callBack) {
                    Logger.e(TAG, "onFlyLineMoveEnd");
                    callBack.onFlyLineMoveEnd(mapType, descPoint);
                }
            });
        }
    }

    /**
     * 点击自车位
     *
     * @param mapType
     * @param geoPoint
     */
    public void onCarClick(MapType mapType, GeoPoint geoPoint) {
        if (callbacks.containsKey(mapType)) {
            callbacks.get(mapType).forEach(new Consumer<ILayerAdapterCallBack>() {
                @Override
                public void accept(ILayerAdapterCallBack callBack) {
                    Logger.e(TAG, "onCarClick");
                    callBack.onCarClick(mapType, geoPoint);
                }
            });
        }
    }
}
