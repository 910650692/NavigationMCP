package com.fy.navi.service.adapter.layer.bls.impl;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.map.bls.MapViewPoolManager;
import com.fy.navi.service.define.map.MapType;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Hashtable;
import java.util.List;

public class LayersPoolManager {

    private String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private final HashMap<MapType, LayersPool> layersPools = new HashMap<>();

//    private final Hashtable<MapType, List<ILayerAdapterCallBack>> mLayerPackageCallBacks = new Hashtable<>();

    private BizControlService bizControlService;
    private MapService mapService;

    public void addLayerClickCallback(MapType mapTypeId, ILayerAdapterCallBack observer) {
//        if (!mLayerPackageCallBacks.containsKey(mapTypeId)) {
//            mLayerPackageCallBacks.put(mapTypeId, new ArrayList<>());
//        }
//        if (!mLayerPackageCallBacks.get(mapTypeId).contains(observer)) {
//            Logger.d(TAG, mapTypeId + " 注册回调 ：" + observer.getClass().getSimpleName() + ";size =" + mLayerPackageCallBacks.get(mapTypeId).size());
//            get(mapTypeId).addLayerClickCallback(observer);
//            mLayerPackageCallBacks.get(mapTypeId).add(observer);
//        }
        if (layersPools.containsKey(mapTypeId)) {
            layersPools.get(mapTypeId).addLayerClickCallback(observer);
        }
    }

    public void removeClickCallback(MapType mapTypeId, ILayerAdapterCallBack observer) {
//        if (!mLayerPackageCallBacks.containsKey(mapTypeId)) {
//            mLayerPackageCallBacks.put(mapTypeId, new ArrayList<>());
//        }
//        if (mLayerPackageCallBacks.get(mapTypeId).contains(observer)) {
//            Logger.d(TAG, mapTypeId + " 移除回调 ：" + observer.getClass().getSimpleName() + ";size =" + mLayerPackageCallBacks.get(mapTypeId).size());
//            get(mapTypeId).removeClickCallback(observer);
//            mLayerPackageCallBacks.get(mapTypeId).remove(observer);
//        }
        if (layersPools.containsKey(mapTypeId)) {
            layersPools.get(mapTypeId).removeClickCallback(observer);
        }
    }

    private static final class Holder {
        private static final LayersPoolManager INSTANCE = new LayersPoolManager();
    }

    private LayersPoolManager() {

    }

    public static LayersPoolManager getInstance() {
        return LayersPoolManager.Holder.INSTANCE;
    }

    public BizControlService getBizControlService() {
        if (bizControlService == null) {
            bizControlService = (BizControlService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.BizControlSingleServiceID);
        }
        return bizControlService;
    }

    public boolean initLayerService(MapType mapTypeId) {
        if (!layersPools.containsKey(mapTypeId)) {
            Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "初始化图层服务");
            createLayerPool(mapTypeId);
        }
        return true;
    }

    public void removeLayerService(MapType mapTypeId) {
        if (ConvertUtils.isEmpty(layersPools)) return;
        ConvertUtils.remove(layersPools, mapTypeId);
    }

    public void unInitLayerService() {
        bizControlService.unInit();
        layersPools.clear();
    }

    public LayersPool get(MapType mapTypeId) {
        if (!layersPools.containsKey(mapTypeId)) {
            Logger.d(TAG, "lvv", "222");
            createLayerPool(mapTypeId);
        }
        return layersPools.get(mapTypeId);
    }

    private void createLayerPool(MapType mapTypeId) {
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, mapTypeId + " 创建图层池");
//        MapView mapView = MapViewPoolManager.getInstance().get(mapTypeId).getMapview();
        //TODO 优化获取底图的方案
        MapView mapView = getMapview(mapTypeId);
        LayersPool layersPool = new LayersPool(getBizControlService(), mapView, AppCache.getInstance().getMContext(), mapTypeId);
        layersPools.put(mapTypeId, layersPool);
    }

    private MapView getMapview(MapType mapTypeId) {
        if (mapService == null) {
            mapService = (MapService) ServiceMgr.getServiceMgrInstance().getBLService(SingleServiceID.MapSingleServiceID);
        }
        return mapService.getMapView(EngineAdapter.getInstance().engineID(mapTypeId));
    }
}
