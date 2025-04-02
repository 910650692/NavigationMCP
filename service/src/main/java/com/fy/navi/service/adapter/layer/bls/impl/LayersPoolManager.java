package com.fy.navi.service.adapter.layer.bls.impl;


import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.map.bls.MapViewPoolManager;
import com.fy.navi.service.define.map.MapType;

import java.util.HashMap;

public class LayersPoolManager {

    private HashMap<MapType, LayersPool> layersPools = new HashMap<>();

    private BizControlService bizControlService;

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
            createLayerPool(mapTypeId);
        }
        return true;
    }

    public void unInitLayerService() {

    }

    public LayersPool get(MapType mapTypeId) {
        if (!layersPools.containsKey(mapTypeId)) {
            createLayerPool(mapTypeId);
        }
        return layersPools.get(mapTypeId);
    }

    private void createLayerPool(MapType mapTypeId) {
        MapView mapView = MapViewPoolManager.getInstance().get(mapTypeId).getMapview();
        LayersPool layersPool = new LayersPool(getBizControlService(), mapView, AppContext.getInstance().getMContext(), mapTypeId);
        layersPools.put(mapTypeId, layersPool);
    }
}
