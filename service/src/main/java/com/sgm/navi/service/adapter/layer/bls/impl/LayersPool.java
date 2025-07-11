package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.engine.EngineAdapter;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.define.layer.refix.LayerType;
import com.sgm.navi.service.define.map.MapType;

import java.util.HashMap;

public final class LayersPool {

    private final static String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private HashMap<LayerType, BaseLayerImpl> allLayers = new HashMap<>();

    private BizControlService bizControlService;

    private MapView mapView;

    private Context context;

    private MapType mapType;

    public LayersPool(BizControlService bizControlService, MapView mapView, Context context, MapType mapTypeId) {
        this.bizControlService = bizControlService;
        this.mapView = mapView;
        this.context = context;
        this.mapType = mapTypeId;

        String styleBlPath = EngineAdapter.getInstance().styleBlPath(mapTypeId);
        int engineId = EngineAdapter.getInstance().engineID(mapTypeId);
//        int eagleEyeEngineId = EngineAdapter.getInstance().eagleEyeEngineID(mapTypeId);

        Logger.d(TAG, mapType,  " 初始化 styleBlPath :", styleBlPath);
        boolean result = bizControlService.init(engineId, styleBlPath);
        Logger.d(TAG, "初始化 engineId :", engineId, " ;result :" + result);
//        result = bizControlService.init(eagleEyeEngineId, styleBlPath);
//        Logger.d(TAG, "初始化 eagleEyeEngineId :" + eagleEyeEngineId + " ;result :" + result);

        result = bizControlService.initCollisionConfig(mapView, styleBlPath);
        Logger.d(TAG, "初始化 initCollisionConfig result :", result);
        result = bizControlService.isInit() == ServiceInitStatus.ServiceInitDone;
        Logger.d(TAG, "初始化 initLayerServiceResult :", result);

        allLayers.put(LayerType.LAYER_AREA, new LayerAreaImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_CAR, new LayerCarImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_FLY_LINE, new LayerFlyLineImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_GUIDE_ROUTE, new LayerGuideRouteImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_SEARCH, new LayerSearchImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_USER, new LayerUserImpl(bizControlService, mapView, context, mapTypeId));
        allLayers.put(LayerType.LAYER_LABEL, new LayerLabelImpl(bizControlService, mapView, context, mapTypeId));
    }

    public LayerAreaImpl getLayerArea() {
        return (LayerAreaImpl) getLayer(LayerType.LAYER_AREA);
    }

    public LayerCarImpl getLayerCar() {
        return (LayerCarImpl) getLayer(LayerType.LAYER_CAR);
    }

    public LayerFlyLineImpl getLayerFlyLine() {
        return (LayerFlyLineImpl) getLayer(LayerType.LAYER_FLY_LINE);
    }

    public LayerGuideRouteImpl getLayerGuideRoute() {
        return (LayerGuideRouteImpl) getLayer(LayerType.LAYER_GUIDE_ROUTE);
    }

    public LayerLabelImpl getLayerLabel() {
        return (LayerLabelImpl) getLayer(LayerType.LAYER_LABEL);
    }

    public LayerSearchImpl getLayerSearch() {
        return (LayerSearchImpl) getLayer(LayerType.LAYER_SEARCH);
    }

    public LayerUserImpl getLayerUser() {
        return (LayerUserImpl) getLayer(LayerType.LAYER_USER);
    }


    public BaseLayerImpl getLayer(LayerType layerId) {
        BaseLayerImpl layer = allLayers.get(layerId);
        if (layer == null) {
            switch (layerId) {
                case LAYER_AREA ->
                        layer = new LayerAreaImpl(bizControlService, mapView, context, mapType);
                case LAYER_CAR ->
                        layer = new LayerCarImpl(bizControlService, mapView, context, mapType);
                case LAYER_FLY_LINE ->
                        layer = new LayerFlyLineImpl(bizControlService, mapView, context, mapType);
                case LAYER_GUIDE_ROUTE ->
                        layer = new LayerGuideRouteImpl(bizControlService, mapView, context, mapType);
                case LAYER_SEARCH ->
                        layer = new LayerSearchImpl(bizControlService, mapView, context, mapType);
                case LAYER_USER ->
                        layer = new LayerUserImpl(bizControlService, mapView, context, mapType);
                case LAYER_LABEL ->
                        layer = new LayerLabelImpl(bizControlService, mapView, context, mapType);
                default -> layer = new BaseLayerImpl(bizControlService, mapView, context, mapType);
            }
            allLayers.put(layerId, layer);
        }
        return layer;
    }

    public void addLayerClickCallback(ILayerAdapterCallBack observer) {
        for (BaseLayerImpl layer : allLayers.values()) {
            layer.setCallBack(observer);
        }
    }

    public void removeClickCallback() {
        for (BaseLayerImpl layer : allLayers.values()) {
            layer.removeCallback();
        }
    }
}
