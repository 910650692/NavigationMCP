package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.util.model.ServiceInitStatus;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.engine.EngineAdapter;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.map.MapType;

import java.util.HashMap;

public class LayersPool {

    private final static String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private HashMap<LayerType, BaseLayerImpl> allLayers = new HashMap<>();

    private BizControlService bizControlService;

    private MapView mapView;

    private Context context;

    public LayersPool(BizControlService bizControlService, MapView mapView, Context context, MapType mapTypeId) {
        this.bizControlService = bizControlService;
        this.mapView = mapView;
        this.context = context;
        String styleBlPath = EngineAdapter.getInstance().styleBlPath(mapTypeId);
        int engineId = EngineAdapter.getInstance().engineID(mapTypeId);
        int eagleEyeEngineId = EngineAdapter.getInstance().eagleEyeEngineID(mapTypeId);
        boolean result = bizControlService.init(engineId, styleBlPath);
        Logger.d(TAG, "engineId :" + engineId + " ;result :" + result);
        result = bizControlService.init(eagleEyeEngineId, styleBlPath);
        Logger.d(TAG, "eagleEyeEngineId :" + eagleEyeEngineId + " ;result :" + result);
        result = bizControlService.isInit() == ServiceInitStatus.ServiceInitDone;
        Logger.d(TAG, "initLayerServiceresult :" + result);
    }

    public LayerAreaImpl getLayerArea() {
        return (LayerAreaImpl) getLayer(LayerType.LAYER_AREA);
    }

    public LayerCarImpl getLayerCar() {
        return (LayerCarImpl) getLayer(LayerType.LAYER_CAR);
    }

    public LayerCustomImpl getLayerCustom() {
        return (LayerCustomImpl) getLayer(LayerType.LAYER_CUSTOM);
    }

    public LayerEagleEye getLayerEagleEye() {
        return (LayerEagleEye) getLayer(LayerType.LAYER_EAGLE_EYE);
    }

    public LayerFlyLineImpl getLayerFlyLine() {
        return (LayerFlyLineImpl) getLayer(LayerType.LAYER_FLY_LINE);
    }

    public LayerGuideRouteImpl getLayerGuideRoute() {
        return (LayerGuideRouteImpl) getLayer(LayerType.LAYER_GUIDE_ROUTE);
    }

    public LayerSearchImpl getLayerSearch() {
        return (LayerSearchImpl) getLayer(LayerType.LAYER_SEARCH);
    }

    public LayerUserImpl getLayerUser() {
        return (LayerUserImpl) getLayer(LayerType.LAYER_USER);
    }

    public LayerCrossImpl getLayerCross() {
        return (LayerCrossImpl) getLayer(LayerType.LAYER_CROSS);
    }

    public BaseLayerImpl getLayer(LayerType layerId) {
        BaseLayerImpl layer = allLayers.get(layerId);
        if (layer == null) {
            switch (layerId) {
                case LAYER_AREA -> layer = new LayerAreaImpl(bizControlService, mapView, context);
                case LAYER_CAR -> layer = new LayerCarImpl(bizControlService, mapView, context);
                case LAYER_CUSTOM ->
                        layer = new LayerCustomImpl(bizControlService, mapView, context);
                case LAYER_EAGLE_EYE ->
                        layer = new LayerEagleEye(bizControlService, mapView, context);
                case LAYER_FLY_LINE ->
                        layer = new LayerFlyLineImpl(bizControlService, mapView, context);
                case LAYER_GUIDE_ROUTE ->
                        layer = new LayerGuideRouteImpl(bizControlService, mapView, context);
                case LAYER_SEARCH ->
                        layer = new LayerSearchImpl(bizControlService, mapView, context);
                case LAYER_USER ->
                        layer = new LayerUserImpl(bizControlService, mapView, context);
                case LAYER_CROSS ->
                        layer = new LayerCrossImpl(bizControlService, mapView, context);
                default -> layer = new BaseLayerImpl(bizControlService, mapView, context);
            }
            allLayers.put(layerId, layer);
        }
        return layer;
    }
}
