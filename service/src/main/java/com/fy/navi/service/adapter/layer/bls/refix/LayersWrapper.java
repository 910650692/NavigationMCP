package com.fy.navi.service.adapter.layer.bls.refix;


import android.content.Context;

import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.define.layer.LayerTypes;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.HashMap;

public class LayersWrapper {
    private HashMap<LayerTypes, BaseLayerImpl> allLayers = new HashMap<>();

    private BizControlService bizControlService;
    private MapView mapView;
    private Context context;
    private MapTypeId mapTypeId;

    public LayersWrapper(BizControlService bizControlService, MapView mapView, Context context) {
        this.bizControlService = bizControlService;
        this.mapView = mapView;
        this.context = context;
        this.mapTypeId = mapTypeId;
        allLayers.put(LayerTypes.LAYER_AREA, new LayerArea(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_CAR, new LayerCar(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_CUSTOM, new LayerCustom(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_EAGLE_EYE, new LayerEagleEye(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_FLY_LINE, new LayerFlyLine(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_GUIDE_ROUTE, new LayerGuideRoute(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_SEARCH, new LayerSearch(bizControlService, mapView, context));
        allLayers.put(LayerTypes.LAYER_USER, new LayerUser(bizControlService, mapView, context));
    }

    public LayerArea getLayerArea() {
        return (LayerArea) getLayer(LayerTypes.LAYER_AREA);
    }

    public LayerCar getLayerCar() {
        return (LayerCar) getLayer(LayerTypes.LAYER_CAR);
    }

    public LayerCustom getLayerCustom() {
        return (LayerCustom) getLayer(LayerTypes.LAYER_CUSTOM);
    }

    public LayerEagleEye getLayerEagleEye() {
        return (LayerEagleEye) getLayer(LayerTypes.LAYER_EAGLE_EYE);
    }

    public LayerFlyLine getLayerFlyLine() {
        return (LayerFlyLine) getLayer(LayerTypes.LAYER_FLY_LINE);
    }

    public LayerGuideRoute getLayerGuideRoute() {
        return (LayerGuideRoute) getLayer(LayerTypes.LAYER_GUIDE_ROUTE);
    }

    public LayerSearch getLayerSearch() {
        return (LayerSearch) getLayer(LayerTypes.LAYER_SEARCH);
    }

    public LayerUser getLayerUser() {
        return (LayerUser) getLayer(LayerTypes.LAYER_USER);
    }


    public BaseLayerImpl getLayer(LayerTypes layerId) {
        BaseLayerImpl layer = allLayers.get(layerId);
        if (layer == null) {
            switch (layerId) {
                case LAYER_AREA -> layer = new LayerArea(bizControlService, mapView, context);
                case LAYER_CAR -> layer = new LayerCar(bizControlService, mapView, context);
                case LAYER_CUSTOM -> layer = new LayerCustom(bizControlService, mapView, context);
                case LAYER_EAGLE_EYE -> layer = new LayerEagleEye(bizControlService, mapView, context);
                case LAYER_FLY_LINE -> layer = new LayerFlyLine(bizControlService, mapView, context);
                case LAYER_GUIDE_ROUTE -> layer = new LayerGuideRoute(bizControlService, mapView, context);
                case LAYER_SEARCH -> layer = new LayerSearch(bizControlService, mapView, context);
                case LAYER_USER -> layer = new LayerUser(bizControlService, mapView, context);
                default -> layer = new BaseLayerImpl(bizControlService, mapView, context);
            }
            allLayers.put(layerId, layer);
        }
        return layer;
    }

}
