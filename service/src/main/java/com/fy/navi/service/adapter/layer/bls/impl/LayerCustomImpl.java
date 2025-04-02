package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;

public class LayerCustomImpl extends BaseLayerImpl {

    public LayerCustomImpl(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerCustomControl().setStyle(this);
        getLayerCustomControl().addClickObserver(this);
        getLayerCustomControl().addFocusChangeObserver(this);
    }

}
