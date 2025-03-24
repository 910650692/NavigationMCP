package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;

import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.map.MapView;

public class LayerCustom extends BaseLayerImpl {

    public LayerCustom(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerCustomControl().setStyle(this);
        getLayerCustomControl().addClickObserver(this);
        getLayerCustomControl().addFocusChangeObserver(this);
    }

}
