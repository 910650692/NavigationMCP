package com.fy.navi.service.adapter.layer.bls.impl;

import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public interface BaseStyleAdapter {

    String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    String provideLayerItemStyleJson(LayerItem item);

    LayerItemBase provideLayerItemDataProcessor(LayerItem item);

    ILayerItemProcessor provideLayerItemProcessor(LayerItem item);

}
