package com.fy.navi.service.adapter.layer.bls.style;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.layer.model.PoiParentType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public class RouteLayerStyleAdapter implements BaseStyleAdapter {

    private static final String KAY_LAYER_ROUTE_START = "route_layer_start";
    private static final String KAY_LAYER_ROUTE_END = "route_layer_end";

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson getBusinessType:" + item.getBusinessType());
        switch (item.getBusinessType()){
            case BizRouteType.BizRouteTypeStartPoint -> {
                return KAY_LAYER_ROUTE_START;
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                return KAY_LAYER_ROUTE_END;
            }
        }
        return null;
    }

    @Override
    public LayerItemBase provideLayerItemDataProcessor(LayerItem item) {
        return null;
    }

    @Override
    public ILayerItemProcessor provideLayerItemProcessor(LayerItem item) {
        return null;
    }
}
