package com.fy.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.model.BizFlyLineType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public class FlyLineStyleAdapter implements BaseStyleAdapter {

    private String favorite_main_poi_layer = "fly_line_layer.json";

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        int businessType = item.getBusinessType();
                switch (businessType) {
                    case BizFlyLineType.BizFlyLineTypePoint: {
                        return favorite_main_poi_layer;
                    }

                    case BizFlyLineType.BizFlyLineTypeLine:{
                        return favorite_main_poi_layer;
                    }
                }
        return "";
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
