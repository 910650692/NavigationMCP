package com.fy.navi.service.adapter.layer.bls.refix.style;

import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.adapter.layer.bls.refix.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.refix.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public class SearchLayerStyleAdapter implements BaseStyleAdapter {

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()){
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
