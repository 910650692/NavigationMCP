package com.fy.navi.service.adapter.layer.bls.style;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;

public class LayerSearchStyleAdapter extends BaseStyleAdapter {

    public LayerSearchStyleAdapter(int engineID, BizSearchControl bizSearchControl) {
        super(engineID);
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson getBusinessType:" + item.getBusinessType());
        int businessType = item.getBusinessType();
        switch (businessType) {
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                if (item instanceof SearchParentLayerItem parentLayerItem) {
                    int poiType = parentLayerItem.getPoiType();
                    switch (poiType) {
                        case LayerSearchPOIType.SEARCH_PARK -> {
                            //替换停车场json  当前默认json仅支持终点推荐停车场(最多3个扎标)
                            return super.provideLayerItemStyleJson(item);
                        }
                    }
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }
}
