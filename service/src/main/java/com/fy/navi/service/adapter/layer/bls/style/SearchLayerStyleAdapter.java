package com.fy.navi.service.adapter.layer.bls.style;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.layer.model.PoiParentType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public class SearchLayerStyleAdapter implements BaseStyleAdapter {

    private static final String KAY_LAYER_SEARCH_LINE = "search_layer_line";
    private static final String KAY_LAYER_SEARCH_PARENT = "search_layer_parent";
    private static final String KAY_LAYER_SEARCH_PARENT_GAS = "search_layer_parent_gas";
    private static final String KAY_LAYER_SEARCH_PARENT_GAS_DISCOUNTS = "search_layer_parent_gas_discounts";
    private static final String KAY_LAYER_SEARCH_PARENT_CAR_WASH = "search_layer_parent_car_wash";
    private static final String KAY_LAYER_SEARCH_PARENT_DELICIOUS_FOOD = "search_layer_parent_delicious_food";
    private static final String KAY_LAYER_SEARCH_PARENT_SCENIC_SPOT = "search_layer_parent_scenic_spot";
    private static final String KAY_LAYER_SEARCH_PARENT_CHARGE_STATION = "search_layer_parent_charge_station";
    private static final String KAY_LAYER_SEARCH_CHILD = "search_layer_child";
    private static final String KAY_LAYER_SEARCH_CENTRAL = "search_layer_central";
    private static final String KAY_LAYER_SEARCH_START_END = "search_layer_start_end";
    private static final String KAY_LAYER_SEARCH_PARK = "search_layer_park";
    private static final String KAY_LAYER_SEARCH_POI_LABEL = "search_layer_poi_label";
    private static final String KAY_LAYER_SEARCH_CHARGE = "search_layer_charge_station";

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson getBusinessType:" + item.getBusinessType());
        switch (item.getBusinessType()){
            case BizSearchType.BizSearchTypeLine -> {
                return KAY_LAYER_SEARCH_LINE;
            }
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                SearchParentLayerItem parentLayerItem = (SearchParentLayerItem) item;
                int poiType = parentLayerItem.getPoiType();
                Logger.d(TAG, "poiType:" + poiType);
                switch (poiType) {
                    case PoiParentType.PoiParentTypeGas -> {
                        Logger.d(TAG, "poiType:");
                        return KAY_LAYER_SEARCH_PARENT_GAS;
                    }
                    case PoiParentType.PoiParentTypeGasDiscounts -> {
                        return KAY_LAYER_SEARCH_PARENT_GAS_DISCOUNTS;
                    }
                    case PoiParentType.PoiParentTypeCarWash -> {
                        return KAY_LAYER_SEARCH_PARENT_CAR_WASH;
                    }
                    case PoiParentType.PoiParentTypeDeliciousFood -> {
                        return KAY_LAYER_SEARCH_PARENT_DELICIOUS_FOOD;
                    }
                    case PoiParentType.PoiParentTypeScenicSpot -> {
                        return KAY_LAYER_SEARCH_PARENT_SCENIC_SPOT;
                    }
                    case PoiParentType.PoiParentTypeChargeStation -> {
                        return KAY_LAYER_SEARCH_PARENT_CHARGE_STATION;
                    }
                    default -> {
                        return KAY_LAYER_SEARCH_PARENT;
                    }
                }
//                Logger.d(TAG, "parentLayerItem:" + parentLayerItem.toString());
//                return KAY_LAYER_SEARCH_PARENT;
            }
            case BizSearchType.BizSearchTypePoiChildPoint -> {
                return KAY_LAYER_SEARCH_CHILD;
            }
            case BizSearchType.BizSearchTypePoiCentralPos -> {
                return KAY_LAYER_SEARCH_CENTRAL;
            }
            case BizSearchType.BizSearchTypePoiBeginEnd -> {
                return KAY_LAYER_SEARCH_START_END;
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                return KAY_LAYER_SEARCH_PARK;
            }
            case BizSearchType.BizSearchTypeChargeStation -> {
                return KAY_LAYER_SEARCH_CHARGE;
            }
            case BizSearchType.BizSearchTypePoiLabel -> {
                return KAY_LAYER_SEARCH_POI_LABEL;
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
