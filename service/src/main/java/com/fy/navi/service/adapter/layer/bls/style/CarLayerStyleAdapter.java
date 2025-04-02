package com.fy.navi.service.adapter.layer.bls.style;

import android.view.View;

import com.android.utils.log.Logger;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.LayerItemType;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;
import com.fy.navi.service.define.layer.refix.LayerItemCar;

public class CarLayerStyleAdapter implements BaseStyleAdapter {

    private static final String KEY_LAYER_CAR_LOGO = "car_logo";
    private static final String KEY_LAYER_CAR_3D = "car_logo_3d";

    private LayerItemCar itemCar;

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getItemType()) {
            case LayerItemType.LayerItem3DModelType -> {
                return KEY_LAYER_CAR_3D;
            }
            default -> {
                return KEY_LAYER_CAR_LOGO;
            }
        }
    }

    public void updateLayerItemCar(LayerItemCar itemCar) {
        Logger.d(TAG,"updateLayerItemCar");
        this.itemCar = itemCar;
    }

    @Override
    public LayerItemBase provideLayerItemDataProcessor(LayerItem item) {
        Logger.d(TAG,"provideLayerItemDataProcessor");
        return itemCar;
    }

    @Override
    public ILayerItemProcessor provideLayerItemProcessor(LayerItem item) {
        return new ILayerItemProcessor<LayerItemCar>() {
            @Override
            public void onNormalProcess(View rootView, LayerItemCar data) {

            }

            @Override
            public void onFocusProcess(View rootView, LayerItemCar data) {

            }
        };
    }
}
