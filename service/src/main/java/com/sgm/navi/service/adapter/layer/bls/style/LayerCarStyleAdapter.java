package com.sgm.navi.service.adapter.layer.bls.style;


import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizCarControl;
import com.autonavi.gbl.layer.model.BizCarType;
import com.autonavi.gbl.layer.model.PrepareLayerCarParam;
import com.autonavi.gbl.layer.model.PrepareLayerMarkerParam;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.autonavi.gbl.map.layer.model.LayerItemType;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

public class LayerCarStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_LAYER_CAR_2d = "car_logo_2d";//2d车标正常样式
    private static final String KEY_LAYER_CAR_SKELETON = "car_logo_skeleton";//骨骼车标正常样式
    private static final String KEY_LAYER_CAR_SPEED = "car_logo_speed";//车速车标
    private static final String KEY_LAYER_CAR_SPEED_POINT = "car_logo_speed_point";//车速车标

    private BizCarControl bizCarControl;

    private AtomicInteger speeds = new AtomicInteger(0);

    public LayerCarStyleAdapter(int engineID, BizCarControl bizCarControl) {
        super(engineID);
        this.bizCarControl = bizCarControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizCarType.BizCarTypeInvalid:
            case BizCarType.BizCarTypeGuide:
            case BizCarType.BizCarTypeCruise:
            case BizCarType.BizCarTypeSearch:
            case BizCarType.BizCarTypeRoute: {
                switch (bizCarControl.getCarMode()) {
                    case CarMode.CarModeSpeed -> {
                        if (item.getItemType() == LayerItemType.LayerItemPointType) {
                            Logger.d(TAG, "返回车速点样式");
                            return KEY_LAYER_CAR_SPEED_POINT;
                        }
                        if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                            Logger.d(TAG, "返回车速");
                            return KEY_LAYER_CAR_SPEED;
                        }
                    }
                    case CarMode.CarMode2D -> {
                        if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                            Logger.d(TAG, "默认车标");
                            return KEY_LAYER_CAR_2d;
                        }
                    }
                    case CarMode.CarModeSkeleton -> {
                        if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                            Logger.d(TAG, "骨骼车标");
                            return KEY_LAYER_CAR_SKELETON;
                        }
                    }
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    public void updateCarSpeed(int speed) {
        if (speed >= 0 && speed != speeds.get()) {
            speeds.set(speed);
        }
    }

    @Override
    public List<CustomUpdatePair> updateTextureUpdatePair(LayerItem item) {
        List<CustomUpdatePair> customUpdatePairs = new ArrayList<>();
        switch (item.getBusinessType()) {
            case BizCarType.BizCarTypeInvalid:
            case BizCarType.BizCarTypeGuide:
            case BizCarType.BizCarTypeCruise:
            case BizCarType.BizCarTypeSearch:
            case BizCarType.BizCarTypeRoute: {
                customUpdatePairs.add(createUpdateValuePair("speed_text_two", String.valueOf(speeds.get())));
                break;
            }
        }
        return customUpdatePairs;
    }

    @Override
    public boolean getPrepareLayerParam(LayerItem item, PrepareLayerMarkerParam param) {
        if (param instanceof PrepareLayerCarParam) {
            PrepareLayerCarParam carParam = (PrepareLayerCarParam) param;
            carParam.speed = speeds.get();
            //超速判断  TODO
//            carParam.isOverSpeed = true;
        }
        return super.getPrepareLayerParam(item, param);
    }
}
