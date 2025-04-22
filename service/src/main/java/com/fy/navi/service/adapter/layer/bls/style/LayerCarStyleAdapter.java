package com.fy.navi.service.adapter.layer.bls.style;

import android.view.View;
import android.widget.TextView;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizCarControl;
import com.autonavi.gbl.layer.SpeedCarLayerItem;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.LayerItemType;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemCarSpeedData;
import com.fy.navi.service.define.layer.refix.LayerItemData;

public class LayerCarStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_LAYER_CAR_DEFAULE = "car_logo_base";
    private static final String KEY_LAYER_CAR_OTHER = "car_logo_other";
    private static final String KEY_LAYER_CAR_SPEED = "car_logo_speed";

    private BizCarControl bizCarControl;
    private LayerItemCarSpeedData currentCarSpeed = new LayerItemCarSpeedData(0);

    public LayerCarStyleAdapter(int engineID, BizCarControl bizCarControl) {
        super(engineID);
        this.bizCarControl = bizCarControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        if (bizCarControl == null) {
            return KEY_LAYER_CAR_DEFAULE;
        }
        switch (bizCarControl.getCarMode()) {
            case CarMode.CarModeSpeed -> {
                if (item.getItemType() == LayerItemType.LayerItemPointType) {
                    Logger.d(TAG, "车速车标");
                    return KEY_LAYER_CAR_SPEED;
                } else if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    return KEY_LAYER_CAR_OTHER;
                }
            }
            case CarMode.CarMode2D -> {
                if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    Logger.d(TAG, "默认车标");
                    return KEY_LAYER_CAR_DEFAULE;
                }
            }
            case CarMode.CarModeSkeleton -> {
                if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    Logger.d(TAG, "骨骼车标");
                    return KEY_LAYER_CAR_OTHER;
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    public void updateCarSpeed(int speed) {
        speed = (speed <= 0 ? 0 : speed);
        if ((currentCarSpeed.getSpeed() != speed) && (bizCarControl != null) && bizCarControl.getCarMode() == CarMode.CarModeSpeed) {
            Logger.d(TAG, "改变车速 =" + speed);
            currentCarSpeed.setSpeed(speed);
            bizCarControl.updateStyle();
        }
    }

    @Override
    public boolean isNeedReCreate(LayerItem item) {
        if ((bizCarControl != null) && bizCarControl.getCarMode() == CarMode.CarModeSpeed) {
            return true;
        }
        return super.isNeedReCreate(item);
    }

    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        if (item instanceof SpeedCarLayerItem) {
            return currentCarSpeed;
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        if (item instanceof SpeedCarLayerItem) {
            return new IUpdateBitmapViewProcessor<LayerItemCarSpeedData>() {
                @Override
                public void onNormalProcess(View rootView, LayerItemCarSpeedData data) {
                    Logger.d(TAG, "更新车速"+data.getSpeed());
                    if (rootView != null) {
                        TextView layerCarSpeed = rootView.findViewById(R.id.layer_car_speed);
                        layerCarSpeed.setText(String.valueOf(data.getSpeed()));
                    }
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }
}
