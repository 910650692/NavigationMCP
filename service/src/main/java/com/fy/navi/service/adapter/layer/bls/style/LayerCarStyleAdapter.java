package com.fy.navi.service.adapter.layer.bls.style;

import android.util.Log;
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

import java.util.concurrent.atomic.AtomicReference;

public class LayerCarStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_LAYER_CAR_DEFAULT = "car_logo_base";
    private static final String KEY_LAYER_CAR_OTHER = "car_logo_other";
    private static final String KEY_LAYER_CAR_SPEED = "car_logo_speed";
    private static final String KEY_LAYER_CAR_SPEED_ARROW = "car_logo_speed_arrow";

    private BizCarControl bizCarControl;

    private final AtomicReference<LayerItemCarSpeedData> currentCarSpeed = new AtomicReference(new LayerItemCarSpeedData(0));

    public LayerCarStyleAdapter(int engineID, BizCarControl bizCarControl) {
        super(engineID);
        this.bizCarControl = bizCarControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        if (bizCarControl == null) {
            Logger.d(TAG, "bizCarControl == null");
            return KEY_LAYER_CAR_DEFAULT;
        }
        switch (bizCarControl.getCarMode()) {
            case CarMode.CarModeSpeed -> {
                if(item instanceof SpeedCarLayerItem){
                    Logger.d("ForTest", "返回车速车标");
                    return KEY_LAYER_CAR_SPEED;
                }
                if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    Logger.d("ForTest", "返回罗盘和箭头");
                    return KEY_LAYER_CAR_SPEED_ARROW;
                }
            }
            case CarMode.CarMode2D -> {
                if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    Logger.d(TAG, "默认车标");
                    return KEY_LAYER_CAR_DEFAULT;
                }
            }
            case CarMode.CarModeSkeleton -> {
                if (item.getItemType() == LayerItemType.LayerItemNaviCarType) {
                    Logger.d(TAG, "骨骼车标");
                    return KEY_LAYER_CAR_OTHER;
                }
            }
        }
        return KEY_LAYER_CAR_DEFAULT;
    }

    public void updateCarSpeed(int speed) {
        speed = (speed <= 0 ? 0 : speed);
        if ((currentCarSpeed.get().getSpeed() != speed) && (bizCarControl != null) && bizCarControl.getCarMode() == CarMode.CarModeSpeed) {
            Logger.d(TAG, "改变车速 =" + speed);
            currentCarSpeed.get().setSpeed(speed);
            bizCarControl.updateStyle();
        }
    }


    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        if (item instanceof SpeedCarLayerItem) {
            return currentCarSpeed.get();
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        if (item instanceof SpeedCarLayerItem) {
            return new IUpdateBitmapViewProcessor<LayerItemCarSpeedData>() {
                @Override
                public void onNormalProcess(View rootView, LayerItemCarSpeedData data) {
                    Logger.d(TAG, "更新车速" + data.getSpeed());
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
