package com.sgm.navi.service.adapter.layer.bls.style;


import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.LayerScale;

public class LayerLabelStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_LABEL_END_PARK = "label_end_park";

    public LayerLabelStyleAdapter(int engineID, BizLabelControl bizLabelControl) {
        super(engineID);
    }

    @Override
    public String provideLayerItemStyleJson(BaseLayer layer, LayerItem item) {
        switch (item.getBusinessType()) {
            case BizLabelType.BizLabelTypeRoutePopSearchPoint -> {
                Logger.d(TAG,"终点停车场图层使用自定义扎标");
                // 设置显示比例尺范围5m~2km
                layer.setDisplayScale(new LayerScale(12.0f, 22.0f));
                return KEY_LABEL_END_PARK;
            }
        }
        return super.provideLayerItemStyleJson(layer, item);
    }
}
