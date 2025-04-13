package com.fy.navi.service.adapter.layer.bls.style;

import android.view.View;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.PopPointLayerItem;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;

public class LayerLabelStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_LABEL_END_PARK = "label_end_park";

    public LayerLabelStyleAdapter(int engineID, BizLabelControl bizLabelControl) {
        super(engineID);
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson BusinessType" + item.getBusinessType());
        switch (item.getBusinessType()) {
            case BizLabelType.BizLabelTypeRoutePopSearchPoint -> {
                Logger.d(TAG,"终点停车场图层");
                if (item instanceof PopPointLayerItem) {
                    PopPointLayerItem popPointLayerItem = (PopPointLayerItem) item;
                    String mText = popPointLayerItem.getMText();
                    Logger.d(TAG, "provideLayerItemStyleJson mText " + mText);
                }
                return KEY_LABEL_END_PARK;
            }
        }
        return super.provideLayerItemStyleJson(item);
    }
}
