package com.sgm.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.BizAreaControl;
import com.autonavi.gbl.layer.model.BizAreaType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;

import java.util.ArrayList;
import java.util.List;

public class LayerAreaStyleAdapter extends BaseStyleAdapter {

    public LayerAreaStyleAdapter(int engineID, BizAreaControl bizAreaControl) {
        super(engineID);
    }

    @Override
    public List<CustomUpdatePair> updateTextureUpdatePair(LayerItem item, boolean isNightMode) {
        List<CustomUpdatePair> customUpdatePairs = new ArrayList<>();
        if (item.getBusinessType() == BizAreaType.BizAreaTypeEndAreaParentPoint) {
            customUpdatePairs.add(createUpdateStylePair("end_area_all", "display:none;"));
            customUpdatePairs.add(createUpdateStylePair("parent_text","font-size:20px;"));
        }
        return customUpdatePairs;
    }
}
