package com.sgm.navi.service.adapter.layer.bls.style;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizFlyLineControl;
import com.autonavi.gbl.layer.model.BizFlyLineType;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.layer.LayerItem;

public class LayerFlyLineStyleAdapter extends BaseStyleAdapter {

    private BizFlyLineControl bizFlyLineControl;
    private final static String FLY_LINE_POINT_MOVE = "fly_line_point_move";

    public LayerFlyLineStyleAdapter(int engineID, BizFlyLineControl bizFlyLineControl) {
        super(engineID);
        this.bizFlyLineControl = bizFlyLineControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizFlyLineType.BizFlyLineTypePoint -> {
                if (bizFlyLineControl != null) {
                    if (bizFlyLineControl.getDrawMode() == FlylineDrawMode.FLYLINE_MOVE_END) {
                        Logger.d(TAG, "飞线移动点");
                        return FLY_LINE_POINT_MOVE;
                    }
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }
}
