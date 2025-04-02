package com.fy.navi.service.adapter.layer.bls.style;

import com.autonavi.gbl.layer.BizFlyLineControl;
import com.autonavi.gbl.layer.FlyLineTypePointLayerItem;
import com.autonavi.gbl.layer.model.BizFlyLineType;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.adapter.layer.bls.impl.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.impl.ILayerItemProcessor;
import com.fy.navi.service.define.layer.refix.LayerItemBase;

public class FlyLineEndPointStyleAdapter implements BaseStyleAdapter {

    public void setVisible(boolean visible) {
        isVisible = visible;
    }

    private boolean isVisible = false;
    private static final String KEY_LAYER_FLY_LINE_END_POINT_MOVE = "fly_line_end_point_layer_move";
    private static final String KEY_LAYER_FLY_LINE_END_POINT_SELECT = "fly_line_end_point_layer_select";
    private static final String KEY_LAYER_FLY_LINE_END_POINT_NONE = "fly_line_end_point_layer_none";
    private static final String KEY_LAYER_FLY_LINE = "fly_line_layer";

    private final BizFlyLineControl control;

    public interface onFlayLineDrawModeChange {
        void onSelectEnd(FlyLineTypePointLayerItem item);
    }

    public FlyLineEndPointStyleAdapter(BizFlyLineControl layerFlyLineControl) {
        this.control = layerFlyLineControl;
    }


    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizFlyLineType.BizFlyLineTypeLine -> {
                return KEY_LAYER_FLY_LINE;
            }
            case BizFlyLineType.BizFlyLineTypePoint -> {
                if(!isVisible){
                    return KEY_LAYER_FLY_LINE_END_POINT_NONE;
                }
                switch (control.getDrawMode()) {
                    case FlylineDrawMode.FLYLINE_MOVE_END:
                        return KEY_LAYER_FLY_LINE_END_POINT_MOVE;
                    case FlylineDrawMode.FLYLINE_SELECT_END:
                        return KEY_LAYER_FLY_LINE_END_POINT_SELECT;
                    case FlylineDrawMode.FLYLINE_NONE_END:
                        return KEY_LAYER_FLY_LINE_END_POINT_NONE;
                }
            }
        }
        return KEY_LAYER_FLY_LINE;
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
