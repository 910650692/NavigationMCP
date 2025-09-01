package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;
import android.util.Log;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizFlyLineType;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.observer.IMapEventObserver;
import com.sgm.navi.service.adapter.layer.bls.style.LayerFlyLineStyleAdapter;
import com.sgm.navi.service.adapter.layer.bls.utils.DebounceHandler;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;

public class LayerFlyLineImpl extends BaseLayerImpl<LayerFlyLineStyleAdapter> implements IMapEventObserver {
    // 防抖动延迟时间，单位毫秒
    private static final long DEBOUNCE_DELAY = 100;
    private final DebounceHandler mDebounceHandler = new DebounceHandler(DEBOUNCE_DELAY);

    public LayerFlyLineImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        this.className = "LayerFlyLineImpl";
        getLayerFlyLineControl().updateDrawMode(FlylineDrawMode.FLYLINE_MOVE_END, true);
        getLayerFlyLineControl().setVisible(BizFlyLineType.BizFlyLineTypeLine, false);
        getLayerFlyLineControl().setVisible(BizFlyLineType.BizFlyLineTypePoint, false);
    }

    @Override
    protected LayerFlyLineStyleAdapter createStyleAdapter() {
        return new LayerFlyLineStyleAdapter(getEngineId(), getLayerFlyLineControl());
    }

    /* 设置飞线显隐 */
    public void openFlyLine(boolean bShow) {
        if (bShow) {
            getLayerFlyLineControl().setStyle(this);
            getMapView().addMapEventObserver(this);
        } else {
            getLayerFlyLineControl().setStyle(null);
            getMapView().removeMapEventObserver(this);
        }
        getLayerFlyLineControl().setVisible(BizFlyLineType.BizFlyLineTypePoint, bShow);
        Logger.e(TAG, "openFlyLine :" + bShow);
    }

    @Override
    public boolean onMapMoveStart() {
        return false;
    }

    @Override
    public boolean onMapMoveEnd() {
        mDebounceHandler.handle(() -> {
            Log.e("zhaoshun", "onMapMoveEnd: ");
            OperatorPosture operatorPosture = getMapView().getOperatorPosture();
            if (null != operatorPosture && getCallBack() != null) {
                Coord3DDouble coord3DDouble = operatorPosture.getMapCenter();
                GeoPoint descPoint = new GeoPoint(coord3DDouble.lon, coord3DDouble.lat);
                getCallBack().onFlyLineMoveEnd(getMapType(), descPoint);
            }
        });
        return getLayerFlyLineControl().getVisible(BizFlyLineType.BizFlyLineTypePoint);
    }
}
