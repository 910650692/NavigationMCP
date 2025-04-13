package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizClickLabelType;
import com.autonavi.gbl.layer.model.BizFlyLineType;
import com.autonavi.gbl.layer.model.FlylineDrawMode;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.OperatorPosture;
import com.autonavi.gbl.map.observer.IMapEventObserver;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerFlyLineStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;

public class LayerFlyLineImpl extends BaseLayerImpl<LayerFlyLineStyleAdapter> implements IMapEventObserver {

    public LayerFlyLineImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
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
            getLayerFlyLineControl().setClickLabelType(BizClickLabelType.ClickTypeLabel);
            getLayerFlyLineControl().updateDrawMode(FlylineDrawMode.FLYLINE_MOVE_END, true);
        } else {
            getMapView().removeMapEventObserver(this);
            getLayerFlyLineControl().setClickLabelType(BizClickLabelType.ClickTypeNone);
            getLayerFlyLineControl().updateDrawMode(FlylineDrawMode.FLYLINE_NONE_END, true);
            getLayerFlyLineControl().setStyle(null);
        }
        getLayerFlyLineControl().setVisible(BizFlyLineType.BizFlyLineTypeLine, false);
        getLayerFlyLineControl().setVisible(BizFlyLineType.BizFlyLineTypePoint, true);
        getLayerFlyLineControl().setClickLabelMoveMap(bShow);
        Logger.e(TAG, "openFlyLine :" + bShow);
    }

    @Override
    public boolean onMapMoveStart() {
        return false;
    }

    @Override
    public boolean onMapMoveEnd() {
        OperatorPosture operatorPosture = getMapView().getOperatorPosture();
        if (null != operatorPosture) {
            Coord3DDouble coord3DDouble = operatorPosture.getMapCenter();
            GeoPoint descPoint = new GeoPoint(coord3DDouble.lon, coord3DDouble.lat);
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    for (ILayerAdapterCallBack callBack : getCallBacks()) {
                        Logger.e(TAG, "onMapMoveEnd-LayerFlyLineImpl:" + Thread.currentThread().getName());
                        callBack.onFlyLineMoveEnd(getMapType(), descPoint);
                    }
                }
            });
        }
        return true;
    }
}
