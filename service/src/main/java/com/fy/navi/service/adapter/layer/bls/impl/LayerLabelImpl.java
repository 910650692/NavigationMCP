package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.layer.model.BizPopPointBusinessInfo;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerLabelStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;

import java.util.ArrayList;

public class LayerLabelImpl extends BaseLayerImpl<LayerLabelStyleAdapter> {

    public LayerLabelImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerLabelControl().setStyle(this);
        getLayerLabelControl().addClickObserver(this);
    }

    @Override
    protected LayerLabelStyleAdapter createStyleAdapter() {
        return new LayerLabelStyleAdapter(getEngineId(), getLayerLabelControl());
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        super.onNotifyClick(layer, pItem, clickViewIds);
        dispatchClick(pItem);
    }

    private void dispatchClick(LayerItem pItem) {
        LayerPointItemType type = LayerPointItemType.NULL;
        LayerItemRoutePointClickResult result = new LayerItemRoutePointClickResult();
        switch (pItem.getBusinessType()) {
            case BizLabelType.BizLabelTypeRoutePopSearchPoint -> {
                type = LayerPointItemType.ROUTE_POINT_END_PARK;
            }
        }
        Logger.d(TAG, "dispatchItemClickEvent type = " + type + " ; result = " + result.toString());
        for (ILayerAdapterCallBack callback : getCallBacks()) {
            callback.onRouteItemClick(getMapType(), type, result);
        }
    }

    /**
     * 显示终点区域弹出框图层
     *
     * @param labelResult
     */
    public boolean updatePopSearchPointInfo(LayerItemLabelResult labelResult) {
        Logger.d(TAG, "updatePopSearchPointInfo");
        if (ConvertUtils.isEmpty(labelResult)) {
            Logger.e(TAG, "updatePopSearchPointInfo labelResult == null");
            return false;
        }
        GeoPoint pos = labelResult.getPos();
        if (ConvertUtils.isEmpty(pos)) {
            Logger.e(TAG, "updatePopSearchPointInfo pos == null");
            return false;
        }
        getLayerLabelControl().setVisible(BizLabelType.BizLabelTypeRoutePopSearchPoint, true);
        ArrayList<BizPopPointBusinessInfo> popEnds = new ArrayList<>();
        BizPopPointBusinessInfo popEnd = new BizPopPointBusinessInfo();
        popEnd.text = labelResult.getPointType();
        popEnd.mPos3D.lat = pos.getLat();
        popEnd.mPos3D.lon = pos.getLon();
        popEnds.add(popEnd);
        //显示终点区域弹出框图层
        getLayerLabelControl().updatePopSearchPointInfo(popEnds);
        return true;
    }

    /**
     * 清除扎标
     */
    public void clearLabelItem() {
        getLayerLabelControl().clearAllItems();
        getLayerLabelControl().setVisible(false);
        Logger.d(TAG, "clearLabelItem");
    }
}
