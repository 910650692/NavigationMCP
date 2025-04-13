package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.PopPointLayerItem;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.layer.model.BizPopPointBusinessInfo;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerLabelStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class LayerLabelImpl extends BaseLayerImpl<LayerLabelStyleAdapter> {

    public LayerLabelImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerLabelControl().setStyle(this);
        getLayerLabelControl().addClickObserver(this);
        getLayerLabelControl().addFocusChangeObserver(this);
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
        List<ILayerAdapterCallBack> callBacks = getCallBacks();
        if (ConvertUtils.isEmpty(callBacks)) {
            Logger.e(TAG, "callBacks is null");
            return;
        }
        Logger.d(TAG, "dispatchClick " + callBacks.size() + " BusinessType " + pItem.getBusinessType());
        for (int i = NumberUtils.NUM_0; i < callBacks.size(); i++) {
            GemLayerItem clickResult = getClickResult(pItem);
            Logger.d(TAG, "dispatchClick clickResult:" + clickResult.toString());
            callBacks.get(i).onRouteItemClick(getMapType(), clickResult);
        }
    }

    private GemLayerItem getClickResult(LayerItem pItem) {
        GemLayerItem result = new GemLayerItem();
        switch (pItem.getBusinessType()) {
            case BizLabelType.BizLabelTypeRoutePopSearchPoint -> {
                if (pItem instanceof PopPointLayerItem popPointLayerItem) {
                    Logger.d(TAG, "getClickResult BizLabelTypeRoutePopSearchPoint");
                    result.setClickBusinessType(GemLayerClickBusinessType.BizLabelTypeRoutePopSearchPoint);
                }
            }
            default -> {
                //待添加其他业务
                result.setClickBusinessType(GemLayerClickBusinessType.UnKnown);
            }
        }
        return result;
    }

    /**
     * 显示终点区域弹出框图层
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
