package com.fy.navi.service.adapter.layer.bls.impl;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.model.InnerStyleParam;
import com.autonavi.gbl.layer.observer.PrepareLayerParam;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RouteLayerItem;
import com.autonavi.gbl.map.layer.model.CustomTextureParam;
import com.autonavi.gbl.map.layer.model.CustomUpdateParam;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.RouteLayerDrawParam;
import com.autonavi.gbl.map.layer.model.RouteLayerStyle;
import com.autonavi.gbl.map.layer.observer.IPrepareLayerStyle;
import com.fy.navi.service.adapter.layer.bls.parser.NaviRouteLayerParser;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.setting.SettingConstant;

/**
 * @Description TODO 自定义图层纹理配置 谨慎使用
 * @Author lvww
 * @date 2024/12/10
 */
public class PrepareLayerStyleImpl implements IPrepareLayerStyle {
    private static final String TAG = "PrepareLayerStyleImpl";
    private MapTypeId mapTypeId;
    private NaviRouteLayerParser mNaviRouteLayerParser;

    public PrepareLayerStyleImpl(MapTypeId mapTypeId, MapView mapView, PrepareLayerParam customParam, InnerStyleParam param) {
        this.mapTypeId = mapTypeId;
        mNaviRouteLayerParser = new NaviRouteLayerParser();
    }

    @Override
    public int getMarkerId(BaseLayer baseLayer, LayerItem layerItem, ItemStyleInfo itemStyleInfo) {

        return 0;
    }

    @Override
    public boolean getCustomTexture(BaseLayer baseLayer, LayerItem layerItem, ItemStyleInfo itemStyleInfo, CustomTextureParam customTextureParam) {
        return false;
    }

    @Override
    public boolean updateCustomTexture(BaseLayer baseLayer, LayerItem layerItem, ItemStyleInfo itemStyleInfo, CustomUpdateParam customUpdateParam) {
        return false;
    }

    @Override
    public int get3DModelId(BaseLayer baseLayer, LayerItem layerItem, String s) {
        return 0;
    }

    @Override
    public String getLayerStyle(BaseLayer baseLayer, LayerItem layerItem, boolean b) {
        String strStyleJson = "EMPTY";
        if (baseLayer == null || layerItem == null) {
            return strStyleJson;
        }
        int itemType = layerItem.getItemType();
        int businessType = layerItem.getBusinessType();
        Logger.d(TAG, "itemType=" + itemType + ", businessType=" + businessType);
        return strStyleJson;
    }

    @Override
    public boolean getRouteLayerStyle(BaseLayer baseLayer, LayerItem layerItem, RouteLayerStyle routeLayerStyle) {
        if (layerItem == null || baseLayer == null || routeLayerStyle == null) return false;

        int itemType = layerItem.getItemType();
        int businessType = layerItem.getBusinessType();
        Logger.d(TAG, "getLayerStyle: route itemType = " + itemType + ",businessType = " + businessType);
        RouteLayerItem routeLayerItem = (RouteLayerItem) layerItem;
        RouteLayerDrawParam routeDrawParam = routeLayerItem.getRouteDrawParam();

        mNaviRouteLayerParser.getRouteLayerStyle(baseLayer, layerItem, routeLayerStyle, this,
                routeDrawParam.mRouteStyleType, SettingConstant.isNightMode, mapTypeId);
        return true;
    }

    @Override
    public boolean isRouteCacheStyleEnabled() {
        return false;
    }

    @Override
    public boolean isRouteStyleNightMode() {
        return SettingConstant.isNightMode;
    }

    @Override
    public String getCommonInfo(String s) {
        return "";
    }

    @Override
    public boolean switchStyle(int i) {
        return false;
    }
}
