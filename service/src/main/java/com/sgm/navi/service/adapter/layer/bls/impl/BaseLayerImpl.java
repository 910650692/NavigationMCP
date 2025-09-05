package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.autonavi.gbl.layer.BizAreaControl;
import com.autonavi.gbl.layer.BizCarControl;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.BizFlyLineControl;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.BizUserControl;
import com.autonavi.gbl.layer.model.InnerStyleParam;
import com.autonavi.gbl.layer.observer.PrepareLayerStyleInner;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RouteLayerItem;
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.CustomTextureParam;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.autonavi.gbl.map.layer.model.CustomUpdateParam;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RouteLayerDrawParam;
import com.autonavi.gbl.map.layer.model.RouteLayerStyle;
import com.autonavi.gbl.map.layer.observer.ICarObserver;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.autonavi.gbl.map.model.MapStyleTime;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.sgm.navi.service.adapter.layer.bls.texture.TexturePoolManager;
import com.sgm.navi.service.adapter.layer.bls.texture.TextureStylePoolManager;
import com.sgm.navi.service.adapter.layer.bls.utils.hudLayerStyle.NaviRouteLayerParser;
import com.sgm.navi.service.define.map.MapType;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

public class BaseLayerImpl<S extends BaseStyleAdapter> extends PrepareLayerStyleInner implements ILayerClickObserver, ICarObserver {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;
    protected String className = "BaseLayerImpl";

    @Getter
    private BizControlService bizService;

    @Getter
    private MapView mapView;

    @Getter
    private ILayerAdapterCallBack callBack;

    @Getter
    private Context context;

    @Getter
    private S styleAdapter;

    @Getter
    private MapType mapType;

    private static InnerStyleParam innerStyleParam;

    private NaviRouteLayerParser mNaviRouteLayerParser;

    private static InnerStyleParam innerStyleParam() {
        if (innerStyleParam == null) {
            innerStyleParam = new InnerStyleParam();
            innerStyleParam.layerAssetPath = GBLCacheFilePath.BLS_ASSETS_LAYER_PATH;
            innerStyleParam.cardCmbPaths.add(GBLCacheFilePath.BLS_ASSETS_LAYER_PATH);
            innerStyleParam.cardCmbPaths.add(GBLCacheFilePath.BLS_ASSET_CARD_IMAGE_PATH);
            innerStyleParam.debugMode = BuildConfig.DEBUG;
            Logger.d(MapDefaultFinalTag.LAYER_SERVICE_TAG, "初始化 InnerStyleParam : ", innerStyleParam);
        }
        return innerStyleParam;
    }

    public BaseLayerImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(mapView, null, innerStyleParam());
        this.bizService = bizService;
        this.mapView = mapView;
        this.context = context;
        this.mapType = mapType;
        this.styleAdapter = createStyleAdapter();
        setParam(styleAdapter);
        this.bizService.setCollisionIntervalTime(getEngineId(), 500);
        mNaviRouteLayerParser = new NaviRouteLayerParser();
    }

    public int getEngineId() {
        return getMapView().getEngineId();
    }

    protected S createStyleAdapter() {
        return null;
    }

    protected BizAreaControl getLayerAreaControl() {
        return getBizService().getBizAreaControl(getMapView());
    }

    protected BizCarControl getLayerCarControl() {
        return getBizService().getBizCarControl(getMapView());
    }

    protected BizFlyLineControl getLayerFlyLineControl() {
        return getBizService().getBizFlyLineControl(getMapView());
    }

    protected BizGuideRouteControl getLayerGuideRouteControl() {
        return getBizService().getBizGuideRouteControl(getMapView());
    }

    protected BizRoadCrossControl getLayerRoadCrossControl() {
        return getBizService().getBizRoadCrossControl(getMapView());
    }

    protected BizRoadFacilityControl getLayerRoadFacilityControl() {
        return getBizService().getBizRoadFacilityControl(getMapView());
    }

    protected BizSearchControl getLayerSearchControl() {
        return getBizService().getBizSearchControl(getMapView());
    }

    protected BizUserControl getLayerUserControl() {
        return getBizService().getBizUserControl(getMapView());
    }

    protected BizLabelControl getLayerLabelControl() {
        return getBizService().getBizLabelControl(getMapView());
    }

    public void setCallBack(ILayerAdapterCallBack callBack) {
        this.callBack = callBack;
    }

    public void removeCallback() {
        this.callBack = null;
    }

    @Override
    public void onBeforeNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem item, ClickViewIdInfo clickViewIds) {
        dispatchItemClickEvent(item, clickViewIds);
    }

    @Override
    public void onAfterNotifyClick(BaseLayer layer, LayerItem item, ClickViewIdInfo clickViewIds) {

    }

    @Override
    public void onCarClick(CarLoc carLoc) {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                dispatchCarClick(carLoc);
            }
        });
    }

    @Override
    public void onCarLocChange(CarLoc carLoc) {
    }

    protected void dispatchCarClick(CarLoc carLoc) {

    }

    protected void dispatchItemClickEvent(LayerItem item, ClickViewIdInfo clickViewIds) {

    }

    @Override
    public boolean getCustomTexture(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, CustomTextureParam customTexture) {
        boolean result = super.getCustomTexture(layer, item, styleInfo, customTexture);
        customTexture.attrs.isNightForAsvg = mapView.getOperatorStyle().getMapStyle().time == MapStyleTime.MapTimeNight;
        if (getStyleAdapter() != null) {
            List<CustomUpdatePair> customUpdatePairs = styleAdapter.updateTextureUpdatePair(item, customTexture.attrs.isNightForAsvg);
            if (!customUpdatePairs.isEmpty()) {
                customTexture.updateList.addAll(customUpdatePairs);
            }
            customTexture.cmbFileInfo.isMergeRes = !styleAdapter.isFromCardImagesRes(item);
            if (!customTexture.cmbFileInfo.isMergeRes) {
                Logger.e(TAG, className, " ", mapType, " 使用 自定义 图片资源  图层 :", layer.getName(), " );图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType());
            }
        }
        return result;
    }

    @Override
    public boolean updateCustomTexture(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, CustomUpdateParam updateParam) {
        boolean result = super.updateCustomTexture(layer, item, styleInfo, updateParam);
        updateParam.isNightForAsvg = mapView.getOperatorStyle().getMapStyle().time == MapStyleTime.MapTimeNight;
        if (getStyleAdapter() != null) {
            List<CustomUpdatePair> customUpdatePairs = getStyleAdapter().updateTextureUpdatePair(item, updateParam.isNightForAsvg);
            if (!customUpdatePairs.isEmpty()) {
                updateParam.updateList.addAll(customUpdatePairs);
            }
        }
        return result;
    }

    @Override
    public String getLayerStyle(BaseLayer layer, LayerItem item, boolean forJava) {
        String styleJson = null;
        if (getStyleAdapter() != null) {
            styleJson = TextureStylePoolManager.get().getLayerStyleJson(getMapType(), layer, item, getStyleAdapter());
        }
        if (TextUtils.isEmpty(styleJson)) {
            styleJson = super.getLayerStyle(layer, item, forJava);
            if (Logger.openLog) {
                Logger.d(TAG, className, " ", mapType, " 默认 纹理样式配置 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 ：", item.getItemType(), "\n", styleJson);
            }
        }
        return styleJson;
    }

    @Override
    public int getMarkerId(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo) {
        int markerId = super.getMarkerId(layer, item, styleInfo);
        LayerTexture texture = layer.getMapView().getLayerTexture(markerId);
        long usedTextureCount = layer.getMapView().getUsedTextureCount();
        long capacityTextureCount = 500;
        Logger.i(TAG, "CurrentMapView:" + getMapType(),
                "已使用纹理/支持最大纹理数:" + usedTextureCount + "/" + capacityTextureCount);
        if (usedTextureCount >= capacityTextureCount) {
            Logger.f(TAG, "###Fatal###", "MapView:" + getMapType() + "纹理数超过最大限制500");
        }
        if (getStyleAdapter() != null && ConvertUtils.isNull(texture)) {
            String markerRes = styleInfo.markerId;
            if (!(TextUtils.isEmpty(markerRes) || "-1".equals(markerRes) || "id_static".equals(markerRes) || markerRes.endsWith(".xml"))) {
                texture = TexturePoolManager.get().createLayerTexture(context, getMapType(), layer, item, styleInfo, getStyleAdapter());
                if (layer.getMapView().addLayerTexture(texture)) {
                    markerId = texture.resID;
                    String key = layer.getName() + item.getBusinessType() + item.getID();
                    TexturePoolManager.get().add(key, markerId);
                    if (Logger.openLog) {
                        Logger.e(TAG, "添加图层name ", layer.getName(), " key ", key, " markerId ", markerId);
                    }
                }
                Logger.d(TAG, className, " ", mapType, " 自定义 纹理 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType(),
                        "\n", "纹理信息 :{ markerRes = ", styleInfo.markerId, " ; resID = ", texture.resID, " ; markerId = ", markerId, " }");
            }
        } else {
            if (Logger.openLog) {
                Logger.d(TAG, className, " ", mapType, " 默认 纹理 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType(),
                        "\n", "纹理信息 :{ markerRes = ", styleInfo.markerId, " ; resID = ", texture.resID, " ; markerId = ", markerId, " }");
            }
        }
        return markerId;
    }

    @Override
    public int get3DModelId(BaseLayer layer, LayerItem item, String str3DModelId) {
        int markerId = super.get3DModelId(layer, item, str3DModelId);
        if (!TexturePoolManager.get().isValid(markerId)) {
            Layer3DModel layer3DModel = TexturePoolManager.get().createLayer3DModel(item, str3DModelId);
            if (layer.getMapView().addLayer3DModel(layer3DModel)) {
                markerId = layer3DModel.resourceID;
            }
            Logger.e(TAG, className, " ", mapType, " 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType(),
                    "\n", "使用 自定义  3D 纹理信息 :{ markerRes = ", str3DModelId, " ; markerId = ", markerId, " }");
        } else {
            Logger.v(TAG, className, " ", mapType, " 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType(),
                    "\n", " 使用 默认  3D 纹理信息 :{ markerRes = ", str3DModelId, " ; markerId = ", markerId, " }");
        }

        return markerId;
    }

    @Override
    public boolean getRouteLayerStyle(BaseLayer layer, LayerItem item, RouteLayerStyle style) {
        Logger.d(TAG, mapType, " getRouteLayerStyle style ", style);
        if (null != style && mapType == MapType.HUD_MAP) {
            RouteLayerItem routeLayerItem = (RouteLayerItem) item;
            RouteLayerDrawParam routeDrawParam = routeLayerItem.getRouteDrawParam();
            boolean isNightMode = ThemeUtils.INSTANCE.isNightModeEnabled(getContext());
            int fillLineColor = getContext().getColor(R.color.hud_color_route);
            mNaviRouteLayerParser.getRouteLayerStyle(layer, item, style,
                    this, routeDrawParam.mRouteStyleType, isNightMode, fillLineColor);
            return true;
        }
        return super.getRouteLayerStyle(layer, item, style);
    }

    @Override
    public boolean isRouteCacheStyleEnabled() {
        return true;
    }

    @Override
    public boolean isRouteStyleNightMode() {
        return super.isRouteStyleNightMode();
    }

    @Override
    public void clearLayerItem(BaseLayer layer, LayerItem item) {
        super.clearLayerItem(layer, item);
        Logger.i(TAG, className, " ", mapType, " 图层 :", layer.getName(), " ;图元业务类型 :", item.getBusinessType(), " ; 图元 :", item.getItemType(), " 删除纹理 ");
        String key = layer.getName() + item.getBusinessType() + item.getID();
        if (TexturePoolManager.get().containsKey(key)) {
            CopyOnWriteArrayList<Integer> markerIdList = TexturePoolManager.get().getValueAsInt(key);
            if (!ConvertUtils.isEmpty(markerIdList)) {
                for (Integer markerId : markerIdList) {
                    if (!ConvertUtils.isEmpty(markerId)) {
                        layer.getMapView().destroyTexture(markerId);
                    }
                }
            }
            TexturePoolManager.get().removeKey(key);
            Logger.e(TAG, className, " clearLayerItem key:", key, " markerIdList.size:", markerIdList.size());
        }
    }

    @Override
    public void clearLayerItems(BaseLayer layer) {
        super.clearLayerItems(layer);
        List<Integer> removeValues = TexturePoolManager.get().removeKeys(layer.getName());
        if (!removeValues.isEmpty()) {
            Logger.i(TAG, className, " ", mapType, " 图层 :", layer.getName(), " 删除纹理数量: ", removeValues.size());
        }
        for (Integer value : removeValues) {
            if (null != value) {
                layer.getMapView().destroyTexture(value);
                if (Logger.openLog) {
                    Logger.e(TAG, className, " clearLayerItems layer: ", layer.getName(), " markerId: ", value);
                }
            }
        }
    }
}
