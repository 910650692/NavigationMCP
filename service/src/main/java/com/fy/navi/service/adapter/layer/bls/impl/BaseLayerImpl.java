package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
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
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.CustomTextureParam;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.autonavi.gbl.map.layer.model.CustomUpdateParam;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RouteLayerStyle;
import com.autonavi.gbl.map.layer.observer.ICarObserver;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.fy.navi.service.BuildConfig;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.texture.TexturePoolManager;
import com.fy.navi.service.adapter.layer.bls.texture.TextureStylePoolManager;
import com.fy.navi.service.define.map.MapType;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

public class BaseLayerImpl<S extends BaseStyleAdapter> extends PrepareLayerStyleInner implements ILayerClickObserver, ICarObserver {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    @Getter
    private BizControlService bizService;

    @Getter
    private MapView mapView;
    @Getter
    private List<ILayerAdapterCallBack> callBacks = new CopyOnWriteArrayList<>();

    @Getter
    private Context context;

    @Getter
    private S styleAdapter;

    @Getter
    private MapType mapType;

    private static InnerStyleParam innerStyleParam() {
        InnerStyleParam param = new InnerStyleParam();
        String path = GBLCacheFilePath.BLS_ASSETS_LAYER_PATH;
        param.layerAssetPath = path;
        param.cardCmbPaths.add(path);
        param.debugMode = BuildConfig.DEBUG;
        return param;
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

    public void addLayerClickCallback(ILayerAdapterCallBack callBack) {
        if (!callBacks.contains(callBack)) {
            callBacks.add(callBack);
        }
    }

    public void removeClickCallback(ILayerAdapterCallBack callBack) {
        if (callBacks.contains(callBack)) {
            callBacks.remove(callBack);
        }
    }

    @Override
    public void onBeforeNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem item, ClickViewIdInfo clickViewIds) {
        dispatchItemClickEvent(item);
    }

    @Override
    public void onAfterNotifyClick(BaseLayer layer, LayerItem item, ClickViewIdInfo clickViewIds) {

    }

    @Override
    public void onCarClick(CarLoc carLoc) {
    }

    @Override
    public void onCarLocChange(CarLoc carLoc) {
    }

    protected void dispatchItemClickEvent(LayerItem item) {

    }

    @Override
    public boolean getCustomTexture(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, CustomTextureParam customTexture) {
        boolean result = super.getCustomTexture(layer, item, styleInfo, customTexture);
        customTexture.cmbFileInfo.vecResPath.add(GBLCacheFilePath.BLS_ASSETS_LAYER_PATH);
        customTexture.cmbFileInfo.strPkgName = GBLCacheFilePath.CMB_FILE_NAME;
        if (getStyleAdapter() != null) {
            List<CustomUpdatePair> customUpdatePairs = styleAdapter.updateTextureUpdatePair(item);
            if (!customUpdatePairs.isEmpty()) {
                customTexture.updateList.addAll(customUpdatePairs);
            }
        }
        Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                + "; 父类  result =" + result + "\n " + customTexture.markerKey.customXmlStr);
        return result;
    }

    @Override
    public boolean updateCustomTexture(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo, CustomUpdateParam updateParam) {
        boolean result = super.updateCustomTexture(layer, item, styleInfo, updateParam);
        Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                + "; 父类  result =" + result);
        if (getStyleAdapter() != null) {
            List<CustomUpdatePair> customUpdatePairs = getStyleAdapter().updateTextureUpdatePair(item);
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
            Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + ";使用 默认纹理 样式配置");
        }
        Logger.v(TAG, "纹理样式 =" + styleJson.replaceAll("\\s", ""));
        return styleJson;
    }

    @Override
    public int getMarkerId(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo) {
        int markerId = super.getMarkerId(layer, item, styleInfo);
        LayerTexture texture = layer.getMapView().getLayerTexture(markerId);
        if (getStyleAdapter() != null && ConvertUtils.isNull(texture)) {
            String markerRes = styleInfo.markerId;
            if (!(TextUtils.isEmpty(markerRes) || "-1".equals(markerRes) || "id_static".equals(markerRes) || markerRes.endsWith(".xml"))) {
                texture = TexturePoolManager.get().createLayerTexture(context, getMapType(), layer, item, styleInfo, getStyleAdapter());
                if (layer.getMapView().addLayerTexture(texture)) {
                    markerId = texture.resID;
                }
                Logger.e(TAG, getClass().getSimpleName() + " " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 :" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                        + "\n" + "使用 自定义  纹理信息 :{ markerRes = " + styleInfo.markerId + " ; resID = " + texture.resID + " ; markerId = " + markerId + " }");
            }
        } else {
            Logger.v(TAG, getClass().getSimpleName() + " " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 :" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + "\n" + "使用 默认  纹理信息 :{ markerRes = " + styleInfo.markerId + " ; resID = " + texture.resID + " ; markerId = " + markerId + " }");
        }
        return markerId;
    }

    @Override
    public int get3DModelId(BaseLayer layer, LayerItem item, String str3DModelId) {
        int markerId = super.get3DModelId(layer, item, str3DModelId);
        boolean isAddSuccess = false;
        if (!TexturePoolManager.get().isValid(markerId)) {
            Layer3DModel layer3DModel = TexturePoolManager.get().createLayer3DModel(item, str3DModelId);
            isAddSuccess = layer.getMapView().addLayer3DModel(layer3DModel);
            if (isAddSuccess) {
                markerId = layer3DModel.resourceID;
            }
            Logger.e(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 :" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + "\n" + "使用 自定义  3D 纹理信息 :{ markerRes = " + str3DModelId + " ; markerId = " + markerId + " }");
        } else {
            Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 :" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                    + "\n" + " 使用 默认  3D 纹理信息 :{ markerRes = " + str3DModelId + " ; markerId = " + markerId + " }");
        }

        return markerId;
    }

    @Override
    public boolean getRouteLayerStyle(BaseLayer layer, LayerItem item, RouteLayerStyle style) {
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
        Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 :" + item.getItemType() + " ; 是否可见 :" + item.getVisible()
                + " 删除纹理 ");
    }

    @Override
    public void clearLayerItems(BaseLayer layer) {
        Logger.v(TAG, getClass().getSimpleName() + " mapType = " + mapType + " 图层 :" + layer.getName() + " ; 是否可见 :" + layer.getVisible()
                + " 删除纹理 ");
    }
}
