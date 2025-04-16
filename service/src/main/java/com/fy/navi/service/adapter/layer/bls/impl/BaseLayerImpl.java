package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.gson.GsonUtils;
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
import com.autonavi.gbl.layer.model.CameraFilterInfo;
import com.autonavi.gbl.layer.model.InnerStyleParam;
import com.autonavi.gbl.layer.observer.IBizRoadFacilityObserver;
import com.autonavi.gbl.layer.observer.PrepareLayerStyleInner;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RouteLayerItem;
import com.autonavi.gbl.map.layer.model.CarLoc;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.Layer3DModel;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RouteLayerDrawParam;
import com.autonavi.gbl.map.layer.model.RouteLayerStyle;
import com.autonavi.gbl.map.layer.observer.ICarObserver;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.autonavi.gbl.map.layer.observer.ILayerFocusChangeObserver;
import com.fy.navi.service.BuildConfig;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.BaseStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.texture.LayerTextureManager;
import com.fy.navi.service.define.map.MapType;

import java.io.File;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

public class BaseLayerImpl<S extends BaseStyleAdapter> extends PrepareLayerStyleInner implements ILayerClickObserver, ICarObserver, ILayerFocusChangeObserver, IBizRoadFacilityObserver {

    protected String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String JSON = ".json";

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
        Logger.d(TAG, " onBeforeNotifyClick");
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(TAG, " onNotifyClick");

    }

    @Override
    public void onAfterNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(TAG, " onAfterNotifyClick");

    }

    @Override
    public void onNotifyFocusChange(BaseLayer baseLayer, LayerItem layerItem, boolean b) {
        Logger.d(TAG, " onNotifyFocusChange");
    }


    @Override
    public void onNotifyCameraFilterInfo(CameraFilterInfo cameraFilterInfo) {
//        Logger.d(TAG, " onNotifyCameraFilterInfo");
    }

    @Override
    public void onCarClick(CarLoc carLoc) {
        Logger.d(TAG, " onCarClick");
    }

    @Override
    public void onCarLocChange(CarLoc carLoc) {
//        Logger.d(TAG, " onCarLocChange");
    }

    @Override
    public String getLayerStyle(BaseLayer layer, LayerItem item, boolean forJava) {
        if (ConvertUtils.isNull(layer) || ConvertUtils.isNull(item)) {
            Logger.e(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " 获取样式配置信息出错");
            return "EMPTY";
        }
        String styleJson = null;

        if (getStyleAdapter() != null) {
            Logger.d(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " 使用自定义配置信息");
            String jsonPathName = getStyleAdapter().provideLayerItemStyleJson(item);
            if (!TextUtils.isEmpty(jsonPathName)) {
                String jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                        .append(getEngineId())
                        .append(File.separator)
                        .append(jsonPathName).toString();
                if (!jsonFilePath.endsWith(JSON)) {
                    jsonFilePath = new StringBuffer(jsonFilePath).append(JSON).toString();
                }
                styleJson = ParseJsonUtils.parseJsonFile(jsonFilePath);
                if (TextUtils.isEmpty(styleJson)) {
                    jsonFilePath = new StringBuffer(GBLCacheFilePath.BLS_ASSETS_CUSTOM_STYLE_PATH)
                            .append(MapType.MAIN_SCREEN_MAIN_MAP.getMapType())
                            .append(File.separator)
                            .append(jsonPathName).toString();
                    Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ; 解析自定义配置 json文件出错 采用主屏默认样式 =" + jsonFilePath);
                    styleJson = ParseJsonUtils.parseJsonFile(jsonFilePath);
                } else {
                    Logger.d(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ; 解析自定义配置 json文件成功 =" + jsonFilePath);
                }
            }
            if (!TextUtils.isEmpty(styleJson)) {
                //缓存自定义json TODO
            }
        }

        if (TextUtils.isEmpty(styleJson)) {
            Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;未配置自定义 ； 使用默认配置");
            styleJson = super.getLayerStyle(layer, item, forJava);
            if (TextUtils.isEmpty(styleJson)) {
                styleJson = "EMPTY";
            }
        }

        Logger.d(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                + " ;样式配置 : \n" + styleJson);

        return styleJson;
    }


    @Override
    public int getMarkerId(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo) {
        if (ConvertUtils.isNull(layer) || ConvertUtils.isNull(item) || ConvertUtils.isNull(styleInfo)) {
            Logger.e(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " null");
            return LayerTextureManager.DEF_ERROR_ID;
        }
        int markerId = super.getMarkerId(layer, item, styleInfo);
        if (!LayerTextureManager.get().isValid(markerId)) {
            String markerIdKey = new StringBuffer(layer.getName())
                    .append("@").append(item.getFocus())
                    .append("@").append(styleInfo.markerId).toString();
            if (!LayerTextureManager.get().isHasMarkerId(markerIdKey) ||
                    (getStyleAdapter() != null && getStyleAdapter().isNeedReCreate(item))) {
                Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;开始创建 新纹理 " + markerIdKey);
                LayerTexture layerTexture = LayerTextureManager.get().createLayerTexture(context, styleInfo.markerId, styleInfo.markerInfo, item.getFocus(),
                        getStyleAdapter().provideUpdateBitmapViewProcessor(item), getStyleAdapter().provideLayerItemData(item));
                if (layer.getMapView().addLayerTexture(layerTexture)) {
                    markerId = LayerTextureManager.get().addMarkerId(markerIdKey, layerTexture.resID);
                    Logger.d(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                            + " \n添加纹理 :{" + markerIdKey + " ; " + markerId + " }; 成功");
                } else {
                    markerId = LayerTextureManager.DEF_ERROR_ID;
                    Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                            + " \n添加纹理 :{" + markerIdKey + " ; " + markerId + " }; 失败 ");
                }
            } else {
                markerId = LayerTextureManager.get().getMarkerId(markerIdKey);
                Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                        + " \n复用 纹理 :{" + markerIdKey + " ; " + markerId + " } ");
            }
        } else {
            Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                    + " \n复用 默认 纹理 :{ NULL ; " + markerId + " } ");
        }
        return markerId;
    }

    @Override
    public int get3DModelId(BaseLayer layer, LayerItem item, String str3DModelId) {
        if (ConvertUtils.isNull(layer) || ConvertUtils.isNull(item)) {
            Logger.e(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " null");
            return LayerTextureManager.DEF_ERROR_ID;
        }

        int markerId = super.get3DModelId(layer, item, str3DModelId);
        if (!LayerTextureManager.get().isValid(markerId) ||
                (getStyleAdapter() != null && getStyleAdapter().isNeedReCreate(item))) {
            String markerIdKey = new StringBuffer(layer.getName())
                    .append("@").append(item.getFocus())
                    .append("@").append(str3DModelId).toString();
            if (!LayerTextureManager.get().isHasMarkerId(markerIdKey)) {
                Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;开始创建  3D 纹理 " + markerIdKey);
                Layer3DModel layer3DModel = LayerTextureManager.get().createLayer3DModel(str3DModelId);
                if (layer.addLayer3DModel(layer3DModel)) {
                    markerId = LayerTextureManager.get().addMarkerId(markerIdKey, layer3DModel.resourceID);
                    Logger.d(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                            + " \n添加 3D 纹理 :{" + markerIdKey + " ; " + markerId + " }; 成功");
                } else {
                    markerId = LayerTextureManager.DEF_ERROR_ID;
                    Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                            + " \n添加 3D 纹理 :{" + markerIdKey + " ; " + markerId + " }; 失败 ");
                }
            } else {
                markerId = LayerTextureManager.get().getMarkerId(markerIdKey);
                Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                        + " \n复用 3D 纹理 :{" + markerIdKey + " ; " + markerId + " } ");
            }
        } else {
            Logger.e(TAG, getClass().getSimpleName() + " 图层 :" + layer.getName() + " ;图元业务类型 :" + item.getBusinessType() + " ; 图元 ：" + item.getItemType()
                    + " \n复用 默认 3D 纹理 :{" + null + " ; " + markerId + " } ");
        }
        return markerId;
    }

    @Override
    public boolean getRouteLayerStyle(BaseLayer layer, LayerItem item, RouteLayerStyle style) {
        if (item instanceof RouteLayerItem) {
            RouteLayerItem routeLayerItem = (RouteLayerItem) item;
            RouteLayerDrawParam routeDrawParam = routeLayerItem.getRouteDrawParam();

        }
        Logger.d(TAG, "路线样式 ：" + GsonUtils.toJson(style));
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
        String markerIdKey = new StringBuffer(layer.getName())
                .append("@").append(item.getFocus())
                .append("@").toString();
        Logger.d(TAG, getClass().getSimpleName() + " 删除纹理 " + markerIdKey);
        LayerTextureManager.get().clearMarkerId(markerIdKey);
    }

    @Override
    public void clearLayerItems(BaseLayer layer) {
        String markerIdKey = new StringBuffer(layer.getName())
                .append("@").toString();
        Logger.d(TAG, getClass().getSimpleName() + " 删除纹理 " + markerIdKey);
        LayerTextureManager.get().clearMarkerId(markerIdKey);
    }
}
