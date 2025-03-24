package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.file.ParseJsonUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizAreaControl;
import com.autonavi.gbl.layer.BizCarControl;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.BizCustomControl;
import com.autonavi.gbl.layer.BizFlyLineControl;
import com.autonavi.gbl.layer.BizGuideEagleEyeControl;
import com.autonavi.gbl.layer.BizGuideRouteControl;
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
import com.autonavi.gbl.map.layer.model.RouteLayerStyleType;
import com.autonavi.gbl.map.layer.observer.ICarObserver;
import com.autonavi.gbl.map.layer.observer.ILayerClickObserver;
import com.autonavi.gbl.map.layer.observer.ILayerFocusChangeObserver;
import com.fy.navi.service.BuildConfig;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;

import java.io.File;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class BaseLayerImpl<S extends BaseStyleAdapter> extends PrepareLayerStyleInner implements ILayerClickObserver, ICarObserver, ILayerFocusChangeObserver, IBizRoadFacilityObserver {
    protected static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private static final String ROOT_PATH = "layerJson" + File.separator + "style";
    private static final String JSON = ".json";

    private BizControlService bizService;
    private MapView mapView;
    private List<ILayerAdapterCallBack> callBacks = new CopyOnWriteArrayList<>();
    private Context context;

    private S styleAdapter;

    private static InnerStyleParam innerStyleParam() {
        InnerStyleParam param = new InnerStyleParam();
        String path = GBLCacheFilePath.BLS_ASSETS_LAYER_PATH;
        param.layerAssetPath = path;
        param.customLayerAssetPath = GBLCacheFilePath.BLS_ASSETS_CUSTOM_LAYER_PATH;
        param.cardCmbPaths.add(path);
        param.debugMode = BuildConfig.DEBUG;
        return param;
    }

    public BaseLayerImpl(BizControlService bizService, MapView mapView, Context context) {
        super(mapView, null, innerStyleParam());
        this.bizService = bizService;
        this.mapView = mapView;
        this.context = context;
        this.styleAdapter = createStyleAdapter();
    }

    protected S createStyleAdapter() {
        return null;
    }

    public S getStyleAdapter() {
        return styleAdapter;
    }

    public MapView getMapView() {
        return mapView;
    }

    protected BizAreaControl getLayerAreaControl() {
        return bizService.getBizAreaControl(mapView);
    }

    protected BizCarControl getLayerCarControl() {
        return bizService.getBizCarControl(mapView);
    }

    protected BizCustomControl getLayerCustomControl() {
        return bizService.getBizCustomControl(mapView);
    }

    protected BizGuideEagleEyeControl getLayerEagleEyeControl() {
        return bizService.getBizGuideEagleEyeControl(mapView.getDeviceId());
    }

    protected BizFlyLineControl getLayerFlyLineControl() {
        return bizService.getBizFlyLineControl(mapView);
    }

    protected BizGuideRouteControl getLayerGuideRouteControl() {
        return bizService.getBizGuideRouteControl(mapView);
    }

    protected BizRoadCrossControl getLayerRoadCrossControl() {
        return bizService.getBizRoadCrossControl(mapView);
    }

    protected BizRoadFacilityControl getLayerRoadFacilityControl() {
        return bizService.getBizRoadFacilityControl(mapView);
    }

    protected BizSearchControl getLayerSearchControl() {
        return bizService.getBizSearchControl(mapView);
    }

    protected BizUserControl getLayerUserControl() {
        return bizService.getBizUserControl(mapView);
    }

    public void addLayerClickCallback(ILayerAdapterCallBack callBack) {
        if (callBacks.contains(callBack)) return;
        callBacks.add(callBack);
    }

    public void removeClickCallback(ILayerAdapterCallBack callBack) {
        if (callBacks.contains(callBack)) {
            callBacks.remove(callBack);
        }
    }

    @Override
    public void onBeforeNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(TAG, getClass().getSimpleName() + " onBeforeNotifyClick");
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(TAG, getClass().getSimpleName() + " onNotifyClick");

    }

    @Override
    public void onAfterNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        Logger.d(TAG, getClass().getSimpleName() + " onAfterNotifyClick");

    }

    @Override
    public void onNotifyFocusChange(BaseLayer baseLayer, LayerItem layerItem, boolean b) {

    }

    @Override
    public void onNotifyCameraFilterInfo(CameraFilterInfo cameraFilterInfo) {

    }

    @Override
    public void onCarClick(CarLoc carLoc) {
        Logger.d(TAG, getClass().getSimpleName() + " onCarClick");
    }

    @Override
    public void onCarLocChange(CarLoc carLoc) {
        Logger.d(TAG, getClass().getSimpleName() + " onCarLocChange");
    }

    @Override
    public String getLayerStyle(BaseLayer layer, LayerItem item, boolean forJava) {
        String styleJson = super.getLayerStyle(layer, item, forJava);
        if (styleAdapter != null) {
            String layerItemJson = styleAdapter.provideLayerItemStyleJson(item);
            if (!TextUtils.isEmpty(layerItemJson)) {
                String jsonFilePath = new StringBuffer(ROOT_PATH).append(mapView.getEngineId())
                        .append(File.separator)
                        .append(layerItemJson).append(JSON).toString();
                if (jsonFilePath.endsWith(JSON)) {
                    styleJson = ParseJsonUtils.parseJsonFile(jsonFilePath);
                }
                Logger.d(TAG, getClass().getSimpleName() +
                        "\n ; 图层 :" + layer.getName() + ";图元业务类型 :" + item.getBusinessType() + ";图元类型 ：" + item.getItemType()
                        + "\n自定义 样式配置 : " + styleJson);
            }
        }
        if (TextUtils.isEmpty(styleJson)) {
            styleJson = super.getLayerStyle(layer, item, forJava);
            Logger.d(TAG, getClass().getSimpleName() +
                    "\n  ; 图层 :" + layer.getName() + ";图元业务类型 :" + item.getBusinessType() + ";图元类型 ：" + item.getItemType()
                    + "\n默认 样式配置 : " + styleJson);
        }
        return styleJson;
    }


    @Override
    public int getMarkerId(BaseLayer layer, LayerItem item, ItemStyleInfo styleInfo) {
        if (ConvertUtils.isNull(layer) || ConvertUtils.isNull(item) || ConvertUtils.isNull(styleInfo)) {
            Logger.e(TAG, " ; 图层 :" + layer.getName() + " null");
            return LayerTextureManager.DEF_ERROR_ID;
        }
        int markerId = super.getMarkerId(layer, item, styleInfo);
        //无效的markID
        if (LayerTextureManager.get().isNotValid(markerId)) {
            String markerIdKey = new StringBuffer(layer.getName()).append("@").append(item.getFocus())
                    .append("@").append(styleInfo.markerKey)
                    .append("@").append(styleInfo.markerId).toString();
            markerId = LayerTextureManager.get().getMarkerId(markerIdKey);
            if (LayerTextureManager.get().isNotValid(markerId)) {
                LayerTexture layerTexture;
                if (styleAdapter != null) {
                    layerTexture = LayerTextureManager.get().createLayerTexture(context, styleInfo.markerId, styleInfo.markerInfo,
                            item.getFocus(), styleAdapter.provideLayerItemProcessor(item), styleAdapter.provideLayerItemDataProcessor(item));
                } else {
                    layerTexture = LayerTextureManager.get().createLayerTexture(context, styleInfo.markerId, styleInfo.markerInfo,
                            item.getFocus(), null, null);
                }
                if (layer.getMapView().addLayerTexture(layerTexture)) {
                    markerId = LayerTextureManager.get().addMarkerId(markerIdKey, layerTexture.resID);
                    Logger.d("LayerTextureManager", getClass().getSimpleName() +
                            "\n 纹理添加成功  图层 :" + layer.getName() + ";图元业务类型 :" + item.getBusinessType() + ";图元类型 ：" + item.getItemType()
                            + "\n动态 纹理ID:" + markerId
                            + "\n 样式配置 : " + GsonUtils.toJson(styleInfo));
                } else {
                    Logger.e(TAG, getClass().getSimpleName() +
                            "\n  纹理添加失败  图层 :" + layer.getName() + "纹理 is null：" + (layerTexture == null));
                }
            }
        } else {
            Logger.d(TAG, getClass().getSimpleName() +
                    "\n  纹理添加成功 图层 :" + layer.getName() + ";图元业务类型 :" + item.getBusinessType() + ";图元类型 ：" + item.getItemType()
                    + "\n 默认纹 理ID:" + markerId
                    + "\n 样式配置 : " + GsonUtils.toJson(styleInfo));
        }
        return markerId;
    }

    @Override
    public int get3DModelId(BaseLayer layer, LayerItem item, String str3DModelId) {
        if (ConvertUtils.isNull(layer) || ConvertUtils.isNull(str3DModelId)) {
            Logger.d(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " error");
            return LayerTextureManager.DEF_ERROR_ID;
        }
        int markerId = super.get3DModelId(layer, item, str3DModelId);
        if (LayerTextureManager.get().isNotValid(markerId)) {
            markerId = LayerTextureManager.get().get3DModelId(layer.getName(), str3DModelId);
            if (LayerTextureManager.get().isNotValid(markerId)) {
                Layer3DModel layer3DModel = LayerTextureManager.get().createLayer3DModel(str3DModelId);
                if (layer.addLayer3DModel(layer3DModel)) {
                    markerId = LayerTextureManager.get().add3DModelId(layer.getName(), str3DModelId, layer3DModel.resourceID);
                } else {
                    Logger.d(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " 3D纹理创建失败");
                }
            }
        }
        Logger.d(TAG, getClass().getSimpleName() + " ; 图层 :" + layer.getName() + " ; 图元个数 : " + layer.getCount() + "; "
                + "\n图元业务类型 :" + ((null == item) ? " null " : item.getBusinessType()) + " ; 图元类型 ：" + ((null == item) ? " null " : item.getItemType())
                + "\n默认3D纹理ID:" + markerId
                + "\n默认3D样式配置 : " + str3DModelId);

        return markerId;
    }

    @Override
    public boolean getRouteLayerStyle(BaseLayer layer, LayerItem item, RouteLayerStyle style) {
        if (item instanceof RouteLayerItem){
            RouteLayerItem routeLayerItem = (RouteLayerItem) item;
            RouteLayerDrawParam routeDrawParam = routeLayerItem.getRouteDrawParam();

        }
        return super.getRouteLayerStyle(layer,item,style);
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
    }

    @Override
    public void clearLayerItems(BaseLayer layer) {
    }

}
