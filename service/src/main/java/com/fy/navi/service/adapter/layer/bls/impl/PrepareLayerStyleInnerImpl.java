package com.fy.navi.service.adapter.layer.bls.impl;

import static com.autonavi.gbl.map.layer.model.LayerIconType.LayerIconTypeBMP;

import android.annotation.SuppressLint;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Canvas;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRoadFacilityType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.layer.model.InnerStyleParam;
import com.autonavi.gbl.layer.observer.PrepareLayerParam;
import com.autonavi.gbl.layer.observer.PrepareLayerStyleInner;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RouteLayerItem;
import com.autonavi.gbl.map.layer.model.ItemStyleInfo;
import com.autonavi.gbl.map.layer.model.LayerIconAnchor;
import com.autonavi.gbl.map.layer.model.LayerIconType;
import com.autonavi.gbl.map.layer.model.LayerItemType;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RouteLayerDrawParam;
import com.autonavi.gbl.map.layer.model.RouteLayerStyle;
import com.autonavi.gbl.util.model.BinaryStream;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.layer.bls.parser.CarLayerParser;
import com.fy.navi.service.adapter.layer.bls.parser.ParserStyleLayerUtils;
import com.fy.navi.service.define.layer.bls.MarkerInfoBean;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.ui.view.SkinTextView;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO 内聚图层纹理配置
 * @Author lvww
 * @date 2024/12/8
 */
public class PrepareLayerStyleInnerImpl extends PrepareLayerStyleInner {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public static final int ROUTE_END_POINT_TEXTURE = 1;

    private PrepareLayerHelper prepareLayerHelper;
    private CarLayerParser carLayerParser;
    private MapView mMapView;
    private MapTypeId mapTypeId;
    private LayerAdapter mLayerAdapter;
    /*** 静态纹理存放空间 **/
    private final List<String> mStaticLayerNameList = new ArrayList<>();
    //自定义静态图片纹理option
    private BitmapFactory.Options staticMarkerOption;
    private Long mGroupLayerId = -1L;

    public PrepareLayerStyleInnerImpl(MapView mapView, PrepareLayerParam customParam, InnerStyleParam param, MapTypeId mapTypeId) {
        super(mapView, customParam, param);
        this.mMapView = mapView;
        this.mapTypeId = mapTypeId;
        this.prepareLayerHelper = new PrepareLayerHelper();
        this.carLayerParser = new CarLayerParser();
        this.staticMarkerOption = new BitmapFactory.Options();
        //不要根据density缩放，原图尺寸放出
        this.staticMarkerOption.inScaled = false;
        this.mLayerAdapter = LayerAdapter.getInstance();
    }

    @SuppressLint("SwitchIntDef")
    @Override
    public int getMarkerId(BaseLayer layer, LayerItem layerItem, ItemStyleInfo itemStyleInfo) {
        int markerId = -1;
        int itemType = layerItem.getItemType();
        int businessType = layerItem.getBusinessType();
        int index = mStaticLayerNameList.indexOf(itemStyleInfo.markerId);
        if (index >= 0) {
            markerId = getStaticMarkerId(index);
        } else {
            String itemMarkerId = itemStyleInfo.markerId;
            String strMarkerInfo = itemStyleInfo.markerInfo;
            Logger.d(TAG, "imageName : " + itemMarkerId + " ; strMarkerInfo : " + strMarkerInfo);
            if (ConvertUtils.equals("-1", itemMarkerId))
                return super.getMarkerId(layer, layerItem, itemStyleInfo);
            markerId = switch (itemType) {
                case LayerItemType.LayerItemNaviCarType ->
                        addStaticMarker(layer, itemMarkerId, strMarkerInfo, staticMarkerOption);
                case LayerItemType.LayerItemPointType -> switch (businessType) {
                    case BizRouteType.BizRouteTypeEndPoint ->
                            addDynamicsMarker(layer, itemMarkerId, strMarkerInfo, businessType);
                    case BizRouteType.BizRouteTypeStartPoint,
                         BizSearchType.BizSearchTypePoiParkRoute,
                         BizSearchType.BizSearchTypePoiParentPoint,
                         BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight ->
                            addStaticMarker(layer, itemMarkerId, strMarkerInfo, staticMarkerOption);
                    default -> super.getMarkerId(layer, layerItem, itemStyleInfo);
                };
                case LayerItemType.LayerItemRasterImageType,
                     LayerItemType.LayerItemVectorCrossType -> switch (businessType) {
                    case BizRoadCrossType.BizRoadCrossTypeRasterImage,
                         BizRoadCrossType.BizRoadCrossTypeVector ->
                            getCustomStaticMarkerId(layer, itemStyleInfo);
                    default -> super.getMarkerId(layer, layerItem, itemStyleInfo);
                };
                default -> super.getMarkerId(layer, layerItem, itemStyleInfo);
            };
        }
        Logger.i(TAG, "itemType: " + itemType + " ; businessType :" + businessType + "; markerId -> " + markerId);
        return markerId;
    }

    @Override
    public int get3DModelId(BaseLayer layer, LayerItem item, String str3DModelId) {
        int modelId = -1;
        if (ConvertUtils.isEmpty(carLayerParser) || ConvertUtils.isEmpty(mMapView) || ConvertUtils.isEmpty(str3DModelId))
            modelId = super.get3DModelId(layer, item, str3DModelId);
        else modelId = carLayerParser.get3DCarModelMarkerId(mMapView, str3DModelId);
        Logger.d("str3DModelId: " + str3DModelId, "get3DCarModelMarkerId: " + modelId);
        return modelId;
    }

    @SuppressLint("SwitchIntDef")
    @Override
    public String getLayerStyle(BaseLayer layer, LayerItem layerItem, boolean forJava) {
        String strStyleJson = "EMPTY";
        int itemType = layerItem.getItemType();
        int businessType = layerItem.getBusinessType();
        Logger.i(TAG, "itemType -> " + itemType, "businessType -> " + businessType);
        try {
            strStyleJson = switch (itemType) {
                case LayerItemType.LayerItemNaviCarType ->
                        carLayerParser.getCarLayer(layerItem.getInfo(), businessType);
                case LayerItemType.LayerItemPointType ->
                        prepareLayerHelper.getLayerItemPointType(businessType, layerItem);
                case LayerItemType.LayerItem3DModelType ->
                        carLayerParser.get3DCarLayer(businessType);
                case LayerItemType.LayerItemRasterImageType,
                     LayerItemType.LayerItemVectorCrossType ->
                        prepareLayerHelper.getLayerItemCrossType(businessType);
                default -> super.getLayerStyle(layer, layerItem, forJava);
            };
        } catch (Exception exception) {
            Logger.i(TAG, "纹理资源加载失败", exception.toString());
        }
        if (ConvertUtils.isEmpty(strStyleJson))
            strStyleJson = super.getLayerStyle(layer, layerItem, forJava);
        Logger.i(TAG, "json -> " + strStyleJson);
        return strStyleJson;
    }

    /**
     * @return bool        true:成功 false:失败
     * @brief 路线图元样式回调接口
     * @param[in] pLayer         图元所在图层
     * @param[in] pItem          需要更新样式的具体图元，通过GetBusinessType返回值判断具体图层
     * @param[in] style          返回的样式数据结果
     * @note thread：main
     */
    @Override
    public boolean getRouteLayerStyle(BaseLayer baseLayer, LayerItem layerItem, RouteLayerStyle routeLayerStyle) {
        Logger.i(TAG, "get route layer style");
        if (layerItem == null || baseLayer == null || routeLayerStyle == null) return false;

        int itemType = layerItem.getItemType();
        int businessType = layerItem.getBusinessType();
        Logger.d(TAG, "getLayerStyle: route itemType = " + itemType + ",businessType = " + businessType);

        RouteLayerItem routeLayerItem = (RouteLayerItem) layerItem;
        RouteLayerDrawParam routeDrawParam = routeLayerItem.getRouteDrawParam();

        return super.getRouteLayerStyle(baseLayer, layerItem, routeLayerStyle);
    }

    @Override
    public boolean isRouteCacheStyleEnabled() {
        return true;
    }

    @Override
    public boolean isRouteStyleNightMode() {
        return false;
    }

    /**
     * @return markerId
     * @brief 获取静态markerId
     */
    private int getStaticMarkerId(int index) {
        return 0x660000 + index;
    }

    /**
     * @return markerId
     * @brief 获取动态markerId
     */
    private int getDynamicsMarkerId(int index) {
        return 0x660000 + 0x60000 + index;
    }

    /**
     * 创建MarkerID.
     *
     * @param pLayer        所在的图层
     * @param name     图元Item ID
     * @param strMarkerInfo 图元marker信息
     * @param businessType       图元列别
     * @return 新的MarkerID
     */
    private synchronized int addDynamicsMarker(BaseLayer pLayer, String name, String strMarkerInfo, int businessType) {
        int markerId = -1;
        if (name == null || name.isEmpty()) {
            Logger.d(TAG, "addDynamicsMarker : " + pLayer.getName() + ", 资源图片未配置 :" + name);
            return -1;
        }

        LayerTexture layerTexture = new LayerTexture();
        layerTexture.name = name;
        setLayerTexture(strMarkerInfo, layerTexture);
        View view = null;
        switch (businessType) {
            case BizRouteType.BizRouteTypeEndPoint:
                LayoutInflater inflater = LayoutInflater.from(AppContext.getInstance().getMContext());
                view = inflater.inflate(R.layout.route_type_end, null);
                SkinTextView textView = view.findViewById(R.id.tv_route_end);
                textView.setText(AppContext.getInstance().getMContext().getString(R.string.arrival_time, mLayerAdapter.getCurrentRouteTime(mapTypeId)));
                markerId = getDynamicsMarkerId(ROUTE_END_POINT_TEXTURE);
                break;
            default:
                break;
        }
        if (view == null) {
            Logger.d(TAG, "addDynamicsMarker : " + pLayer.getName() + " , 资源获取失败 :" + name);
            return -1;
        }

        Bitmap dynamicBitmap = createBitmapFromView(view);
        if (dynamicBitmap == null) {
            return -1;
        }
        ByteBuffer dataBuffer = ByteBuffer.allocate(dynamicBitmap.getByteCount());
        dynamicBitmap.copyPixelsToBuffer(dataBuffer);

        layerTexture.dataBuff = new BinaryStream(dataBuffer.array());
        layerTexture.width = dynamicBitmap.getWidth();
        layerTexture.height = dynamicBitmap.getHeight();
        layerTexture.iconType = LayerIconTypeBMP;
        layerTexture.resID = markerId;

        boolean isAddSuccess = pLayer.getMapView().addLayerTexture(layerTexture);
        if (isAddSuccess) {
            Logger.d(TAG, "addDynamicsMarker: 添加纹理成功 = " + name);
        } else {
            Logger.d(TAG, "addDynamicsMarker: 添加纹理失败 = " + name);
            markerId = -1;
        }
        return markerId;
    }

    /**
     * 创建MarkerID.
     *
     * @param pLayer        所在的图层
     * @param imageName     图元Item ID
     * @param strMarkerInfo 图元marker信息
     * @param options       图片
     * @return 新的MarkerID
     */
    private synchronized int addStaticMarker(BaseLayer pLayer, String imageName, String strMarkerInfo, BitmapFactory.Options options) {
        int markerId = -1;
        if (imageName == null || imageName.isEmpty()) {
            Logger.d(TAG, "addStaticMarker : " + pLayer.getName() + ", 资源图片未配置 :" + imageName);
            return markerId;
        }
        LayerTexture layerTexture = new LayerTexture();
        layerTexture.name = imageName;
        setLayerTexture(strMarkerInfo, layerTexture);//设置锚点
        int resID = ResourceUtils.Companion.getInstance().getMipmapId(imageName);
        if (resID == 0) {
            Logger.d(TAG, "addStaticMarker : " + pLayer.getName() + ", 资源图片缺失 :" + imageName);
            return markerId;
        }
        Bitmap bitmap = BitmapFactory.decodeResource(AppContext.getInstance().getMContext().getResources(), resID, options);
        if (bitmap == null) {
            Logger.d(TAG, "addStaticMarker : " + pLayer.getName() + " , 创建新图片纹理失败 :" + imageName);
            return markerId;
        }
        ByteBuffer dataBuffer = ByteBuffer.allocate(bitmap.getByteCount());
        bitmap.copyPixelsToBuffer(dataBuffer);
        layerTexture.dataBuff = new BinaryStream(dataBuffer.array());
        layerTexture.width = bitmap.getWidth();
        layerTexture.height = bitmap.getHeight();
        layerTexture.iconType = LayerIconType.LayerIconTypeBMP;
        markerId = getStaticMarkerId(mStaticLayerNameList.size());
        layerTexture.resID = markerId;
        boolean isAddSuccess = pLayer.getMapView().addLayerTexture(layerTexture);
        if (isAddSuccess) {
            Logger.d(TAG, "addStaticMarker: 添加纹理成功 = " + imageName);
            mStaticLayerNameList.add(imageName);
        } else {
            Logger.d(TAG, "addStaticMarker: 添加纹理失败 = " + imageName);
            markerId = -1;
        }
        return markerId;
    }

    /**
     * @return markerID
     * @brief 对于不存在的纹理通过添加并生成id
     * @param[in] pLayer         图元所在图层
     * @param[in] strMarkerInfo  marker对应纹理的数据参数配置名
     * @param[in] imageName      图元(唯一,用于过滤防止重复创建)
     * @param[in] resId          资源id
     */
    private synchronized int addStaticMarker(BaseLayer pLayer, String strMarkerInfo, String imageName, int resId) {
        int markerId = -1;
        if (pLayer == null || imageName == null || TextUtils.isEmpty(imageName) || resId <= 0) {
            return markerId;
        }
        LayerTexture layerTexture = CreateLayerTexture(resId);
        if (layerTexture == null) {
            return markerId;
        }
        setLayerTexture(strMarkerInfo, layerTexture); //设置锚点
        layerTexture.name = imageName;
        layerTexture.iconType = LayerIconTypeBMP;
        markerId = getStaticMarkerId(mStaticLayerNameList.size());
        layerTexture.resID = markerId;

        boolean isAddSuccess = pLayer.getMapView().addLayerTexture(layerTexture);
        if (!isAddSuccess) {
            Logger.d(TAG, "addStaticMarker: 创建纹理失败 name:" + imageName);
            return -1;
        }
        mStaticLayerNameList.add(imageName);
        Logger.d(TAG, "addStaticMarker: 创建纹理成功 = {?}, width = {?}, height = {?}", imageName, layerTexture.width, layerTexture.height);
        return markerId;
    }

    /**
     * 设置锚点.
     *
     * @param strMarkerInfo 纹理配置文本
     * @param layerTexture  纹理对象
     */
    @SuppressLint("WrongConstant")
    private void setLayerTexture(String strMarkerInfo, LayerTexture layerTexture) {
        MarkerInfoBean markerInfoBean = ParserStyleLayerUtils.getPointMarkerInfo(strMarkerInfo);
        if (markerInfoBean != null) {
            layerTexture.anchorType = markerInfoBean.getAnchor();
            layerTexture.xRatio = markerInfoBean.getX_ratio();
            layerTexture.yRatio = markerInfoBean.getY_ratio();
            layerTexture.isRepeat = markerInfoBean.getRepeat() == 1;
            layerTexture.isGenMipmaps = markerInfoBean.getGen_mipmaps() == 1;
        } else {
            layerTexture.anchorType = LayerIconAnchor.LayerIconAnchorCenter;
            layerTexture.isRepeat = false;
            layerTexture.xRatio = 0;
            layerTexture.yRatio = 0;
            layerTexture.isGenMipmaps = false;
        }
        layerTexture.isPreMulAlpha = true;//纹理是否预乘透明通道,1：预乘；0：未预乘  bitmap Image are loaded with the {@link Bitmap.Config#ARGB_8888} config by default
        Logger.d(TAG, "setLayerTexture() anchorType:" + layerTexture.anchorType + ", xRatio:" + layerTexture.xRatio + ", yRatio:" + layerTexture.yRatio);
    }

    private synchronized int getCustomStaticMarkerId(BaseLayer pLayer, ItemStyleInfo itemStyleInfo) {
        if (pLayer == null || itemStyleInfo == null) {
            Logger.d(TAG, "Get end viaPoint markerId failed.");
            return -1;
        }
        int markerId = -1;

        String strMarkerId = itemStyleInfo.markerId;
        String strMarkerInfo = itemStyleInfo.markerInfo;
        Logger.d(TAG, "CrossImage_tag getCustomStaticMarkerId: strMarkerId = " + strMarkerId + ",strMarkerInfo:" + strMarkerInfo);
        if (strMarkerId == null || TextUtils.isEmpty(strMarkerId)) {
            return markerId;
        }
        markerId = getStaticLayerMarKerId(strMarkerId);
        if (markerId > 0) {
            return markerId;
        }
        int resId = AppContext.getInstance().getMContext().getResources().getIdentifier(strMarkerId, "drawable", AppContext.getInstance().getMContext().getApplicationInfo().packageName);
        return addStaticMarker(pLayer, strMarkerInfo, strMarkerId, resId);
    }

    /**
     * 判断静态图层是否已经创建
     *
     * @return 如果存在返回MarkerId大于0
     */
    private int getStaticLayerMarKerId(String layerName) {
        int index = mStaticLayerNameList.indexOf(layerName);
        if (index >= 0) {
            int markerId = getStaticMarkerId(index);
            Logger.d(TAG, "getStaticLayerMarKerId markerId = " + markerId);
            return markerId;
        }
        return 0;
    }

    private LayerTexture CreateLayerTexture(int resId) {
        Bitmap bitmap = BitmapFactory.decodeResource(AppContext.getInstance().getMContext().getResources(), resId, staticMarkerOption);
        if (bitmap == null) {
            Logger.d(TAG, "CreateLayerTexture failed. res id: " + resId);
            return null;
        }
        ByteBuffer dataBuffer = ByteBuffer.allocate(bitmap.getByteCount());
        bitmap.copyPixelsToBuffer(dataBuffer);

        LayerTexture layerTexture = new LayerTexture();
        layerTexture.dataBuff = new BinaryStream(dataBuffer.array());
        layerTexture.width = bitmap.getWidth();
        layerTexture.height = bitmap.getHeight();
        layerTexture.iconType = LayerIconTypeBMP;
        bitmap.recycle();
        return layerTexture;
    }

    public static Bitmap createBitmapFromView(View view) {
        view.measure(
                View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED),
                View.MeasureSpec.makeMeasureSpec(0, View.MeasureSpec.UNSPECIFIED)
        );
        int width = view.getMeasuredWidth();
        int height = view.getMeasuredHeight();
        if (width == 0 || height == 0) {
            Logger.d(TAG, "createBitmapFromView failed");
            return null;
        }
        Bitmap bitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
        Canvas canvas = new Canvas(bitmap);
        view.layout(0, 0, width, height);
        view.draw(canvas);

        return bitmap;
    }
}