package com.fy.navi.service.adapter.layer.bls.style;

import static com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint.LayerRouteEndPointType.LAYER_ROUTE_END_TYPE_BATTERY;
import static com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint.LayerRouteEndPointType.LAYER_ROUTE_END_TYPE_OIL;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.StyleSpan;
import android.view.View;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.ViaChargeStationLayerItem;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.layer.bls.bean.RasterImageBean;
import com.fy.navi.service.adapter.layer.bls.bean.VectorCrossBean;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteViaChargeInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteChargeStationInfo;
import com.fy.navi.service.define.utils.NumberUtils;
import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicReference;

public class LayerGuideRouteStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_ROAD_RASTER_CROSS = "road_raster_cross";
    private static final String KEY_ROAD_VECTOR_CROSS = "road_vector_cross";
    private static final String KEY_ROAD_REAL_CROSS = "road_real_cross";
    private static final String KEY_ROAD_END_POINT = "road_end_point";
    private static final String KEY_ROAD_ENERGY_EMPTY = "road_energy_empty";
    private static final String KEY_ROAD_VIA_CHARGE_STATION = "road_via_charge_station";
    private final AtomicReference<Rect> mRect;
    private final Gson mGson;
    private VectorCrossBean mVectorCrossBean;
    private RasterImageBean mRasterImageBean;
    private BizGuideRouteControl mRouteControl;
    private LayerItemRouteEndPoint mRouteEndPoint = new LayerItemRouteEndPoint();
    private static RequestRouteResult mRouteResult = new RequestRouteResult();

    public LayerGuideRouteStyleAdapter(int engineID, BizRoadCrossControl bizRoadCrossControl, BizGuideRouteControl bizGuideRouteControl, BizRoadFacilityControl roadFacilityControl, BizLabelControl bizLabelControl) {
        super(engineID);
        this.mRouteControl = bizGuideRouteControl;
        mGson = new Gson();
        mRect = new AtomicReference<>();
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson BusinessType" + item.getBusinessType());
        switch (item.getBusinessType()) {
            case BizRoadCrossType.BizRoadCrossTypeVector -> {
                Logger.d(TAG, "2D矢量路口大图图层");
                return KEY_ROAD_VECTOR_CROSS;
            }
            case BizRoadCrossType.BizRoadCrossTypeRasterImage -> {
                Logger.d(TAG, "栅格路口大图图层");
                return KEY_ROAD_RASTER_CROSS;
            }
            case BizRoadCrossType.BizRoadCrossTypeRealCity -> {
                Logger.d(TAG, "3D精品大图图层");
                return KEY_ROAD_REAL_CROSS;
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                //等待算路提供数据
                return super.provideLayerItemStyleJson(item);
//                if (ConvertUtils.isEmpty(mRouteResult) ||
//                    ConvertUtils.isEmpty(mRouteResult.getEndPointInfo())) {
//                    Logger.d(TAG, "默认终点扎标 ");
//                    return super.provideLayerItemStyleJson(item);
//                }
//                if (item instanceof RoutePathPointItem) {
//                    LayerItemRoutePoints.LayerRouteEndPointType endPointType =
//                            mRouteResult.getEndPointInfo().getEndPointType();
//                    if (endPointType == LAYER_ROUTE_END_TYPE_BATTERY ||
//                            endPointType == LAYER_ROUTE_END_TYPE_OIL) {
//                        Logger.d(TAG, "剩余油量/电量 终点扎标 " + mRouteResult.toString());
//                        return KEY_ROAD_END_POINT;
//                    }
//                }
            }
            case BizRouteType.BizRouteTypeEnergyEmptyPoint -> {
                Logger.d(TAG, "能量耗尽点");
                return KEY_ROAD_ENERGY_EMPTY;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                if (ConvertUtils.isEmpty(mRouteResult) ||
                    ConvertUtils.isEmpty(mRouteResult.getMRouteChargeStationParam()) ||
                    ConvertUtils.isEmpty(mRouteResult.getMRouteChargeStationParam().getMRouteChargeStationInfos())) {
                    Logger.d(TAG, "默认补能规划扎标");
                    return super.provideLayerItemStyleJson(item);
                }
                Logger.d(TAG, "自定义补能规划扎标");
                return KEY_ROAD_VIA_CHARGE_STATION;
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    // 更新路线图层数据
    public void updateRouteResult(RequestRouteResult routeResult) {
        if (ConvertUtils.isEmpty(routeResult)) {
            Logger.e(TAG, "updateRouteViaChargeInfo routeChargeStationInfos == null");
            return;
        }
        Logger.d(TAG, "updateRouteResult");
        mRouteResult = routeResult;
    }

    // 更新终点扎标数据
    public void updateRoutePoints(LayerItemRouteEndPoint result) {
        if (ConvertUtils.isEmpty(result)) {
            Logger.e(TAG, "updateRoutePoints result == null");
            return;
        }
        Logger.d(TAG, "updateRoutePoints result " + result.toString());
        if (mRouteControl != null) {
            Logger.d(TAG, "updateRoutePoints");
            mRouteEndPoint.setRestNum(result.getRestNum());
            mRouteControl.updateStyle(BizRouteType.BizRouteTypeEndPoint);
        }
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(Rect rect) {
        Logger.d(TAG, "updateRoadCrossRect");
        mRect.set(rect);
    }

    @Override
    public boolean isNeedRefreshJsonValue(LayerItem item) {
        Logger.d(TAG, "isNeedRefreshJsonValue");
        boolean needRefresh = false;
        if (item.getBusinessType() == BizRoadCrossType.BizRoadCrossTypeVector ||
            item.getBusinessType() == BizRoadCrossType.BizRoadCrossTypeRasterImage) {
            needRefresh = true;
        }
        return needRefresh;
    }

    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizRouteType.BizRouteTypeEndPoint -> {
                //终点扎标数据
                return mRouteEndPoint;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                //补能规划数据
                return getRouteChargeInfo(item);
            }
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public boolean isNeedReCreate(LayerItem item) {
        if (mRouteControl != null && item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            return true;
        } else if (item.getBusinessType() == BizRouteType.BizRouteTypeViaChargeStationPoint) {
            return true;
        }
        return super.isNeedReCreate(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        // 终点剩余油量/电量扎标
        if (item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            return new IUpdateBitmapViewProcessor<LayerItemRouteEndPoint>() {
                @SuppressLint("StringFormatInvalid")
                @Override
                public void onNormalProcess(View rootView, LayerItemRouteEndPoint data) {
                    Logger.d(TAG, "更新终点扎标信息");
                    if (rootView != null && data != null) {
                        TextView text = rootView.findViewById(R.id.route_end_detail);
                        if (ConvertUtils.isEmpty(data)) {
                            Logger.e(TAG, "更新终点扎标信息 getEndPointInfo is null");
                            return;
                        }
                        Logger.d(TAG, "更新终点扎标信息 data " + data.toString());
                        LayerItemRouteEndPoint.LayerRouteEndPointType pointType = data.getEndPointType();
                        int restNum = data.getRestNum();
                        String string = "";
                        switch (pointType) {
                            case LAYER_ROUTE_END_TYPE_BATTERY -> {
                                string = rootView.getContext().getString(R.string.layer_route_end_pop_detail, rootView.getContext().getString(R.string.layer_route_end_pop_type_battery), restNum);
                            }
                            case LAYER_ROUTE_END_TYPE_OIL -> {
                                string = rootView.getContext().getString(R.string.layer_route_end_pop_detail, rootView.getContext().getString(R.string.layer_route_end_pop_type_oil), restNum);
                            }
                        }
                        SpannableString spannableString = new SpannableString(string);
                        // 找到需要加粗的部分的起始和结束位置
                        int startIndex = string.indexOf(rootView.getContext().getString(R.string.layer_route_end_pop_default)) + 2;
                        int endIndex = string.length() - 1;
                        // 设置加粗样式
                        spannableString.setSpan(new StyleSpan(android.graphics.Typeface.BOLD), startIndex, endIndex, Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
                        text.setText(spannableString);
                    }
                }
            };
            // 补能规划扎标
        } else if (item.getBusinessType() == BizRouteType.BizRouteTypeViaChargeStationPoint) {
            return new IUpdateBitmapViewProcessor<LayerItemRouteViaChargeInfo>() {
                @Override
                public void onNormalProcess(View rootView, LayerItemRouteViaChargeInfo data) {
                    Logger.d(TAG, "更新补能规划扎标信息");
                    TextView position = rootView.findViewById(R.id.route_via_charge_position);
                    TextView time = rootView.findViewById(R.id.route_via_charge_time);
                    TextView distance = rootView.findViewById(R.id.route_via_charge_distance);
                    if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getMRouteChargeStationInfo())) {
                        Logger.e(TAG, "更新补能规划扎标信息 getMRouteChargeStationInfos is null");
                        return;
                    }
                    Logger.d(TAG, "更新补能规划扎标信息 " + data.getMRouteChargeStationInfo().toString());
                    position.setText(String.valueOf(data.getIndex() + 1));
                    RouteChargeStationDetailInfo chargeStationInfo = data.getMRouteChargeStationInfo();

                    if (!ConvertUtils.isEmpty(chargeStationInfo)) {
                        int chargeTime = chargeStationInfo.getMChargeTime();
                        //等待算路提供剩余距离
//                                int chargeDistance = routeLayerViaChargeInfo.getDistance();
                        Context rootViewContext = rootView.getContext();
                        String timeStr = rootViewContext.getString(R.string.layer_route_via_charge_time, chargeTime);
                        time.setText(timeStr);

//                                String preDistanceStr = (index == 0) ?
//                                        rootViewContext.getString(R.string.layer_route_via_charge_distance_first) :
//                                        rootViewContext.getString(R.string.layer_route_via_charge_distance_behind);
//                                String distanceStr = rootViewContext.getString(R.string.layer_route_via_charge_distance, preDistanceStr, chargeDistance);
//                                distance.setText(distanceStr);
                    }
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }

    //转换终点扎标数据
    private LayerItemRouteEndPoint getRouteEndPoint() {
        //等待算路提供数据
        LayerItemRouteEndPoint endPoint = new LayerItemRouteEndPoint();
        return endPoint;
    }

    //转换补能规划数据
    private LayerItemRouteViaChargeInfo getRouteChargeInfo(LayerItem item) {
        Logger.d(TAG, "getRouteChargeInfo start");
        LayerItemRouteViaChargeInfo chargeInfo = new LayerItemRouteViaChargeInfo();
        if (item instanceof ViaChargeStationLayerItem chargeStationLayerItem) {
            String id = chargeStationLayerItem.getID();
            int index = Integer.parseInt(id);
            ArrayList<RouteChargeStationInfo> chargeStationInfos = mRouteResult.getMRouteChargeStationParam().getMRouteChargeStationInfos();
            if (ConvertUtils.isEmpty(mRouteControl)) {
                Logger.e(TAG, "getRouteChargeInfo mRouteControl == null");
                return chargeInfo;
            }
            int selectedPathIndex = mRouteControl.getSelectedPathIndex();
            Logger.d(TAG, "getRouteChargeInfo selectedPathIndex " + selectedPathIndex);
            if (selectedPathIndex >= NumberUtils.NUM_0 && selectedPathIndex < chargeStationInfos.size()) {
                RouteChargeStationInfo routeChargeStationInfo = chargeStationInfos.get(selectedPathIndex);
                if (ConvertUtils.isEmpty(routeChargeStationInfo)) {
                    Logger.e(TAG, "getRouteChargeInfo routeChargeStationInfo == null");
                    return chargeInfo;
                }
                ArrayList<RouteChargeStationDetailInfo> chargeStationDetailInfoArrayList = routeChargeStationInfo.getMRouteChargeStationDetailInfo();
                if (ConvertUtils.isEmpty(chargeStationDetailInfoArrayList)) {
                    Logger.e(TAG, "getRouteChargeInfo chargeStationDetailInfoArrayList is Empty");
                    return chargeInfo;
                }
                if (index >= NumberUtils.NUM_0 && index < chargeStationDetailInfoArrayList.size()) {
                    RouteChargeStationDetailInfo routeChargeStationDetailInfo = chargeStationDetailInfoArrayList.get(index);
                    if (ConvertUtils.isEmpty(routeChargeStationDetailInfo)) {
                        Logger.e(TAG, "getRouteChargeInfo routeChargeStationDetailInfo == null");
                        return chargeInfo;
                    }
                    chargeInfo.setIndex(index);
                    chargeInfo.setMRouteChargeStationInfo(routeChargeStationDetailInfo);
                }
            } else {
                Logger.e(TAG, "getRouteChargeInfo 下标越界");
                return chargeInfo;
            }
        }
        Logger.d(TAG, "getRouteChargeInfo chargeInfo " + chargeInfo.toString());
        return chargeInfo;
    }

    // 更新路口大图json
    @Override
    public String provideUpdateStyleJson(LayerItem item, String oldJson) {
        String updateJson = "";
        switch (item.getBusinessType()) {
            case BizRoadCrossType.BizRoadCrossTypeVector -> {
                Logger.d(TAG, "2D矢量路口大图图层");
                updateJson = updateVectorCross(oldJson);
            }
            case BizRoadCrossType.BizRoadCrossTypeRasterImage -> {
                Logger.d(TAG, "栅格路口大图图层");
                updateJson = updateRasterCross(oldJson);
            }
        }
        if (TextUtils.isEmpty(updateJson)) {
            Logger.e(TAG, "provideUpdateStyleJson updateJson is null");
            updateJson = oldJson;
        }
        Logger.d(TAG, "provideUpdateStyleJson Success businessType " + item.getBusinessType());
        return updateJson;
    }

    // 更新矢量图
    private String updateVectorCross(String oldJson) {
        if (TextUtils.isEmpty(oldJson)) {
            Logger.e(TAG, "updateVectorCross oldJson is null");
            return "";
        }
        if (ConvertUtils.isEmpty(mRect) || ConvertUtils.isNull(mRect.get())) {
            return oldJson;
        }
        String updateJson = "";
        //根据屏幕适配矢量路口大图宽高
        if (ConvertUtils.isEmpty(mVectorCrossBean)) {
            mVectorCrossBean = mGson.fromJson(oldJson, VectorCrossBean.class);
        }
        if (mVectorCrossBean != null) {
            VectorCrossBean.VectorCrossLayerStyleBean vectorCrossLayerStyle = mVectorCrossBean.getVector_cross_layer_style();
            if (ConvertUtils.isEmpty(vectorCrossLayerStyle)) {
                Logger.e(TAG, "updateVectorCross vectorCrossLayerStyle is Empty");
                return oldJson;
            }
            VectorCrossBean.VectorCrossLayerStyleBean.VectorCrossAttrBean vectorCrossAttr = vectorCrossLayerStyle.getVector_cross_attr();
            if (ConvertUtils.isEmpty(vectorCrossAttr)) {
                Logger.e(TAG, "updateVectorCross vectorCrossAttr is Empty");
                return oldJson;
            }
            VectorCrossBean.VectorCrossLayerStyleBean.VectorCrossAttrBean.RectBean rectBean = vectorCrossAttr.getRect();
            if (ConvertUtils.isEmpty(rectBean)) {
                Logger.e(TAG, "updateVectorCross rectBean is Empty");
                return oldJson;
            }
            if (ConvertUtils.isNull(mRect) || ConvertUtils.isNull(mRect.get())) {
                return oldJson;
            }
            rectBean.setX_min(mRect.get().left);
            rectBean.setY_min(mRect.get().top);
            rectBean.setX_max(mRect.get().right);
            rectBean.setY_max(mRect.get().bottom);
        } else {
            Logger.e(TAG, "updateVectorCross VectorCrossBean 转换失败");
            updateJson = oldJson;
        }
        updateJson = mGson.toJson(mVectorCrossBean);
        Logger.d(TAG, "updateVectorCross rect: " + mRect);
        return updateJson;
    }

    // 更新栅格图
    private String updateRasterCross(String oldJson) {
        if (TextUtils.isEmpty(oldJson)) {
            Logger.e(TAG, "updateRasterCross oldJson is null");
            return "";
        }
        if (ConvertUtils.isEmpty(mRect)) {
            return oldJson;
        }
        String updateJson = "";
        //根据屏幕适配栅格大图宽高
        if (ConvertUtils.isEmpty(mRasterImageBean)) {
            mRasterImageBean = mGson.fromJson(oldJson, RasterImageBean.class);
        }
        if (mRasterImageBean != null) {
            RasterImageBean.RasterImageLayerItemStyleBean rasterImageLayerItemStyleBean =
                    mRasterImageBean.getRaster_image_layer_item_style();
            if (ConvertUtils.isEmpty(rasterImageLayerItemStyleBean)) {
                Logger.e(TAG, "updateRasterCross rasterImageLayerItemStyleBean is Empty");
                return oldJson;
            }
            if (ConvertUtils.isEmpty(mRect) || ConvertUtils.isNull(mRect.get())) {
                Logger.e(TAG, "mRect is Empty");
                return oldJson;
            }
            rasterImageLayerItemStyleBean.setWinx(mRect.get().left);
            rasterImageLayerItemStyleBean.setWiny(mRect.get().top);
            rasterImageLayerItemStyleBean.setWidth(mRect.get().width());
            rasterImageLayerItemStyleBean.setHeight(mRect.get().height());
        } else {
            Logger.e(TAG, "updateRasterCross RasterImageBean 转换失败");
            updateJson = oldJson;
        }
        updateJson = mGson.toJson(mRasterImageBean);
        Logger.d(TAG, "updateRasterCross rect: " + mRect);
        return updateJson;
    }
}
