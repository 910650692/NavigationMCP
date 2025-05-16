package com.fy.navi.service.adapter.layer.bls.style;

import static android.view.View.VISIBLE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Rect;
import android.text.SpannableString;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.StyleSpan;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.ViaChargeStationLayerItem;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.layer.bls.bean.RasterImageBean;
import com.fy.navi.service.adapter.layer.bls.bean.VectorCrossBean;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteReplaceChargePoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteViaChargeInfo;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.route.RouteChargeStationDetailInfo;
import com.fy.navi.service.define.route.RouteChargeStationInfo;
import com.fy.navi.service.define.utils.NumberUtils;
import com.google.gson.Gson;

import java.util.ArrayList;
import java.util.concurrent.atomic.AtomicReference;

public class LayerGuideRouteStyleAdapter extends BaseStyleAdapter {

    //路口大图
    private static final String KEY_ROAD_RASTER_CROSS = "road_raster_cross";
    private static final String KEY_ROAD_VECTOR_CROSS = "road_vector_cross";
    private static final String KEY_ROAD_REAL_CROSS = "road_real_cross";
    //起点
    private static final String KEY_ROAD_START_DEFAULT = "road_start_default";
    //终点
    private static final String KEY_ROAD_END_DEFAULT = "road_end_default";
    //终点-剩余续航
    private static final String KEY_ROAD_END_SURPLUS_POWER_POINT = "road_end_surplus_power_point";
    //终点-营业时间
    private static final String KEY_ROAD_END_BUSINESS_HOURS_POINT = "road_end_business_hours_point";
    //能量耗尽点
    private static final String KEY_ROAD_ENERGY_EMPTY = "road_energy_empty";
    //路线补能点
    private static final String KEY_ROAD_VIA_CHARGE_STATION = "road_via_charge_station";
    //路线替换补能点扎标
    private static final String KEY_SEARCH_REPLACE_CHARGE = "road_via_replace_charge";

    private final AtomicReference<Rect> mRect;
    private final Gson mGson;
    private VectorCrossBean mVectorCrossBean;
    private RasterImageBean mRasterImageBean;
    private BizGuideRouteControl mRouteControl;
    //终点扎标数据
    private LayerItemRouteEndPoint mRouteEndPoint = new LayerItemRouteEndPoint();
    //替换补能点数据
    private ArrayList<RouteAlterChargeStationInfo> mReplaceChargeInfos = new ArrayList<>();
    //路线图层总数据
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
            case BizRouteType.BizRouteTypeStartPoint -> {
                Logger.d(TAG, "起点扎标-默认扎标");
                return KEY_ROAD_START_DEFAULT;
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                if (item instanceof RoutePathPointItem) {
                    if (ConvertUtils.isEmpty(mRouteResult) ||
                            ConvertUtils.isEmpty(mRouteResult.getMLayerItemRouteEndPoint()) ||
                            ConvertUtils.isEmpty(mRouteResult.getMLayerItemRouteEndPoint()
                                    .get(mRouteControl.getSelectedPathIndex())) ||
                            mRouteResult.getMLayerItemRouteEndPoint()
                                    .get(mRouteControl.getSelectedPathIndex()).getRestNum() == NumberUtils.NUM_ERROR) {
                        Logger.d(TAG, "终点扎标-默认扎标");
                        return KEY_ROAD_END_DEFAULT;
                    } else {
                        LayerItemRouteEndPoint layerItemRouteEndPoint = mRouteResult.getMLayerItemRouteEndPoint()
                                .get(mRouteControl.getSelectedPathIndex());
                        LayerPointItemType endPointType = layerItemRouteEndPoint.getEndPointType();
                        if (endPointType == LayerPointItemType.ROUTE_POINT_END_OIL ||
                                endPointType == LayerPointItemType.ROUTE_POINT_END_BATTERY) {
                            Logger.d(TAG, "终点扎标-剩余续航");
                            return KEY_ROAD_END_SURPLUS_POWER_POINT;
                        } else if (endPointType == LayerPointItemType.ROUTE_POINT_END_BUSINESS_HOURS) {
                            Logger.d(TAG, "终点扎标-营业时间");
                            return KEY_ROAD_END_BUSINESS_HOURS_POINT;
                        } else if (endPointType == LayerPointItemType.ROUTE_POINT_END) {
                            Logger.d(TAG, "终点扎标-默认扎标");
                            return KEY_ROAD_END_DEFAULT;
                        }
                    }
                } else {
                    Logger.d(TAG, "终点扎标-默认扎标");
                    return KEY_ROAD_END_DEFAULT;
                }
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
            case BizRouteType.BizRouteTypeViaPoint -> {
                if (item instanceof RoutePathPointItem viaPoint) {
                    long pathId = viaPoint.getPathId();
                    if (pathId == LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal()) {
                        Logger.d(TAG, "途经点扎标-自定义扎标");
                        return KEY_SEARCH_REPLACE_CHARGE;
                    }
                    Logger.d(TAG, "途经点扎标-默认扎标");
                    return super.provideLayerItemStyleJson(item);
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
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
                getRouteEndPoint();
                return mRouteEndPoint;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                //补能规划数据
                return getRouteChargeInfo(item);
            }
            case BizRouteType.BizRouteTypeViaPoint -> {
                //替换补能点数据
                return getRouteReplaceChargePoint(item);
            }
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        if (item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            return new IUpdateBitmapViewProcessor<LayerItemRouteEndPoint>() {
                @SuppressLint("StringFormatInvalid")
                @Override
                public void onNormalProcess(View rootView, LayerItemRouteEndPoint data) {
                    if (ConvertUtils.isEmpty(rootView) || ConvertUtils.isEmpty(data)) {
                        Logger.e(TAG, "更新终点扎标样式 data == null");
                        return;
                    }
                    LayerPointItemType endPointType = data.getEndPointType();
                    Logger.d(TAG, "更新终点扎标样式 endPointType " + endPointType);
                    if (endPointType == LayerPointItemType.ROUTE_POINT_END_OIL ||
                            endPointType == LayerPointItemType.ROUTE_POINT_END_BATTERY) {
                        TextView text = rootView.findViewById(R.id.route_end_detail);
                        if (ConvertUtils.isEmpty(data)) {
                            Logger.e(TAG, "更新终点扎标信息 getEndPointInfo is null");
                            return;
                        }
                        Logger.d(TAG, "更新终点扎标信息 data " + data.toString());
                        LayerPointItemType pointType = data.getEndPointType();
                        int restNum = data.getRestNum();
                        String string = "";
                        switch (pointType) {
                            case ROUTE_POINT_END_BATTERY -> {
                                string = rootView.getContext().getString(R.string.layer_route_end_pop_detail, rootView.getContext().getString(R.string.layer_route_end_pop_type_battery), restNum);
                            }
                            case ROUTE_POINT_END_OIL -> {
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
                    } else if (endPointType == LayerPointItemType.ROUTE_POINT_END_BUSINESS_HOURS) {
                        Logger.d(TAG, "终点扎标-营业时间 data " + data.toString());
                        TextView textView = rootView.findViewById(R.id.route_end_business_hours_text);
                        textView.setText(data.getBusinessHours());
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
                        int chargeTime = chargeStationInfo.getMChargeTime() / 60;
                        int intervalDistance = chargeStationInfo.getMInterval() / 1000;
                        Context rootViewContext = rootView.getContext();
                        String timeStr = rootViewContext.getString(R.string.layer_route_via_charge_time, chargeTime);
                        time.setText(timeStr);

                        String firstString = rootViewContext.getString(R.string.layer_route_via_charge_distance_first);
                        String behindString = rootViewContext.getString(R.string.layer_route_via_charge_distance_behind);
                        String distanceStringTitle = data.getIndex() == 0 ? firstString : behindString;
                        String distanceString = rootViewContext.getString(R.string.layer_route_via_charge_distance, distanceStringTitle, intervalDistance);
                        distance.setText(distanceString);
                    }
                }
            };
            //替换补能点扎标
        } else if (item.getBusinessType() == BizRouteType.BizRouteTypeViaPoint) {
            return new IUpdateBitmapViewProcessor<LayerItemRouteReplaceChargePoint>() {
                @Override
                public void onNormalProcess(View rootView, LayerItemRouteReplaceChargePoint data) {
                    Logger.d(TAG, "替换补能充电站扎标");
                    if (ConvertUtils.isEmpty(rootView)) {
                        Logger.e(TAG, "替换补能充电站扎标 rootView == null");
                        return;
                    }
                    Context context = rootView.getContext();
                    TextView positionView = rootView.findViewById(R.id.search_replace_charge_position);
                    LinearLayout wholeLinearLayout = rootView.findViewById(R.id.search_replace_charge_whole_linear);
                    LinearLayout countLinearLayout = rootView.findViewById(R.id.search_replace_charge_count_linear);
                    boolean fastVisible = false;
                    boolean slowVisible = false;
                    boolean priceVisible = false;
                    TextView fastTextView = rootView.findViewById(R.id.search_replace_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_replace_charge_slow);
                    TextView priceTextView = rootView.findViewById(R.id.search_replace_charge_price);
                    int index = data.getIndex();
                    Logger.d(TAG, "替换补能充电站扎标 index " + index);
                    positionView.setText(String.valueOf(index + 1));
                    RouteAlterChargeStationInfo info = data.getInfo();
                    String fastNumber = info.getMFastPlugInfo().getMTotalNumber();
                    String slowNumber = info.getMSlowPlugInfo().getMTotalNumber();
                    String priceValue = info.getMPriceInfo().getMLowestPriceValue();
                    String priceUnit = info.getMPriceInfo().getMLowestPriceUnit();
                    if (!ConvertUtils.isEmpty(fastNumber)) {
                        String fastString = context.getString(R.string.layer_route_replace_charge_fast, fastNumber);
                        fastTextView.setText(fastString);
                        fastVisible = true;
                    }
                    if (!ConvertUtils.isEmpty(slowNumber)) {
                        String slowString = context.getString(R.string.layer_route_replace_charge_slow, slowNumber);
                        slowTextView.setText(slowString);
                        slowVisible = true;
                    }
                    if (!ConvertUtils.isEmpty(priceValue) && !ConvertUtils.isEmpty(priceUnit)) {
                        priceTextView.setText(context.getString(R.string.layer_route_replace_charge_price, priceValue, priceUnit));
                        priceVisible = true;
                    }
                    if (fastVisible || slowVisible || priceVisible) {
                        wholeLinearLayout.setVisibility(VISIBLE);
                    }
                    if (fastVisible || slowVisible) {
                        countLinearLayout.setVisibility(VISIBLE);
                    }
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }

    // 更新路线补能替换点扎标数据
    public void updateRouteReplaceChargeInfo(ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        if (ConvertUtils.isEmpty(chargeStationInfos)) {
            Logger.e(TAG, "updateRouteReplaceChargeInfo chargeStationInfos is Empty");
            return;
        }
        Logger.d(TAG, "updateRouteReplaceChargeInfo chargeStationInfos " + chargeStationInfos.size());
        mReplaceChargeInfos.clear();
        mReplaceChargeInfos = chargeStationInfos;
    }

    // 更新路线图层数据
    public void updateRouteResult(RequestRouteResult routeResult) {
        if (ConvertUtils.isEmpty(routeResult)) {
            Logger.e(TAG, "updateRouteResult routeResult == null");
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
            mRouteEndPoint.setBusinessHours(result.getBusinessHours());
            mRouteEndPoint.setEndPointType(result.getEndPointType());
            mRouteControl.updateStyle(BizRouteType.BizRouteTypeEndPoint);
        }
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(Rect rect) {
        Logger.d(TAG, "updateRoadCrossRect");
        mRect.set(rect);
    }

    //转换替换补能点扎标数据
    private LayerItemRouteReplaceChargePoint getRouteReplaceChargePoint(LayerItem item) {
        LayerItemRouteReplaceChargePoint chargePoint = new LayerItemRouteReplaceChargePoint();
        String id = item.getID();
        int index = Integer.parseInt(id);
        if (index >= 0 && index < mReplaceChargeInfos.size()) {
            RouteAlterChargeStationInfo chargeStationInfo = mReplaceChargeInfos.get(index);
            chargePoint.setInfo(chargeStationInfo);
            chargePoint.setIndex(index);
        }
        Logger.d(TAG, "getRouteReplaceChargePoint index " + index +
                " mReplaceChargeInfos.size " + mReplaceChargeInfos.size());
        return chargePoint;
    }

    //转换终点扎标数据
    private void getRouteEndPoint() {
        mRouteEndPoint = mRouteResult.getMLayerItemRouteEndPoint().get(mRouteControl.getSelectedPathIndex());
        Logger.d(TAG, "getRouteEndPoint type " + mRouteEndPoint.getEndPointType() + " num " +
                mRouteEndPoint.getRestNum() + " string " + mRouteEndPoint.getBusinessHours());
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

    @Override
    public String refreshOldJsonValue(LayerItem item, String oldJson) {
        switch (item.getBusinessType()) {
            case BizRoadCrossType.BizRoadCrossTypeVector -> {
                Logger.d(TAG, "2D矢量路口大图图层");
                return updateVectorCross(oldJson);
            }
            case BizRoadCrossType.BizRoadCrossTypeRasterImage -> {
                Logger.d(TAG, "栅格路口大图图层");
                return updateRasterCross(oldJson);
            }
        }
        return super.refreshOldJsonValue(item, oldJson);
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
