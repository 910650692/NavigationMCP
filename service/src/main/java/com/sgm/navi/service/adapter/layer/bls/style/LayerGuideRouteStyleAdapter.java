package com.sgm.navi.service.adapter.layer.bls.style;

import static android.view.View.GONE;
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
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.ViaChargeStationLayerItem;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRoadFacilityType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.layer.bls.bean.RasterImageBean;
import com.sgm.navi.service.adapter.layer.bls.bean.VectorCrossBean;
import com.sgm.navi.service.define.layer.refix.LayerItemData;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteReplaceChargePoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteViaChargeInfo;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteChargeStationDetailInfo;
import com.sgm.navi.service.define.route.RouteChargeStationNumberInfo;
import com.sgm.navi.service.define.route.RouteChargeStationParam;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.route.RouteSupplementInfo;
import com.sgm.navi.service.define.route.RouteSupplementParams;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.utils.GasCarTipManager;

import java.util.ArrayList;
import java.util.List;
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
    //能量耗尽点
    private static final String KEY_ROAD_ENERGY_EMPTY = "road_energy_empty";
    //路线补能点
    private static final String KEY_ROAD_VIA_CHARGE_STATION = "road_via_charge_station";
    //路线替换补能点扎标
    private static final String KEY_SEARCH_REPLACE_CHARGE = "road_via_replace_charge";
    //途经点是充电站的扎标
    private static final String KEY_ROAD_ROUTE_VIA_CHARGE_STATION = "road_route_via_charge_station";

    private final AtomicReference<Rect> mRect = new AtomicReference<>();
    private VectorCrossBean mVectorCrossBean;
    private RasterImageBean mRasterImageBean;
    private final BizGuideRouteControl mRouteControl;
    //终点扎标数据
    private final LayerItemRouteEndPoint mRouteEndPoint = new LayerItemRouteEndPoint();
    //替换补能点数据
    private ArrayList<RouteAlterChargeStationInfo> mReplaceChargeInfos = new ArrayList<>();
    private int mViaCount = 0;
    //路线图层总数据
    private RequestRouteResult mRouteResult = new RequestRouteResult();
    //自动添加的补能站
    private RouteChargeStationParam mRouteChargeStation = new RouteChargeStationParam();
    //途径点信息
    private List<PoiInfoEntity> mViaPointList = new ArrayList<>();

    public LayerGuideRouteStyleAdapter(int engineID, BizRoadCrossControl bizRoadCrossControl, BizGuideRouteControl bizGuideRouteControl, BizRoadFacilityControl roadFacilityControl, BizLabelControl bizLabelControl) {
        super(engineID);
        this.mRouteControl = bizGuideRouteControl;
    }

    @Override
    public String provideLayerItemStyleJson(BaseLayer layer, LayerItem item) {
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
                //终点扎标开启图层避让 防止终点扎标被其他图元重叠
                layer.enablePoiFilter(true);
                return KEY_ROAD_END_DEFAULT;
            }
            case BizRouteType.BizRouteTypeEnergyEmptyPoint -> {
                Logger.d(TAG, "能量耗尽点");
                return KEY_ROAD_ENERGY_EMPTY;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                if (ConvertUtils.isEmpty(mRouteChargeStation.getMRouteSupplementParams())) {
                    Logger.d(TAG, "默认补能规划扎标");
                    return super.provideLayerItemStyleJson(layer, item);
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
                    } else if (pathId == LayerPointItemType.ROUTE_POINT_VIA_CHARGE.ordinal()) {
                        Logger.d(TAG, "途经点扎标-充电站");
                        return KEY_ROAD_ROUTE_VIA_CHARGE_STATION;
                    }
                    Logger.d(TAG, "途经点扎标-默认扎标");
                    return super.provideLayerItemStyleJson(layer, item);
                }
            }
            case BizRouteType.BizRouteTypeGuideLabel -> {
                Logger.i(TAG, "多备选路线标签 BizRouteTypeGuideLabel");
                return super.provideLayerItemStyleJson(layer, item);
            }
        }
        return super.provideLayerItemStyleJson(layer, item);
    }

    @Override
    public boolean isNeedRefreshStyleJson(LayerItem item) {
        return item.getBusinessType() == BizRoadCrossType.BizRoadCrossTypeVector ||
                item.getBusinessType() == BizRoadCrossType.BizRoadCrossTypeRasterImage;
    }

    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizRouteType.BizRouteTypeEndPoint -> {
                //终点扎标数据
                setRouteEndPoint();
                return mRouteEndPoint;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                //补能规划数据
                return getRouteChargeInfo(item);
            }
            case BizRouteType.BizRouteTypeViaPoint -> {
                //替换补能点数据
                if (item instanceof RoutePathPointItem viaPoint) {
                    long pathId = viaPoint.getPathId();
                    if (pathId == LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal()) {
                        return getRouteReplaceChargePoint(item);
                    } else if (pathId == LayerPointItemType.ROUTE_POINT_VIA_CHARGE.ordinal()) {
                        return getRouteViaChargePoint(item);
                    } else {
                        return super.provideLayerItemData(item);
                    }
                }
            }
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        return switch (item.getBusinessType()) {
            case BizRouteType.BizRouteTypeEndPoint ->
                    new IUpdateBitmapViewProcessor<LayerItemRouteEndPoint>() {
                        @SuppressLint("StringFormatInvalid")
                        @Override
                        public void onNormalProcess(LayerItem layerItem, View rootView, LayerItemRouteEndPoint data) {
                            if (ConvertUtils.isEmpty(rootView) || ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "更新终点扎标样式 data == null");
                                return;
                            }
                            if (ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "更新终点扎标信息 getEndPointInfo is null");
                                return;
                            }
                            Logger.d(TAG, "更新终点扎标信息 data " + data);
                            final TextView text = rootView.findViewById(R.id.route_end_detail);
                            int restNum = data.getRestNum();
                            if (restNum > 0) {
                                final LayerPointItemType pointType = data.getEndPointType();
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
                                safetySetText(text, string);
                            } else {
                                safetySetText(text, "");
                            }
                            Logger.d(TAG, "终点扎标-营业时间 data " + data.toString());
                            final TextView textView = rootView.findViewById(R.id.route_end_business);
                            safetySetText(textView, data.getBusinessHours());
                        }

                        @Override
                        public void onFocusProcess(LayerItem layerItem, View rootView, LayerItemRouteEndPoint data) {
                            if (ConvertUtils.isEmpty(rootView) || ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "onFocusProcess 更新终点扎标样式 data == null");
                                return;
                            }
                            if (ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "onFocusProcess 更新终点扎标信息 getEndPointInfo is null");
                                return;
                            }
                            Logger.d(TAG, "onFocusProcess 更新终点扎标信息 data " + data);
                            final TextView text = rootView.findViewById(R.id.route_end_detail);
                            int restNum = data.getRestNum();
                            if (restNum > 0) {
                                final LayerPointItemType pointType = data.getEndPointType();
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
                                safetySetText(text, string);
                            } else {
                                safetySetText(text, "");
                            }
                            Logger.d(TAG, "onFocusProcess 终点扎标-营业时间 data " + data.toString());
                            final TextView textView = rootView.findViewById(R.id.route_end_business);
                            safetySetText(textView, data.getBusinessHours());
                        }
                    };
            // 补能规划扎标
            case BizRouteType.BizRouteTypeViaChargeStationPoint ->
                    new IUpdateBitmapViewProcessor<LayerItemRouteViaChargeInfo>() {
                        @Override
                        public void onNormalProcess(LayerItem layerItem, View rootView, LayerItemRouteViaChargeInfo data) {
                            Logger.d(TAG, "更新补能规划扎标信息");
                            TextView position = rootView.findViewById(R.id.route_via_charge_position);
                            TextView time = rootView.findViewById(R.id.route_via_charge_time);
                            TextView distance = rootView.findViewById(R.id.route_via_charge_distance);
                            if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getMRouteChargeStationInfo())) {
                                Logger.e(TAG, "更新补能规划扎标信息 getMRouteChargeStationInfos is null");
                                return;
                            }
                            if (Logger.openLog) {
                                Logger.d(TAG, "更新补能规划扎标信息 ", data.getMRouteChargeStationInfo());
                            }
                            safetySetText(position, String.valueOf(data.getIndex() + 1));
                            RouteChargeStationDetailInfo chargeStationInfo = data.getMRouteChargeStationInfo();

                            if (!ConvertUtils.isEmpty(chargeStationInfo)) {
                                int chargeTime = chargeStationInfo.getMChargeTime() / 60;
                                int intervalDistance = chargeStationInfo.getMInterval() / 1000;
                                Context rootViewContext = rootView.getContext();
                                String timeStr = rootViewContext.getString(R.string.layer_route_via_charge_time, String.valueOf(chargeTime > 0 ? chargeTime : "-"));
                                safetySetText(time, timeStr);

                                String firstString = rootViewContext.getString(R.string.layer_route_via_charge_distance_first);
                                String behindString = rootViewContext.getString(R.string.layer_route_via_charge_distance_behind);
                                String distanceStringTitle = data.getIndex() == 0 ? firstString : behindString;
                                String distanceString = rootViewContext.getString(R.string.layer_route_via_charge_distance, distanceStringTitle, String.valueOf(intervalDistance > 0 ? intervalDistance : "-"));
                                safetySetText(distance, distanceString);
                            }
                        }
                    };
            //替换补能点扎标
            case BizRouteType.BizRouteTypeViaPoint ->
                    new IUpdateBitmapViewProcessor<LayerItemRouteReplaceChargePoint>() {
                        @Override
                        public void onNormalProcess(LayerItem layerItem, View rootView, LayerItemRouteReplaceChargePoint data) {
                            Logger.d(TAG, "替换补能充电站扎标");
                            if (ConvertUtils.isEmpty(rootView)) {
                                Logger.e(TAG, "替换补能充电站扎标 rootView == null");
                                return;
                            }
                            if (ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "the data is empty");
                                return;
                            }

                            if (data.getType() == 1) {
                                setViaChargeStationInfo(rootView, data);
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
                            safetySetText(positionView, String.valueOf(index + 1));
                            RouteAlterChargeStationInfo info = data.getInfo();
                            if (ConvertUtils.isEmpty(info)) {
                                Logger.e(TAG, "替换补能充电站扎标 info == null");
                                return;
                            }
                            if (!ConvertUtils.isEmpty(info.getMFastPlugInfo())) {
                                String fastNumber = info.getMFastPlugInfo().getMTotalNumber();
                                if (!ConvertUtils.isEmpty(fastNumber)) {
                                    String fastString = context.getString(R.string.layer_route_replace_charge_fast, fastNumber);
                                    safetySetText(fastTextView, fastString);
                                    fastVisible = true;
                                }
                            }
                            if (!ConvertUtils.isEmpty(info.getMSlowPlugInfo())) {
                                String slowNumber = info.getMSlowPlugInfo().getMTotalNumber();
                                if (!ConvertUtils.isEmpty(slowNumber)) {
                                    String slowString = context.getString(R.string.layer_route_replace_charge_slow, slowNumber);
                                    safetySetText(slowTextView, slowString);
                                    slowVisible = true;
                                }
                            }
                            if (!ConvertUtils.isEmpty(info.getMPriceInfo())) {
                                String priceValue = info.getMPriceInfo().getMLowestPriceValue();
                                String priceUnit = info.getMPriceInfo().getMLowestPriceUnit();
                                if (!ConvertUtils.isEmpty(priceValue) && !ConvertUtils.isEmpty(priceUnit)) {
                                    safetySetText(priceTextView, context.getString(R.string.layer_route_replace_charge_price, priceValue, priceUnit));
                                    priceVisible = true;
                                }
                            }
                            if (fastVisible || slowVisible || priceVisible) {
                                wholeLinearLayout.setVisibility(VISIBLE);
                            }
                            if (fastVisible || slowVisible) {
                                countLinearLayout.setVisibility(VISIBLE);
                            }
                        }

                        @Override
                        public void onFocusProcess(LayerItem layerItem, View rootView, LayerItemRouteReplaceChargePoint data) {
                            if (ConvertUtils.isEmpty(rootView)) {
                                Logger.e(TAG, "替换补能充电站扎标 rootView == null");
                                return;
                            }
                            if (ConvertUtils.isEmpty(data)) {
                                Logger.e(TAG, "the data is empty");
                                return;
                            }
                            if (data.getType() == 1) {
                                setViaChargeStationInfo(rootView, data);
                            }
                        }
                    };
            default -> super.provideUpdateBitmapViewProcessor(item);
        };
    }

    /**
     * 途经点充电站数据
     *
     * @param rootView
     * @param data
     */
    private void setViaChargeStationInfo(View rootView, LayerItemRouteReplaceChargePoint data) {
        final RouteAlterChargeStationInfo info = data.getInfo();
        if (ConvertUtils.isEmpty(info)) {
            Logger.e(TAG, "info is empty");
            return;
        }
        final TextView fastText = rootView.findViewById(R.id.route_charge_station_fast);
        final TextView slowText = rootView.findViewById(R.id.route_charge_station_slow);
        final Context context = rootView.getContext();
        final String fastTotalNumber = info.getMFastPlugInfo().getMTotalNumber();
        if (TextUtils.isEmpty(fastTotalNumber)) {
            safetySetText(fastText, "");
        } else {
            final String fastString = context.getString(R.string.layer_route_replace_charge_fast, info.getMFastPlugInfo().getMTotalNumber());
            safetySetText(fastText, fastString);
        }
        final String slowTotalNumber = info.getMSlowPlugInfo().getMTotalNumber();
        if (TextUtils.isEmpty(slowTotalNumber)) {
            safetySetText(slowText, "");
        } else {
            final String slowString = context.getString(R.string.layer_route_replace_charge_slow, info.getMSlowPlugInfo().getMTotalNumber());
            safetySetText(slowText, slowString);
        }

    }

    private void safetySetText(TextView textView, String string) {
        if (ConvertUtils.isEmpty(textView)) {
            Logger.e(TAG, "safetySetText textView == null");
            return;
        }
        if (ConvertUtils.isEmpty(string)) {
            Logger.e(TAG, "safetySetText string is Empty");
            textView.setVisibility(GONE);
        }
        textView.setText(string);
    }

    /**
     * 更新途径 点信息
     *
     * @param viaPointList
     */
    public void updateViaPointList(List<PoiInfoEntity> viaPointList) {
        mViaPointList = new ArrayList<>(viaPointList);
    }

    /**
     * 更新算路时补能点信息
     *
     * @param routeChargeStation
     */
    public void updateRouteChargeStation(RouteChargeStationParam routeChargeStation) {
        if (ConvertUtils.isNull(routeChargeStation)) {
            Logger.d(TAG, "routeChargeStation is null");
            return;
        }
        updateChargeStationDistance(routeChargeStation);
        mRouteChargeStation = routeChargeStation;
    }

    /**
     * 更新间距
     *
     * @param routeChargeStation
     */
    private void updateChargeStationDistance(RouteChargeStationParam routeChargeStation) {
        final ArrayList<RouteSupplementParams> routeSupplementParams = routeChargeStation.getMRouteSupplementParams();
        if (!ConvertUtils.isEmpty(routeSupplementParams)) {
            for (RouteSupplementParams mRouteSupplementParam : routeSupplementParams) {
                final ArrayList<RouteSupplementInfo> routeSupplementInfos = mRouteSupplementParam.getMRouteSupplementInfos();
                if (!ConvertUtils.isEmpty(routeSupplementInfos)) {
                    for (int i = routeSupplementInfos.size() - 1; i >= 0; i--) {
                        final RouteSupplementInfo currentRouteInfo = routeSupplementInfos.get(i);
                        if (i == 0) {
                            currentRouteInfo.setMInterval(currentRouteInfo.getMDistance());
                        } else {
                            final RouteSupplementInfo previousRouteInfo = routeSupplementInfos.get(i - 1);
                            currentRouteInfo.setMInterval(currentRouteInfo.getMDistance() - previousRouteInfo.getMDistance());
                        }

                    }
                }
            }
        }
    }

    // 更新路线补能替换点扎标数据
    public void updateRouteReplaceChargeInfo(ArrayList<RouteAlterChargeStationInfo> chargeStationInfos, int viaCount) {
        if (ConvertUtils.isEmpty(chargeStationInfos)) {
            Logger.e(TAG, "updateRouteReplaceChargeInfo chargeStationInfos is Empty");
            return;
        }
        Logger.d(TAG, "updateRouteReplaceChargeInfo chargeStationInfos " + chargeStationInfos.size() + " viaCount " + viaCount);
        mReplaceChargeInfos.clear();
        mReplaceChargeInfos = chargeStationInfos;
        mViaCount = viaCount;
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
//            mRouteControl.updateStyle(BizRouteType.BizRouteTypeEndPoint);
        }
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(Rect rect) {
        Logger.e(TAG, "updateRoadCrossRect");
        mRect.set(rect);
    }

    public Rect getRoadCrossRect() {
        Logger.e(TAG, "getRoadCrossRect ", mRect);
        return mRect.get();
    }

    /**
     * 获取途经点充电站数据
     *
     * @param item
     * @return LayerItemRouteReplaceChargePoint
     */
    private LayerItemRouteReplaceChargePoint getRouteViaChargePoint(LayerItem item) {
        final int index = Integer.parseInt(item.getID());
        final LayerItemRouteReplaceChargePoint chargePoint = new LayerItemRouteReplaceChargePoint();
        chargePoint.setType(1);
        if (index >= 0 && index < mViaPointList.size()) {
            final PoiInfoEntity poiInfoEntity = mViaPointList.get(index);
            if (!ConvertUtils.isEmpty(poiInfoEntity) && !ConvertUtils.isEmpty(poiInfoEntity.getChargeInfoList())) {
                final ChargeInfo chargeInfo = poiInfoEntity.getChargeInfoList().get(0);
                final RouteAlterChargeStationInfo info = new RouteAlterChargeStationInfo();
                final RouteChargeStationNumberInfo fastInfo = new RouteChargeStationNumberInfo();
                final RouteChargeStationNumberInfo slowInfo = new RouteChargeStationNumberInfo();
                if (chargeInfo.getMFastTotal() > 0) {
                    fastInfo.setMTotalNumber(chargeInfo.getMFastFree() + "/" + chargeInfo.getMFastTotal());
                }
                if (chargeInfo.getMSlowTotal() > 0) {
                    slowInfo.setMTotalNumber(chargeInfo.getMSlowFree() + "/" + chargeInfo.getMSlowTotal());
                }
                info.setMFastPlugInfo(fastInfo);
                info.setMSlowPlugInfo(slowInfo);
                chargePoint.setInfo(info);
            }
            chargePoint.setIndex(index);
        }
        return chargePoint;
    }

    //转换替换补能点扎标数据
    private LayerItemRouteReplaceChargePoint getRouteReplaceChargePoint(LayerItem item) {
        LayerItemRouteReplaceChargePoint chargePoint = new LayerItemRouteReplaceChargePoint();
        String id = item.getID();
        int index = Integer.parseInt(id);
        int replaceChargeIndex = index - mViaCount;
        if (index >= 0 && index < mReplaceChargeInfos.size()) {
            RouteAlterChargeStationInfo chargeStationInfo = mReplaceChargeInfos.get(index);
            chargePoint.setInfo(chargeStationInfo);
        }
        chargePoint.setIndex(replaceChargeIndex);
        chargePoint.setType(0);
        Logger.d(TAG, "getRouteReplaceChargePoint index " + index +
                " mReplaceChargeInfos.size " + mReplaceChargeInfos.size() +
                " replaceChargeIndex " + replaceChargeIndex +
                " mViaCount " + mViaCount);
        return chargePoint;
    }

    //转换终点扎标数据
    private void setRouteEndPoint() {
        mRouteEndPoint.setRestNum(-1);
        int selectPathIndex = mRouteControl.getSelectedPathIndex();
        if (ConvertUtils.isNull(mRouteResult)) {
            return;
        }
        List<RouteLineInfo> mRouteLineInfos = mRouteResult.getMRouteLineInfos();
        if (ConvertUtils.isEmpty(mRouteLineInfos)) {
            Logger.v(TAG, "mRouteLineInfos is Empty");
            return;
        }
        int mCarType = CalibrationPackage.getInstance().powerType();
        if (selectPathIndex < mRouteLineInfos.size()) {
            if (mCarType == 0 || mCarType == 2) {
                final int num = GasCarTipManager.getInstance().getRemainGasPercent(ConvertUtils
                        .convertMetersToKilometers(mRouteLineInfos.get(selectPathIndex).getMDistance()));
                Logger.d(TAG, "getRemainGasPercent: " + num);
                mRouteEndPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END_OIL);
                if (num != 0) {
                    mRouteEndPoint.setRestNum(num);
                } else {
                    mRouteEndPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END);
                    mRouteEndPoint.setRestNum(-1);
                }
            } else {
                if (mRouteLineInfos.get(selectPathIndex).isMCanBeArrive()) {
                    mRouteEndPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END_BATTERY);
                    mRouteEndPoint.setRestNum(mRouteLineInfos.get(selectPathIndex).getMRemainPercent());
                } else {
                    mRouteEndPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END);
                    mRouteEndPoint.setRestNum(-1);
                }
            }
        } else {
            mRouteEndPoint.setEndPointType(LayerPointItemType.ROUTE_POINT_END);
            Logger.v(TAG, "selectPathIndex < mRouteLineInfos.size()");
        }
        Logger.d(TAG, "getRouteEndPoint type " + mRouteEndPoint.getEndPointType() + " num " +
                mRouteEndPoint.getRestNum() + " BusinessHours is " + mRouteEndPoint.getBusinessHours());
    }

    //转换补能规划数据
    private LayerItemRouteViaChargeInfo getRouteChargeInfo(LayerItem item) {
        Logger.d(TAG, "getRouteChargeInfo start");
        LayerItemRouteViaChargeInfo chargeInfo = new LayerItemRouteViaChargeInfo();
        if (item instanceof ViaChargeStationLayerItem chargeStationLayerItem) {
            String id = chargeStationLayerItem.getID();
            int index = Integer.parseInt(id);
            ArrayList<RouteSupplementParams> mRouteSupplementParams = mRouteChargeStation.getMRouteSupplementParams();
            if (ConvertUtils.isEmpty(mRouteSupplementParams)) {
                Logger.e(TAG, "RouteSupplement is empty");
                return chargeInfo;
            }
            if (ConvertUtils.isEmpty(mRouteControl)) {
                Logger.e(TAG, "getRouteChargeInfo mRouteControl == null");
                return chargeInfo;
            }
            int selectedPathIndex = mRouteControl.getSelectedPathIndex();
            Logger.d(TAG, "getRouteChargeInfo selectedPathIndex " + selectedPathIndex);
            if (selectedPathIndex >= NumberUtils.NUM_0 && selectedPathIndex < mRouteSupplementParams.size()) {
                RouteSupplementParams routeSupplementParams = mRouteSupplementParams.get(selectedPathIndex);
                if (ConvertUtils.isNull(routeSupplementParams)) {
                    Logger.e(TAG, "getRouteChargeInfo routeSupplementParams == null");
                    return chargeInfo;
                }
                ArrayList<RouteSupplementInfo> mRouteSupplementInfos = routeSupplementParams.getMRouteSupplementInfos();
                if (ConvertUtils.isEmpty(mRouteSupplementInfos)) {
                    Logger.e(TAG, "getRouteChargeInfo mRouteSupplementInfos is Empty");
                    return chargeInfo;
                }
                if (index >= NumberUtils.NUM_0 && index < mRouteSupplementInfos.size()) {
                    final RouteSupplementInfo routeSupplementInfo = mRouteSupplementInfos.get(index);
                    if (ConvertUtils.isEmpty(routeSupplementInfo)) {
                        Logger.e(TAG, "getRouteChargeInfo routeSupplementInfo == null");
                        return chargeInfo;
                    }
                    chargeInfo.setIndex(index);
                    final RouteChargeStationDetailInfo routeChargeStationDetailInfo = new RouteChargeStationDetailInfo();
                    routeChargeStationDetailInfo.setMChargeTime(routeSupplementInfo.getMChargeTime());
                    routeChargeStationDetailInfo.setMInterval(routeSupplementInfo.getMInterval());
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
    public String refreshStyleJson(LayerItem item, String oldJson) {
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
        return super.refreshStyleJson(item, oldJson);
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
            mVectorCrossBean = GsonUtils.fromJson(oldJson, VectorCrossBean.class);
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
        updateJson = GsonUtils.toJson(mVectorCrossBean);
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
            mRasterImageBean = GsonUtils.fromJson(oldJson, RasterImageBean.class);
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
        updateJson = GsonUtils.toJson(mRasterImageBean);
        Logger.d(TAG, "updateRasterCross rect: " + mRect);
        return updateJson;
    }

    @Override
    public List<CustomUpdatePair> updateTextureUpdatePair(LayerItem item, boolean isNightMode) {
        List<CustomUpdatePair> customUpdatePairs = new ArrayList<>();
        switch (item.getBusinessType()) {
            case BizRouteType.BizRouteTypeViaPoint -> {
                RoutePathPointItem pointItem = (RoutePathPointItem) item;
                if (pointItem.getPathId() == 0) {
                    //隐藏途经点右上角关闭按钮
                    customUpdatePairs.add(createUpdateStylePair("click_delete_focus", "display:none;"));
                    customUpdatePairs.add(createUpdateStylePair("click_delete", "display:none;"));
                }
            }
            case BizRouteType.BizRouteTypeGuideLabel -> {
                //备选路线标签深色文字颜色适配
                if (isNightMode) {
                    customUpdatePairs.add(createUpdateStylePair("label_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("time_diff_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("cost_text1", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("distance_diff_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("traffic_light_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("cost_text2", "color:#ffffff;"));
                }
            }
            case BizRoadFacilityType.BizRoadFacilityTypeGuideCameraActive -> {
                //导航态电子眼深色文字颜色适配
                if (isNightMode) {
                    customUpdatePairs.add(createUpdateStylePair("distance_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("distance_text_penalty", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("speed_text2", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("speed_text3", "color:#ffffff;"));
                }
            }
            case BizRoadFacilityType.BizRoadFacilityTypeCruiseCameraActive -> {
                //巡航态电子眼深色文字颜色适配
                if (isNightMode) {
                    customUpdatePairs.add(createUpdateStylePair("distance_text", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("distance_text_penalty", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("speed_text2", "color:#ffffff;"));
                    customUpdatePairs.add(createUpdateStylePair("speed_text3", "color:#ffffff;"));
                }
            }
        }
        return customUpdatePairs;
    }
}
