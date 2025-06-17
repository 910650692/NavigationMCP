package com.fy.navi.service.adapter.layer.bls.style;

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
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteSupplementInfo;
import com.fy.navi.service.define.route.RouteSupplementParams;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.utils.GasCarTipManager;

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

    public LayerGuideRouteStyleAdapter(int engineID, BizRoadCrossControl bizRoadCrossControl, BizGuideRouteControl bizGuideRouteControl, BizRoadFacilityControl roadFacilityControl, BizLabelControl bizLabelControl) {
        super(engineID);
        this.mRouteControl = bizGuideRouteControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
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
                    return KEY_ROAD_END_DEFAULT;
            }
            case BizRouteType.BizRouteTypeEnergyEmptyPoint -> {
                Logger.d(TAG, "能量耗尽点");
                return KEY_ROAD_ENERGY_EMPTY;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                if (ConvertUtils.isEmpty(mRouteChargeStation.getMRouteSupplementParams())) {
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
                    } else if (pathId == LayerPointItemType.ROUTE_POINT_VIA_CHARGE.ordinal()) {
                        Logger.d(TAG, "途经点扎标-充电站");
                        return KEY_ROAD_ROUTE_VIA_CHARGE_STATION;
                    }
                    Logger.d(TAG, "途经点扎标-默认扎标");
                    return super.provideLayerItemStyleJson(item);
                }
            }
            case BizRouteType.BizRouteTypeGuideLabel -> {
                Logger.i(TAG, "多备选路线标签 BizRouteTypeGuideLabel");
                return super.provideLayerItemStyleJson(item);
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    @Override
    public boolean isNeedRefreshStyleJson(LayerItem item) {
        Logger.d(TAG, "isNeedRefreshJsonValue");
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
                        LayerPointItemType endPointType = data.getEndPointType();
                        Logger.d(TAG, "更新终点扎标样式 endPointType " + endPointType);
                        TextView text = rootView.findViewById(R.id.route_end_detail);
                        if (ConvertUtils.isEmpty(data)) {
                            Logger.e(TAG, "更新终点扎标信息 getEndPointInfo is null");
                            return;
                        }
                        Logger.d(TAG, "更新终点扎标信息 data " + data);
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
                        Logger.d(TAG, "更新补能规划扎标信息 " + data.getMRouteChargeStationInfo().toString());
                        safetySetText(position, String.valueOf(data.getIndex() + 1));
                        RouteChargeStationDetailInfo chargeStationInfo = data.getMRouteChargeStationInfo();

                        if (!ConvertUtils.isEmpty(chargeStationInfo)) {
                            int chargeTime = chargeStationInfo.getMChargeTime() / 60;
                            int intervalDistance = chargeStationInfo.getMInterval() / 1000;
                            Context rootViewContext = rootView.getContext();
                            String timeStr = rootViewContext.getString(R.string.layer_route_via_charge_time, chargeTime);
                            safetySetText(time, timeStr);

                            String firstString = rootViewContext.getString(R.string.layer_route_via_charge_distance_first);
                            String behindString = rootViewContext.getString(R.string.layer_route_via_charge_distance_behind);
                            String distanceStringTitle = data.getIndex() == 0 ? firstString : behindString;
                            String distanceString = rootViewContext.getString(R.string.layer_route_via_charge_distance, distanceStringTitle, intervalDistance);
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
     * 途经点充电站数据  //todo 暂时没有数据
     * @param rootView
     * @param data
     */
    private void setViaChargeStationInfo(View rootView, LayerItemRouteReplaceChargePoint data) {
        final TextView fastText = rootView.findViewById(R.id.route_charge_station_fast);
        final TextView slowText = rootView.findViewById(R.id.route_charge_station_slow);
        final Context context = rootView.getContext();
        final String fastString = context.getString(R.string.layer_route_replace_charge_fast, "5/5");
        safetySetText(fastText, fastString);
        final String slowString = context.getString(R.string.layer_route_replace_charge_slow, "6/6");
        safetySetText(slowText, slowString);
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
     * 更新算路时补能点信息
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
     * @param routeChargeStation
     */
    private void updateChargeStationDistance(RouteChargeStationParam routeChargeStation) {
        final ArrayList<RouteSupplementParams> routeSupplementParams = routeChargeStation.getMRouteSupplementParams();
        if (!ConvertUtils.isEmpty(routeSupplementParams)) {
            for (RouteSupplementParams mRouteSupplementParam : routeSupplementParams) {
               final ArrayList<RouteSupplementInfo> routeSupplementInfos = mRouteSupplementParam.getMRouteSupplementInfos();
                if (!ConvertUtils.isEmpty(routeSupplementInfos) && routeSupplementInfos.size() > NumberUtils.NUM_1) {
                    for (int i = 0; i < routeSupplementInfos.size(); i++) {
                        final RouteSupplementInfo currentRouteInfo = routeSupplementInfos.get(i);
                        final RouteSupplementInfo previousRouteInfo = routeSupplementInfos.get(i - 1);
                        currentRouteInfo.setMDistance(currentRouteInfo.getMDistance() - previousRouteInfo.getMDistance());
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
     * 获取途经点充电站数据   //todo mViaList还没有充电站数据
     * @param item
     * @return LayerItemRouteReplaceChargePoint
     */
    private LayerItemRouteReplaceChargePoint getRouteViaChargePoint(LayerItem item) {
        LayerItemRouteReplaceChargePoint chargePoint = new LayerItemRouteReplaceChargePoint();
        chargePoint.setIndex(Integer.parseInt(item.getID()));
        chargePoint.setType(1);
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
        if (mCarType == 0 || mCarType == 2) {
            if (selectPathIndex < mRouteLineInfos.size()) {
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
                Logger.v(TAG, "selectPathIndex < mRouteLineInfos.size()");
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
                    routeChargeStationDetailInfo.setMInterval(routeSupplementInfo.getMDistance());
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

//    @Override
//    public List<CustomUpdatePair> createUpdatePair(LayerItem item, String markerInfo) {
//        switch (item.getBusinessType()) {
//            case BizRouteType.BizRouteTypeGuideLabel:
//                Logger.d(TAG, "多备选路线卡片内容填充");
//                GuideLabelLayerItem labelItem = (GuideLabelLayerItem) item;
//                return addCardGuideLabelMarker(labelItem, markerInfo);
//            case BizRouteType.BizRouteTypeRouteJamBubbles:
//                RouteJamBubblesLayerItem jamBubblesLayerItem = (RouteJamBubblesLayerItem) item;
//                return addCardGuideJamBubblesMarker(jamBubblesLayerItem, markerInfo);
//            case BizRouteType.BizRouteTypeViaETA:
//                ViaETALayerItem viaETALayerItem = (ViaETALayerItem) item;
//                return addCardGuideViaETAMarker(viaETALayerItem);
//
//        }
//        return super.createUpdatePair(item, markerInfo);
//    }
//
//    private List<CustomUpdatePair> addCardGuideViaETAMarker(ViaETALayerItem viaETALayerItem) {
//        List<CustomUpdatePair> viaETAList = new ArrayList<>();
//        viaETAList.add(createUpdateStylePair("end_area_all", "display: none;"));
//        return viaETAList;
//    }
//
//    private List<CustomUpdatePair> addCardGuideJamBubblesMarker(RouteJamBubblesLayerItem jamBubblesLayerItem, String markerInfo) {
//        List<CustomUpdatePair> jamBubblesList = new ArrayList<>();
//        JamBubblesSegmentDeepInfo deepInfo = jamBubblesLayerItem.getDeepInfo();
//        JamBubblesSegmentDeepInfo cost = jamBubblesLayerItem.getCost();
//        JamBubblesSegmentDeepInfo degree = jamBubblesLayerItem.getDegree();
//        JamBubblesSegmentDeepInfo trend = jamBubblesLayerItem.getTrend();
//
//        Logger.d(TAG, "addCardGuideJamBubblesMarker" +
//                "\n deepInfo " + GsonUtils.toJson(deepInfo) +
//                "\n cost " + GsonUtils.toJson(cost) +
//                "\n degree " + GsonUtils.toJson(degree) +
//                "\n trend " + GsonUtils.toJson(trend) +
//                "\n markerInfo " + markerInfo);
//        int deepInfoSceneType = deepInfo.sceneType;
//        String deepInfoText = deepInfo.text;
//
//        String costText = cost.text;
//        int costSceneType = cost.sceneType;
//
//        int degreeSceneType = degree.sceneType;
//        String degreeText = degree.text;
//
//        String trendText = trend.text;
//        int trendSceneType = trend.sceneType;
//
//        if (ConvertUtils.isEmpty(deepInfoText) && ConvertUtils.isEmpty(costText) &&
//                ConvertUtils.isEmpty(trendText) && ConvertUtils.isEmpty(degreeText)) {
//            jamBubblesList.add(createUpdateStylePair("jam_bubbles", "display: none;"));
//            Logger.e(TAG, "addCardGuideJamBubblesMarker no title");
//            return jamBubblesList;
//        }
//        List<Integer> validNumbers = new ArrayList<>();
//        if (!ConvertUtils.isEmpty(trendText)) validNumbers.add(trendSceneType);
//        if (!ConvertUtils.isEmpty(degreeText)) validNumbers.add(degreeSceneType);
//        if (!ConvertUtils.isEmpty(costText)) validNumbers.add(costSceneType);
//        if (!ConvertUtils.isEmpty(deepInfoText)) validNumbers.add(deepInfoSceneType);
//        int minScene = 1;
//        if (!ConvertUtils.isEmpty(validNumbers)) {
//            for (int num : validNumbers) {
//                if (num > minScene) {
//                    minScene = num;
//                }
//            }
//        }
//        Logger.d(TAG, "addCardGuideJamBubblesMarker minScene " + minScene);
//        if (!ConvertUtils.isEmpty(markerInfo)) {
//            if (markerInfo.contains("left_up")) {
//                jamBubblesList.add(createUpdateStylePair("jam_bubbles", "background-image:global_image_bubble_left_top_day_night;"));
//            } else if (markerInfo.contains("left_down")) {
//                jamBubblesList.add(createUpdateStylePair("jam_bubbles", "background-image:global_image_bubble_left_bottom_day_night;"));
//            } else if (markerInfo.contains("right_up")) {
//                jamBubblesList.add(createUpdateStylePair("jam_bubbles", "background-image:global_image_bubble_right_top_day_night;"));
//            } else if (markerInfo.contains("right_down")) {
//                jamBubblesList.add(createUpdateStylePair("jam_bubbles", "background-image:global_image_bubble_right_bottom_day_night;"));
//            }
//        }
//        jamBubblesList.add(createUpdateStylePair("jam_bubbles", "width:auto"));
//        jamBubblesList.add(createUpdateStylePair("jam_bubbles", "height:auto"));
//        /**
//         * sceneType
//         * 1 默认黑色
//         * 2 缓行绿色
//         * 3 缓慢橘色
//         * 4 严重褐色
//         */
//        switch (minScene) {
//            case 1 -> {
//                jamBubblesList.add(createUpdateStylePair("traffic_div_smooth", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_slow", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_congested", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_default", "display: flex;"));
//                if (ConvertUtils.isEmpty(deepInfoText)) {
//                    jamBubblesList.add(createUpdateStylePair("main_title_default", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_default", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("main_title_default", deepInfoText));
//                }
//                if (ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_col_default", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_default", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_col_default", costText));
//                }
//                if (ConvertUtils.isEmpty(deepInfoText) && ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("text_time_dist_default", "display: none;"));
//                }
//                if (ConvertUtils.isEmpty(degreeText)) {
//                    jamBubblesList.add(createUpdateStylePair("sub_title_default", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("sub_title_default", degreeText));
//                }
//                if (ConvertUtils.isEmpty(trendText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_row_default", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_row_default", trendText));
//                }
//            }
//            case 2 -> {
//                jamBubblesList.add(createUpdateStylePair("traffic_div_smooth", "display: flex;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_slow", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_congested", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_default", "display: none;"));
//                if (ConvertUtils.isEmpty(deepInfoText)) {
//                    jamBubblesList.add(createUpdateStylePair("main_title_smooth", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_smooth", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("main_title_smooth", deepInfoText));
//                }
//                if (ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_col_smooth", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_smooth", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_col_smooth", costText));
//                }
//                if (ConvertUtils.isEmpty(deepInfoText) && ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("text_time_dist_smooth", "display: none;"));
//                }
//                if (ConvertUtils.isEmpty(degreeText)) {
//                    jamBubblesList.add(createUpdateStylePair("sub_title_smooth", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("sub_title_smooth", degreeText));
//                }
//                if (ConvertUtils.isEmpty(trendText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_row_smooth", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_row_smooth", trendText));
//                }
//            }
//            case 3 -> {
//                jamBubblesList.add(createUpdateStylePair("traffic_div_smooth", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_slow", "display: flex;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_congested", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_default", "display: none;"));
//                if (ConvertUtils.isEmpty(deepInfoText)) {
//                    jamBubblesList.add(createUpdateStylePair("main_title_slow", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_slow", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("main_title_slow", deepInfoText));
//                }
//                if (ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_col_slow", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_slow", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_col_slow", costText));
//                }
//                if (ConvertUtils.isEmpty(deepInfoText) && ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("text_time_dist_slow", "display: none;"));
//                }
//                if (ConvertUtils.isEmpty(degreeText)) {
//                    jamBubblesList.add(createUpdateStylePair("sub_title_slow", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("sub_title_slow", degreeText));
//                }
//                if (ConvertUtils.isEmpty(trendText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_row_slow", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_row_slow", trendText));
//                }
//            }
//            case 4 -> {
//                jamBubblesList.add(createUpdateStylePair("traffic_div_smooth", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_slow", "display: none;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_congested", "display: flex;"));
//                jamBubblesList.add(createUpdateStylePair("traffic_div_default", "display: none;"));
//                if (ConvertUtils.isEmpty(deepInfoText)) {
//                    jamBubblesList.add(createUpdateStylePair("main_title_congested", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_congested", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("main_title_congested", deepInfoText));
//                }
//                if (ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_col_congested", "display: none;"));
//                    jamBubblesList.add(createUpdateStylePair("line_image_congested", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_col_congested", costText));
//                }
//                if (ConvertUtils.isEmpty(deepInfoText) && ConvertUtils.isEmpty(costText)) {
//                    jamBubblesList.add(createUpdateStylePair("text_time_dist_congested", "display: none;"));
//                }
//                if (ConvertUtils.isEmpty(degreeText)) {
//                    jamBubblesList.add(createUpdateStylePair("sub_title_congested", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("sub_title_congested", degreeText));
//                }
//                if (ConvertUtils.isEmpty(trendText)) {
//                    jamBubblesList.add(createUpdateStylePair("time_row_congested", "display: none;"));
//                } else {
//                    jamBubblesList.add(createUpdatePair("time_row_congested", trendText));
//                }
//            }
//        }
//
//        jamBubblesList.add(createUpdateStylePair("pic_image", "display: none;"));
//        return jamBubblesList;
//    }
//
//    private List<CustomUpdatePair> addCardGuideLabelMarker(GuideLabelLayerItem item, String markerInfo) {
//        List<CustomUpdatePair> guideLabelPairs = new ArrayList<>();
//        String time;
//        int travelTimeDiff = item.getMTravelTimeDiff();
//        if (travelTimeDiff > 0) {
//            time = "慢";
//        } else {
//            time = "快";
//        }
//        time += CommonUtil.formatTimeBySecond(Math.abs(travelTimeDiff));
//
//        String distance;
//        int distanceDiff = item.getMDistanceDiff();
//        if (distanceDiff > 0) {
//            distance = "多";
//        } else {
//            distance = "少";
//        }
//        distance += CommonUtil.distanceUnitTransform(Math.abs(distanceDiff));
//
//        String trafficLight;
//        int trafficLightDiff = item.getMTrafficLightDiff();
//        if (trafficLightDiff > 0) {
//            trafficLight = "多%d个";
//        } else {
//            trafficLight = "少%d个";
//        }
//        if (trafficLightDiff == 0) {
//            trafficLight = "一致";
//        } else {
//            trafficLight = String.format(trafficLight, Math.abs(trafficLightDiff));
//        }
//
//        String cost;
//        int costDiff = item.getMCostDiff();
//        if (costDiff > 0) {
//            cost = "多%d元";
//        } else {
//            cost = "少%d元";
//        }
//        cost = String.format(cost, Math.abs(costDiff));
//        if (!ConvertUtils.isEmpty(markerInfo)) {
//            if (markerInfo.contains("left_up")) {
//                guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "background-image:global_image_navi_bg_route_label_left_up_day_night;"));
//            } else if (markerInfo.contains("left_down")) {
//                guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "background-image:global_image_navi_bg_route_label_left_down_day_night;"));
//            } else if (markerInfo.contains("right_up")) {
//                guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "background-image:global_image_navi_bg_route_label_right_up_day_night;"));
//            } else if (markerInfo.contains("right_down")) {
//                guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "background-image:global_image_navi_bg_route_label_right_down_day_night;"));
//            }
//        }
//        guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "width:auto"));
//        guideLabelPairs.add(createUpdateStylePair("guide_label_background_image", "height:auto"));
//        if (costDiff == 0) {
//            guideLabelPairs.add(createUpdateStylePair("cost_text1", "display: none;"));
//            guideLabelPairs.add(createUpdateStylePair("cost_text2", "display: none;"));
//            guideLabelPairs.add(createUpdateStylePair("cost_image1", "display: none;"));
//            guideLabelPairs.add(createUpdateStylePair("cost_image2", "display: none;"));
//        }
//
//        Logger.d(TAG, "addCardGuideLabelMarker " +
//                "\n time " + time +
//                "\n distance " + distance +
//                "\n trafficLight " + trafficLight +
//                "\n cost " + cost +
//                "\n markerInfo " + markerInfo);
//        guideLabelPairs.add(createUpdatePair("time_diff_text", time));
//        guideLabelPairs.add(createUpdatePair("cost_text1", item.getMPathCost() + "元"));
//        guideLabelPairs.add(createUpdatePair("distance_diff_text", distance));
//        guideLabelPairs.add(createUpdatePair("traffic_light_text", trafficLight));
//        return guideLabelPairs;
//    }
}
