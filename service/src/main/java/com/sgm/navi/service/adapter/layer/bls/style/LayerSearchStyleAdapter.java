package com.sgm.navi.service.adapter.layer.bls.style;

import static android.view.View.GONE;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.SearchAlongWayLayerItem;
import com.autonavi.gbl.layer.SearchChargeStationLayerItem;
import com.autonavi.gbl.layer.model.BizChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.layer.model.SearchAlongWayExtraData;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.CustomUpdatePair;
import com.sgm.navi.service.R;
import com.sgm.navi.service.define.layer.refix.LayerItemData;
import com.sgm.navi.service.define.layer.refix.LayerItemSearchPoint;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.sgm.navi.service.define.search.ParkingInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicReference;

public class LayerSearchStyleAdapter extends BaseStyleAdapter {

    private BizSearchControl mSearchControl;
    //沿途搜-自定义充电站扎标
    private static final String KEY_SEARCH_POINT_ALONG_WAY_CHARGE = "search_point_along_way_charge";
    private static final String KEY_SEARCH_ALONG_WAY_CHARGE = "search_along_way_charge";
    //终点可停车-自定义停车场扎标
    private static final String KEY_SEARCH_PARK_ROUTE = "search_park_route";
    //搜索列表可见数字扎标
    private static final String KEY_SEARCH_LIST_INDEX = "search_list_index";
    //充电桩列表可见数字扎标
    private static final String KEY_SEARCH_LIST_CHARGE_INDEX = "search_list_charge_index";

    private final AtomicReference<List<PoiInfoEntity>> mPoiInfoList = new AtomicReference(new ArrayList<>());

    public LayerSearchStyleAdapter(int engineID, BizSearchControl bizSearchControl) {
        super(engineID);
        this.mSearchControl = bizSearchControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                int index = getIndexOfLayerItem(item);
                if (index <= 11 && (mPoiInfoList.get().size() > (index -1))) {
                    PoiInfoEntity poiInfoEntity = mPoiInfoList.get().get(index - 1);
                    if (!ConvertUtils.isEmpty(poiInfoEntity) && poiInfoEntity.isMIsVisible()) {
                        Logger.d(TAG, "搜索列表可见数字扎标-index =" + index);
                        return KEY_SEARCH_LIST_INDEX + "_" + index;
                    }
                }
            }
            case BizSearchType.BizSearchTypePoiAlongRoute -> {
                if (item instanceof SearchAlongWayLayerItem alongWayLayerItem) {
                    int typeCode = alongWayLayerItem.getMTypeCode();
                    Logger.d(TAG, "沿途搜类型 typeCode " + typeCode);
                    switch (typeCode) {
                        case LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CHARGE -> {
                            Logger.d(TAG, "沿途搜-自定义充电站扎标");
                            return KEY_SEARCH_POINT_ALONG_WAY_CHARGE;
                        }
                    }
                }
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                Logger.d(TAG, "自定义终点停车场扎标");
                return KEY_SEARCH_PARK_ROUTE;
            }
            case BizSearchType.BizSearchTypeChargeStation -> {
                int index = getIndexOfLayerItem(item);
                if (index <= 11 && (mPoiInfoList.get().size() > (index - 1))) {
                    PoiInfoEntity poiInfoEntity = mPoiInfoList.get().get(index - 1);
                    if (!ConvertUtils.isEmpty(poiInfoEntity) && poiInfoEntity.isMIsVisible()) {
                        Logger.d(TAG, "搜索充电桩 列表可见数字扎标-index =" + index);
                        return KEY_SEARCH_LIST_CHARGE_INDEX + "_" + index;
                    }
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                //自定义终点停车场扎标
                return getSearchParkRoutePoint(item);
            }
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        int index = getLayerItemIndex(item);
        if (item.getBusinessType() == BizSearchType.BizSearchTypePoiAlongRoute) {
            return new IUpdateBitmapViewProcessor<>() {
                @Override
                public void onFocusProcess(View rootView, LayerItemData data) {
                    SearchAlongWayLayerItem chargeItem = (SearchAlongWayLayerItem) item;
                    BizChargeStationInfo info = chargeItem.getMExtraData().chargeStationInfo;
                    TextView position = rootView.findViewById(R.id.search_along_way_charge_position);
                    TextView fastTextView = rootView.findViewById(R.id.search_along_way_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_along_way_charge_slow);
                    safetySetText(position, String.valueOf(index + 1));
                    Logger.d(TAG, "自定义沿途搜充电站扎标-选中态 index " + (index + 1));
                    int fastFree = info.fastFree;
                    int fastTotal = info.fastTotal;
                    int slowFree = info.slowFree;
                    int slowTotal = info.slowTotal;

                    if (fastFree == NumberUtils.NUM_0 && fastTotal == NumberUtils.NUM_0) {
                        fastTextView.setVisibility(GONE);
                    }
                    Context context = rootView.getContext();
                    String fastString = context.getString(R.string.layer_search_along_way_charge_fast, fastFree, fastTotal);
                    safetySetText(fastTextView, fastString);

                    if (slowFree == NumberUtils.NUM_0 && slowTotal == NumberUtils.NUM_0) {
                        slowTextView.setVisibility(GONE);
                    }
                    String slowString = context.getString(R.string.layer_search_along_way_charge_slow, slowFree, slowTotal);
                    safetySetText(slowTextView, slowString);
                }

                @Override
                public void onNormalProcess(View rootView, LayerItemData data) {
                    SearchAlongWayLayerItem chargeItem = (SearchAlongWayLayerItem) item;
                    BizChargeStationInfo info = chargeItem.getMExtraData().chargeStationInfo;
                    TextView position = rootView.findViewById(R.id.search_along_way_charge_position);
                    TextView fastTextView = rootView.findViewById(R.id.search_along_way_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_along_way_charge_slow);
                    safetySetText(position, String.valueOf(index + 1));
                    Logger.d(TAG, "自定义沿途搜充电站扎标-普通态 index " + (index + 1));
                    int fastFree = info.fastFree;
                    int fastTotal = info.fastTotal;
                    int slowFree = info.slowFree;
                    int slowTotal = info.slowTotal;

                    if (fastFree == NumberUtils.NUM_0 && fastTotal == NumberUtils.NUM_0) {
                        fastTextView.setVisibility(GONE);
                    }
                    Context context = rootView.getContext();
                    String fastString = context.getString(R.string.layer_search_along_way_charge_fast, fastFree, fastTotal);
                    safetySetText(fastTextView, fastString);

                    if (slowFree == NumberUtils.NUM_0 && slowTotal == NumberUtils.NUM_0) {
                        slowTextView.setVisibility(GONE);
                    }
                    String slowString = context.getString(R.string.layer_search_along_way_charge_slow, slowFree, slowTotal);
                    safetySetText(slowTextView, slowString);
                }
            };
        } else if (item.getBusinessType() == BizSearchType.BizSearchTypePoiParkRoute) {
            return new IUpdateBitmapViewProcessor<LayerItemSearchPoint>() {
                @Override
                public void onFocusProcess(View rootView, LayerItemSearchPoint data) {
                    if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getPoiInfo()) ||
                            ConvertUtils.isEmpty(data.getPoiInfo().getMParkingInfoList())) {
                        Logger.e(TAG, "自定义终点停车场扎标  getMParkingInfoList is Empty");
                        return;
                    }
                    List<ParkingInfo> parkingInfoList = data.getPoiInfo().getMParkingInfoList();
                    ParkingInfo parkingInfo = parkingInfoList.get(NumberUtils.NUM_0);
                    if (ConvertUtils.isEmpty(parkingInfo)) {
                        Logger.e(TAG, "自定义终点停车场扎标 parkingInfo == null");
                        return;
                    }
                    TextView titleTextView = rootView.findViewById(R.id.search_park_route_title_text);
                    TextView detailPercentageTextView = rootView.findViewById(R.id.search_park_route_detail_percentage_text);
                    TextView detailTotalTextView = rootView.findViewById(R.id.search_park_route_detail_total_text);

                    Context context = rootView.getContext();
                    int spaceTotal = parkingInfo.getSpaceTotal();
                    int spaceFree = parkingInfo.getSpaceFree();
                    Logger.d(TAG, "自定义终点停车场扎标 spaceFree " + spaceFree + " spaceTotal " + spaceTotal);
                    String spaceResult = judgeParkingSpace(context, spaceFree, spaceTotal);
                    safetySetText(titleTextView, context.getString(R.string.layer_search_park_route_title, spaceResult));

                    if (spaceFree == NumberUtils.NUM_0 || spaceTotal == NumberUtils.NUM_0) {
                        if (!ConvertUtils.isEmpty(detailPercentageTextView)) {
                            detailPercentageTextView.setVisibility(GONE);
                        }
                    } else {
                        int ratio = spaceFree / spaceTotal * 100;
                        safetySetText(detailPercentageTextView, context.getString(R.string.layer_search_park_route_detail_percentage, ratio));
                    }
                    String detailTotalString = context.getString(R.string.layer_search_park_route_detail_total, spaceTotal);
                    try {
                        int detailTotalCount = Integer.parseInt(detailTotalString);
                        if (detailTotalCount == -1) {
                            if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                                detailTotalTextView.setVisibility(GONE);
                            }
                        } else {
                            if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                                detailTotalTextView.setText(detailTotalString);
                            }
                        }
                    } catch (Exception e) {
                        Logger.e(TAG, "类型转换错误");
                        if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                            detailTotalTextView.setVisibility(GONE);
                        }
                    }
                }

                @Override
                public void onNormalProcess(View rootView, LayerItemSearchPoint data) {
                    if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getPoiInfo()) ||
                            ConvertUtils.isEmpty(data.getPoiInfo().getMParkingInfoList())) {
                        Logger.e(TAG, "自定义终点停车场扎标  getMParkingInfoList is Empty");
                        return;
                    }
                    List<ParkingInfo> parkingInfoList = data.getPoiInfo().getMParkingInfoList();
                    ParkingInfo parkingInfo = parkingInfoList.get(NumberUtils.NUM_0);
                    if (ConvertUtils.isEmpty(parkingInfo)) {
                        Logger.e(TAG, "自定义终点停车场扎标 parkingInfo == null");
                        return;
                    }
                    TextView titleTextView = rootView.findViewById(R.id.search_park_route_title_text);
                    TextView detailPercentageTextView = rootView.findViewById(R.id.search_park_route_detail_percentage_text);
                    TextView detailTotalTextView = rootView.findViewById(R.id.search_park_route_detail_total_text);

                    Context context = rootView.getContext();
                    int spaceTotal = parkingInfo.getSpaceTotal();
                    int spaceFree = parkingInfo.getSpaceFree();
                    Logger.d(TAG, "自定义终点停车场扎标 spaceFree " + spaceFree + " spaceTotal " + spaceTotal);
                    String spaceResult = judgeParkingSpace(context, spaceFree, spaceTotal);
                    safetySetText(titleTextView, context.getString(R.string.layer_search_park_route_title, spaceResult));

                    if (spaceFree == NumberUtils.NUM_0 || spaceTotal == NumberUtils.NUM_0) {
                        detailPercentageTextView.setVisibility(GONE);
                    } else {
                        int ratio = spaceFree / spaceTotal * 100;
                        safetySetText(detailPercentageTextView, context.getString(R.string.layer_search_park_route_detail_percentage, ratio));
                    }
                    String detailTotalString = context.getString(R.string.layer_search_park_route_detail_total, spaceTotal);
                    try {
                        int detailTotalCount = Integer.parseInt(detailTotalString);
                        if (detailTotalCount == -1) {
                            if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                                detailTotalTextView.setVisibility(GONE);
                            }
                        } else {
                            if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                                detailTotalTextView.setText(detailTotalString);
                            }
                        }
                    } catch (Exception e) {
                        Logger.e(TAG, "类型转换错误");
                        if (!ConvertUtils.isEmpty(detailTotalTextView)) {
                            detailTotalTextView.setVisibility(GONE);
                        }
                    }
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
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

    /* 更新搜索结果数据 */
    public void updateSearchResult(List<PoiInfoEntity> poiInfoEntityList) {
        if (ConvertUtils.isEmpty(poiInfoEntityList)) {
            Logger.e(TAG, "updateSearchResult poiInfoEntityList is Empty");
            return;
        }
        Logger.d(TAG, "updateSearchResult");
        mPoiInfoList.get().clear();
        mPoiInfoList.get().addAll(poiInfoEntityList);
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(LayerPointItemType type, List<PoiInfoEntity> poiInfoEntityList) {
        if (ConvertUtils.isEmpty(poiInfoEntityList)) {
            Logger.e(TAG, "updateSearchResult poiInfoEntityList is Empty");
            return;
        }
        Logger.d(TAG, "updateSearchResult type " + type);
        mPoiInfoList.get().clear();
        mPoiInfoList.get().addAll(poiInfoEntityList);
        switch (type) {
            case SEARCH_PARENT_POINT -> {
                mSearchControl.updateStyle(BizSearchType.BizSearchTypePoiParentPoint);
            }
            case SEARCH_PARENT_CHARGE_STATION -> {
                mSearchControl.updateStyle(BizSearchType.BizSearchTypeChargeStation);
            }
        }
    }

    private LayerItemSearchPoint getSearchParkRoutePoint(LayerItem item) {
        LayerItemSearchPoint searchPark = new LayerItemSearchPoint();
        int index = getLayerItemIndex(item);
        if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.get().size()) {
            PoiInfoEntity poiInfoEntity = mPoiInfoList.get().get(index);
            searchPark.setPoiInfo(poiInfoEntity);
        }
        Logger.d(TAG, "getSearchParkRoutePoint index " + index + " searchPark " + searchPark.toString());
        return searchPark;
    }

    private int getLayerItemIndex(LayerItem item) {
        if (item == null) {
            Logger.e(TAG, "getLayerItemIndex item == null");
            return 0;
        }
        String id = item.getID();
        try {
            return Integer.parseInt(id);
        } catch (Exception e) {
            Logger.e(TAG, "getLayerItemIndex ID format: " + id);
            return 0;
        }
    }

    //判断停车位资源是否充足
    private String judgeParkingSpace(Context context, int spaceFree, int spaceTotal) {
        if (spaceTotal == 0) {
            return "";
        }
        double ratio = (double) spaceFree / spaceTotal;
        if (ratio >= 0.3) {
            return context.getResources().getString(R.string.layer_search_park_route_resource_enough);
        } else {
            return context.getResources().getString(R.string.layer_search_park_route_resource_shortage);
        }
    }


    @Override
    public List<CustomUpdatePair> updateTextureUpdatePair(LayerItem item) {
        List<CustomUpdatePair> customUpdatePairs = new ArrayList<>();
        switch (item.getBusinessType()) {
            case BizSearchType.BizSearchTypePoiAlongRoute:
                if (item instanceof SearchAlongWayLayerItem chargeItem) {
                    SearchAlongWayExtraData extraData = chargeItem.getMExtraData();
                    if (ConvertUtils.isNull(extraData) || ConvertUtils.isNull(extraData.chargeStationInfo)) {
                        Logger.e(TAG, "charge station is null");
                        return customUpdatePairs;
                    }
                    final BizChargeStationInfo chargeStation = extraData.chargeStationInfo;
                    final int fastTotal = chargeStation.fastTotal;
                    final int fastFree = chargeStation.fastFree;
                    final int slowTotal = chargeStation.slowTotal;
                    final int slowFree = chargeStation.slowFree;
                    final int index = getLayerItemIndex(item);
                    customUpdatePairs.add(createUpdateValuePair("id_position", String.valueOf(index + 1)));
                    if (fastTotal == 0) {
                        customUpdatePairs.add(createUpdateStylePair("div_fast", "display:none;"));
                    } else {
                        customUpdatePairs.add(createUpdateValuePair("id_fast", "快"+fastFree + "/" + fastTotal));
                    }
                    if (slowTotal == 0) {
                        customUpdatePairs.add(createUpdateStylePair("div_slow", "display:none;"));
                    } else {
                        customUpdatePairs.add(createUpdateValuePair("id_slow", "慢"+slowFree + "/" + slowTotal));
                    }
                    if (fastTotal == 0 && slowTotal == 0) {
                        customUpdatePairs.add(createUpdateStylePair("search_charge_label", "display:none;"));
                    }
                }
                break;
            case BizSearchType.BizSearchTypeChargeStation: {
                //TODO判断是否已经预约
                SearchChargeStationLayerItem searchChargeItem = (SearchChargeStationLayerItem) item;
                BizChargeStationInfo chargeStationInfo = searchChargeItem.getMChargeStationInfo();
                if (ConvertUtils.isEmpty(chargeStationInfo)) {
                    if (Logger.openLog) {
                        Logger.e(TAG, "chargeStationInfo == null");
                    }
                    return customUpdatePairs;
                }
                int slowFree = chargeStationInfo.slowFree;
                int slowTotal = chargeStationInfo.slowTotal;
                int fastFree = chargeStationInfo.fastFree;
                int fastTotal = chargeStationInfo.fastTotal;
                boolean isSlowShow = true;
                boolean isFastShow = true;
                if (slowTotal == 0) {
                    customUpdatePairs.add(createUpdateStylePair("div_slow", "display:none;"));
                    isSlowShow = false;
                } else {
                    customUpdatePairs.add(createUpdateValuePair("id_slow", slowFree + "/" + slowTotal));
                }
                if (fastTotal == 0) {
                    customUpdatePairs.add(createUpdateStylePair("div_fast", "display:none;"));
                    isFastShow = false;
                } else {
                    customUpdatePairs.add(createUpdateValuePair("id_fast", fastFree + "/" + fastTotal));
                }
                if (!isSlowShow && !isFastShow) {
                    customUpdatePairs.add(createUpdateStylePair("detail_info", "display:none;"));
                } else if (isSlowShow && isFastShow){
                    break;
                } else if (isSlowShow){
                    customUpdatePairs.add(createUpdateStylePair("detail_info", "margin-start:120px;"));
                } else {
                    customUpdatePairs.add(createUpdateStylePair("detail_info", "margin-start:140px;"));
                }
                break;
            }
        }
        return customUpdatePairs;
    }

    public boolean isFromCardImagesRes(LayerItem item) {
        if (item.getBusinessType() == BizSearchType.BizSearchTypePoiAlongRoute) {
            if (item instanceof SearchAlongWayLayerItem alongWayLayerItem) {
               final int typeCode = alongWayLayerItem.getMTypeCode();
                Logger.d(TAG, "沿途搜类型 typeCode " + typeCode);
                return typeCode == LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CHARGE;
            }
        }
        return false;
    }

}
