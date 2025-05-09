package com.fy.navi.service.adapter.layer.bls.style;

import static android.view.View.GONE;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.SearchAlongWayLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemSearchPark;
import com.fy.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerSearchStyleAdapter extends BaseStyleAdapter {

    //沿途搜-自定义充电站扎标
    private static final String KEY_SEARCH_ALONG_WAY_CHARGE = "search_along_way_charge";
    //终点可停车-自定义停车场扎标
    private static final String KEY_SEARCH_PARK_ROUTE = "search_park_route";

    private static List<PoiInfoEntity> mParkingInfoList = new ArrayList<>();

    public LayerSearchStyleAdapter(int engineID, BizSearchControl bizSearchControl) {
        super(engineID);
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                if (item instanceof SearchParentLayerItem parentLayerItem) {
                    int poiType = parentLayerItem.getPoiType();
                    switch (poiType) {
                        case LayerSearchPOIType.SEARCH_PARK -> {
                            //替换停车场json  当前默认json仅支持终点推荐停车场(最多3个扎标)
                            return super.provideLayerItemStyleJson(item);
                        }
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
                            return KEY_SEARCH_ALONG_WAY_CHARGE;
                        }
                    }
                }
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                Logger.d(TAG, "自定义终点停车场扎标 isNightMode " + isNightMode());
                return KEY_SEARCH_PARK_ROUTE;
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
        if (item.getBusinessType() == BizSearchType.BizSearchTypePoiAlongRoute) {
            return new IUpdateBitmapViewProcessor() {
                @Override
                public void onFocusProcess(View rootView, LayerItemData data) {
                    SearchAlongWayLayerItem chargeItem = (SearchAlongWayLayerItem) item;
                    BizChargeStationInfo info = chargeItem.getMExtraData().chargeStationInfo;
                    TextView position = rootView.findViewById(R.id.search_along_way_charge_position);
                    TextView fastTextView = rootView.findViewById(R.id.search_along_way_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_along_way_charge_slow);
                    String id = chargeItem.getID();
                    int index = Integer.parseInt(id);
                    position.setText(String.valueOf(index + 1));
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
                    fastTextView.setText(fastString);

                    if (slowFree == NumberUtils.NUM_0 && slowTotal == NumberUtils.NUM_0) {
                        slowTextView.setVisibility(GONE);
                    }
                    String slowString = context.getString(R.string.layer_search_along_way_charge_slow, slowFree, slowTotal);
                    slowTextView.setText(slowString);
                }

                @Override
                public void onNormalProcess(View rootView, LayerItemData data) {
                    SearchAlongWayLayerItem chargeItem = (SearchAlongWayLayerItem) item;
                    BizChargeStationInfo info = chargeItem.getMExtraData().chargeStationInfo;
                    TextView position = rootView.findViewById(R.id.search_along_way_charge_position);
                    TextView fastTextView = rootView.findViewById(R.id.search_along_way_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_along_way_charge_slow);
                    String id = chargeItem.getID();
                    int index = Integer.parseInt(id);
                    position.setText(String.valueOf(index + 1));
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
                    fastTextView.setText(fastString);

                    if (slowFree == NumberUtils.NUM_0 && slowTotal == NumberUtils.NUM_0) {
                        slowTextView.setVisibility(GONE);
                    }
                    String slowString = context.getString(R.string.layer_search_along_way_charge_slow, slowFree, slowTotal);
                    slowTextView.setText(slowString);
                }
            };
        } else if (item.getBusinessType() == BizSearchType.BizSearchTypePoiParkRoute) {
            return new IUpdateBitmapViewProcessor<LayerItemSearchPark>() {
                @Override
                public void onFocusProcess(View rootView, LayerItemSearchPark data) {
                    if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getParkInfo()) ||
                            ConvertUtils.isEmpty(data.getParkInfo().getMParkingInfoList())) {
                        Logger.e(TAG, "自定义终点停车场扎标  getMParkingInfoList is Empty");
                        return;
                    }
                    List<ParkingInfo> parkingInfoList = data.getParkInfo().getMParkingInfoList();
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
                    titleTextView.setText(context.getString(R.string.layer_search_park_route_title, spaceResult));

                    if (spaceFree == NumberUtils.NUM_0 || spaceTotal == NumberUtils.NUM_0) {
                        detailPercentageTextView.setVisibility(GONE);
                    } else {
                        int ratio = spaceFree / spaceTotal * 100;
                        detailPercentageTextView.setText(context.getString(R.string.layer_search_park_route_detail_percentage, ratio));
                    }
                    detailTotalTextView.setText(context.getString(R.string.layer_search_park_route_detail_total, spaceTotal));
                }

                @Override
                public void onNormalProcess(View rootView, LayerItemSearchPark data) {
                    if (ConvertUtils.isEmpty(data) || ConvertUtils.isEmpty(data.getParkInfo()) ||
                            ConvertUtils.isEmpty(data.getParkInfo().getMParkingInfoList())) {
                        Logger.e(TAG, "自定义终点停车场扎标  getMParkingInfoList is Empty");
                        return;
                    }
                    List<ParkingInfo> parkingInfoList = data.getParkInfo().getMParkingInfoList();
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
                    titleTextView.setText(context.getString(R.string.layer_search_park_route_title, spaceResult));

                    if (spaceFree == NumberUtils.NUM_0 || spaceTotal == NumberUtils.NUM_0) {
                        detailPercentageTextView.setVisibility(GONE);
                    } else {
                        int ratio = spaceFree / spaceTotal * 100;
                        detailPercentageTextView.setText(context.getString(R.string.layer_search_park_route_detail_percentage, ratio));
                    }
                    detailTotalTextView.setText(context.getString(R.string.layer_search_park_route_detail_total, spaceTotal));
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }

    public void updateSearchResult(List<PoiInfoEntity> parkingInfoList) {
        if (ConvertUtils.isEmpty(parkingInfoList)) {
            Logger.e(TAG, "updateSearchResult parkingInfoList is Empty");
            return;
        }
        Logger.d(TAG, "updateSearchResult");
        mParkingInfoList = parkingInfoList;
    }

    private LayerItemSearchPark getSearchParkRoutePoint(LayerItem item) {
        LayerItemSearchPark searchPark = new LayerItemSearchPark();
        String id = item.getID();
        int index = Integer.parseInt(id);
        if (index >= NumberUtils.NUM_0 && index < mParkingInfoList.size()) {
            PoiInfoEntity poiInfoEntity = mParkingInfoList.get(index);
            searchPark.setParkInfo(poiInfoEntity);
        }
        Logger.d(TAG, "getSearchParkRoutePoint index " + index + " searchPark " + searchPark.toString());
        return searchPark;
    }

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
}
