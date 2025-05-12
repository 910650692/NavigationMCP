package com.fy.navi.service.adapter.layer.bls.style;

import static android.view.View.GONE;

import android.content.Context;
import android.content.res.TypedArray;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.SearchAlongWayLayerItem;
import com.autonavi.gbl.layer.SearchChargeStationLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemSearchPoint;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerSearchStyleAdapter extends BaseStyleAdapter {

    private BizSearchControl mSearchControl;
    //沿途搜-自定义充电站扎标
    private static final String KEY_SEARCH_ALONG_WAY_CHARGE = "search_along_way_charge";
    //终点可停车-自定义停车场扎标
    private static final String KEY_SEARCH_PARK_ROUTE = "search_park_route";
    //搜索列表默认扎标
    private static final String KEY_SEARCH_LIST_INDEX_NORMAL = "search_list_index_normal";
    //搜索列表可见数字扎标
    private static final String KEY_SEARCH_LIST_INDEX_FOCUSED = "search_list_index_focused";
    //充电站-自定义扎标
    private static final String KEY_SEARCH_CHARGE_POINT = "search_charge_point";
    //充电站-已预约扎标
    private static final String KEY_SEARCH_CHARGE_APPOINTMENT = "search_charge_appointment";
    //充电站-列表可见数字扎标
    private static final String KEY_SEARCH_CHARGE_LIST_INDEX_FOCUSED = "search_charge_list_index_focused";

    private static List<PoiInfoEntity> mPoiInfoList = new ArrayList<>();

    public LayerSearchStyleAdapter(int engineID, BizSearchControl bizSearchControl) {
        super(engineID);
        this.mSearchControl = bizSearchControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        switch (item.getBusinessType()) {
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                int index = getLayerItemIndex(item);
                if (ConvertUtils.isEmpty(mPoiInfoList)) {
                    Logger.d(TAG, "搜索列表默认扎标-mPoiInfoList is Empty");
                    return KEY_SEARCH_LIST_INDEX_NORMAL;
                } else {
                    if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
                        PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
                        if (ConvertUtils.isEmpty(poiInfoEntity) || !poiInfoEntity.isMIsVisible()) {
                            Logger.d(TAG, "搜索列表默认扎标-非list可见图元");
                            return KEY_SEARCH_LIST_INDEX_NORMAL;
                        } else {
                            Logger.d(TAG, "搜索列表可见数字扎标-index " + index);
                            return KEY_SEARCH_LIST_INDEX_FOCUSED;
                        }
                    } else {
                        Logger.d(TAG, "搜索列表默认扎标-下标越界");
                        return KEY_SEARCH_LIST_INDEX_NORMAL;
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
            case BizSearchType.BizSearchTypeChargeStation -> {
                int index = getLayerItemIndex(item);
                if (ConvertUtils.isEmpty(mPoiInfoList)) {
                    Logger.d(TAG, "充电站-自定义扎标-mPoiInfoList is Empty");
                    return KEY_SEARCH_CHARGE_POINT;
                } else {
                    if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
                        PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
                        if (ConvertUtils.isEmpty(poiInfoEntity) || !poiInfoEntity.isMIsVisible()) {
                            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
                            if (ConvertUtils.isEmpty(chargeInfoList) || ConvertUtils.isEmpty(chargeInfoList.get(0)) ||
                                    !chargeInfoList.get(0).isMIsAppointment()) {
                                Logger.d(TAG, "充电站-自定义扎标-非list可见图元-非预约");
                                return KEY_SEARCH_CHARGE_POINT;
                            } else {
                                Logger.d(TAG, "充电站-已预约扎标");
                                return KEY_SEARCH_CHARGE_APPOINTMENT;
                            }
                        } else {
                            Logger.d(TAG, "充电站-列表可见数字扎标-index " + index);
                            return KEY_SEARCH_CHARGE_LIST_INDEX_FOCUSED;
                        }
                    } else {
                        Logger.d(TAG, "充电站-自定义扎标-下标越界");
                        return KEY_SEARCH_CHARGE_POINT;
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
            return new IUpdateBitmapViewProcessor() {
                @Override
                public void onFocusProcess(View rootView, LayerItemData data) {
                    SearchAlongWayLayerItem chargeItem = (SearchAlongWayLayerItem) item;
                    BizChargeStationInfo info = chargeItem.getMExtraData().chargeStationInfo;
                    TextView position = rootView.findViewById(R.id.search_along_way_charge_position);
                    TextView fastTextView = rootView.findViewById(R.id.search_along_way_charge_fast);
                    TextView slowTextView = rootView.findViewById(R.id.search_along_way_charge_slow);
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
        } else if (item.getBusinessType() == BizSearchType.BizSearchTypePoiParentPoint) {
            return new IUpdateBitmapViewProcessor() {
                @Override
                public void onNormalProcess(View rootView, LayerItemData data) {
                    if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
                        PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
                        if (!ConvertUtils.isEmpty(poiInfoEntity) && poiInfoEntity.isMIsVisible()) {
                            Logger.d(TAG, "搜索列表可见数字扎标-index " + index);
                            ImageView imageView = rootView.findViewById(R.id.search_charge_list_focused);
                            TypedArray imageArray = rootView.getContext().getResources().obtainTypedArray(R.array.layer_icon_search_list_poi_array);
                            try {
                                int resourceId = imageArray.getResourceId(index, 0);
                                imageView.setImageResource(resourceId);
                            } finally {
                                imageArray.recycle();
                            }
                        }
                    }
                }
            };
        } else if (item.getBusinessType() == BizSearchType.BizSearchTypeChargeStation) {
            return new IUpdateBitmapViewProcessor() {
                @Override
                public void onFocusProcess(View rootView, LayerItemData data) {
                    SearchChargeStationLayerItem info = (SearchChargeStationLayerItem) item;
                    TextView fastText = rootView.findViewById(R.id.search_charge_detail_fast);
                    TextView slowText = rootView.findViewById(R.id.search_charge_detail_slow);
                    int fastFree = info.getMChargeStationInfo().fastFree;
                    int fastTotal = info.getMChargeStationInfo().fastTotal;
                    int slowFree = info.getMChargeStationInfo().slowFree;
                    int slowTotal = info.getMChargeStationInfo().slowTotal;

                    if (fastFree == NumberUtils.NUM_0 && fastTotal == NumberUtils.NUM_0) {
                        fastText.setVisibility(GONE);
                    }
                    Context context = rootView.getContext();
                    String fastString = context.getString(R.string.layer_search_along_way_charge_fast, fastFree, fastTotal);
                    fastText.setText(fastString);

                    if (slowFree == NumberUtils.NUM_0 && slowTotal == NumberUtils.NUM_0) {
                        slowText.setVisibility(GONE);
                    }
                    String slowString = context.getString(R.string.layer_search_along_way_charge_slow, slowFree, slowTotal);
                    slowText.setText(slowString);

                    if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
                        PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
                        if (ConvertUtils.isEmpty(poiInfoEntity) || !poiInfoEntity.isMIsVisible()) {
                            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
                            if (ConvertUtils.isEmpty(chargeInfoList) || ConvertUtils.isEmpty(chargeInfoList.get(0)) ||
                                    !chargeInfoList.get(0).isMIsAppointment()) {
                                Logger.d(TAG, "充电站-自定义扎标-Focus-非list可见图元-非预约");
                                ImageView imageView = rootView.findViewById(R.id.search_charge_focused);
                                String brand = chargeInfoList.get(0).getMBrand();
                                int brandIndex = getSearchChargeBrandIndex(brand);
                                if (brandIndex == NumberUtils.NUM_ERROR) {
                                    return;
                                }
                                TypedArray imageArray = rootView.getContext().getResources().obtainTypedArray(R.array.layer_icon_search_brand_normal_focused);
                                try {
                                    int resourceId = imageArray.getResourceId(brandIndex, 0);
                                    imageView.setImageResource(resourceId);
                                } finally {
                                    imageArray.recycle();
                                }
                            }
                        } else {
                            Logger.d(TAG, "充电站-列表可见数字扎标-index " + index);
                            ImageView imageView = rootView.findViewById(R.id.search_charge_focused);
                            TypedArray imageArray = rootView.getContext().getResources().obtainTypedArray(R.array.layer_icon_search_list_poi_array);
                            try {
                                int resourceId = imageArray.getResourceId(index, 0);
                                imageView.setImageResource(resourceId);
                            } finally {
                                imageArray.recycle();
                            }
                        }
                    }
                }

                @Override
                public void onNormalProcess(View rootView, LayerItemData data) {
                    if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
                        PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
                        if (ConvertUtils.isEmpty(poiInfoEntity) || !poiInfoEntity.isMIsVisible()) {
                            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
                            if (ConvertUtils.isEmpty(chargeInfoList) || ConvertUtils.isEmpty(chargeInfoList.get(0)) ||
                                    !chargeInfoList.get(0).isMIsAppointment()) {
                                Logger.d(TAG, "充电站-自定义扎标-Normal-非list可见图元-非预约");
                                ImageView imageView = rootView.findViewById(R.id.search_charge_normal);
                                String brand = chargeInfoList.get(0).getMBrand();
                                int brandIndex = getSearchChargeBrandIndex(brand);
                                if (brandIndex == NumberUtils.NUM_ERROR) {
                                    return;
                                }
                                TypedArray imageArray = rootView.getContext().getResources().obtainTypedArray(R.array.layer_icon_search_brand_normal_array);
                                try {
                                    int resourceId = imageArray.getResourceId(brandIndex, 0);
                                    imageView.setImageResource(resourceId);
                                } finally {
                                    imageArray.recycle();
                                }
                            }
                        }
                    }
                }
            };
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }

    /* 更新搜索结果数据 */
    public void updateSearchResult(List<PoiInfoEntity> poiInfoEntityList) {
        if (ConvertUtils.isEmpty(poiInfoEntityList)) {
            Logger.e(TAG, "updateSearchResult poiInfoEntityList is Empty");
            return;
        }
        Logger.d(TAG, "updateSearchResult");
        mPoiInfoList = poiInfoEntityList;
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(LayerPointItemType type, List<PoiInfoEntity> poiInfoEntityList) {
        if (ConvertUtils.isEmpty(poiInfoEntityList)) {
            Logger.e(TAG, "updateSearchResult poiInfoEntityList is Empty");
            return;
        }
        Logger.d(TAG, "updateSearchResult type " + type);
        mPoiInfoList = poiInfoEntityList;
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
        if (index >= NumberUtils.NUM_0 && index < mPoiInfoList.size()) {
            PoiInfoEntity poiInfoEntity = mPoiInfoList.get(index);
            searchPark.setPoiInfo(poiInfoEntity);
        }
        Logger.d(TAG, "getSearchParkRoutePoint index " + index + " searchPark " + searchPark.toString());
        return searchPark;
    }

    private int getLayerItemIndex(LayerItem item) {
        String id = item.getID();
        return Integer.parseInt(id);
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

    //获取充电站品牌对应下标
    private int getSearchChargeBrandIndex(String brand) {
        int index = NumberUtils.NUM_ERROR;
        if (ConvertUtils.isEmpty(brand)) {
            Logger.e(TAG, "getSearchChargeBrandIndex brand == null");
            return index;
        }
        if (brand.contains("国家电网")) {
            index = 0;
        } else if (brand.contains("普天")) {
            index = 1;
        } else if (brand.contains("星星")) {
            index = 2;
        } else if (brand.contains("特来电")) {
            index = 3;
        }
        Logger.d(TAG, "getSearchChargeBrandIndex index " + index);
        return index;
    }
}
