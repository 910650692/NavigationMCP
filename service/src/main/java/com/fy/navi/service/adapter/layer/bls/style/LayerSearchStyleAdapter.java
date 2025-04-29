package com.fy.navi.service.adapter.layer.bls.style;

import static android.view.View.GONE;

import android.content.Context;
import android.view.View;
import android.widget.TextView;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizSearchControl;
import com.autonavi.gbl.layer.SearchAlongWayLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;
import com.fy.navi.service.define.utils.NumberUtils;

public class LayerSearchStyleAdapter extends BaseStyleAdapter {

    private static final String KEY_SEARCH_ALONG_WAY_CHARGE = "search_along_way_charge";

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
        }
        return super.provideLayerItemStyleJson(item);
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
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }
}
