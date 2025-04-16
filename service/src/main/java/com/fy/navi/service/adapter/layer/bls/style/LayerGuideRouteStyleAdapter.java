package com.fy.navi.service.adapter.layer.bls.style;

import android.annotation.SuppressLint;
import android.text.Html;
import android.text.Spanned;
import android.view.View;
import android.widget.TextView;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.BizLabelControl;
import com.autonavi.gbl.layer.BizRoadCrossControl;
import com.autonavi.gbl.layer.BizRoadFacilityControl;
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.R;
import com.fy.navi.service.define.layer.refix.LayerItemData;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;

public class LayerGuideRouteStyleAdapter extends BaseStyleAdapter {
    private static final String KEY_ROAD_RASTER_CROSS = "road_raster_cross";
    private static final String KEY_ROAD_VECTOR_CROSS = "road_vector_cross";
    private static final String KEY_ROAD_REAL_CROSS = "road_real_cross";
    private static final String KEY_ROAD_END_POINT = "road_end_point";

    private BizGuideRouteControl mRouteControl;
    private LayerItemRouteEndPoint mLayerItemRouteEndPoint = new LayerItemRouteEndPoint();

    public LayerGuideRouteStyleAdapter(int engineID, BizRoadCrossControl bizRoadCrossControl, BizGuideRouteControl bizGuideRouteControl, BizRoadFacilityControl roadFacilityControl, BizLabelControl bizLabelControl) {
        super(engineID);
        this.mRouteControl = bizGuideRouteControl;
    }

    @Override
    public String provideLayerItemStyleJson(LayerItem item) {
        Logger.d(TAG, "provideLayerItemStyleJson BusinessType" + item.getBusinessType());
        switch (item.getBusinessType()) {
            case BizRoadCrossType.BizRoadCrossTypeVector -> {
                Logger.d(TAG,"2D矢量路口大图图层");
                return KEY_ROAD_VECTOR_CROSS;
            }
            case BizRoadCrossType.BizRoadCrossTypeRasterImage -> {
                Logger.d(TAG,"栅格路口大图图层");
                return KEY_ROAD_RASTER_CROSS;
            }
            case BizRoadCrossType.BizRoadCrossTypeRealCity -> {
                Logger.d(TAG,"3D精品大图图层");
                return KEY_ROAD_REAL_CROSS;
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                if (ConvertUtils.isEmpty(mLayerItemRouteEndPoint)) {
                    Logger.d(TAG, "默认终点扎标 ");
                    return super.provideLayerItemStyleJson(item);
                }
                int endPointType = mLayerItemRouteEndPoint.getEndPointType();
                if (endPointType == 1 || endPointType == 2) {
                    Logger.d(TAG, "剩余油量/电量 终点扎标 " + mLayerItemRouteEndPoint.toString());
                    return KEY_ROAD_END_POINT;
                }
            }
        }
        return super.provideLayerItemStyleJson(item);
    }

    public void updateLabelPoint(LayerItemRouteEndPoint result) {
        if (ConvertUtils.isEmpty(result)) {
            Logger.e(TAG, "updateLabelPoint result == null");
            return;
        }
        Logger.d(TAG, "updateLabelPoint result " + result.toString());
        if (mRouteControl != null) {
            Logger.d(TAG, "改变终点扎标内容");
            mLayerItemRouteEndPoint = result;
            mRouteControl.updateStyle();
        }
    }

    @Override
    public LayerItemData provideLayerItemData(LayerItem item) {
        if (item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            if (item instanceof RoutePathPointItem) {
                return mLayerItemRouteEndPoint;
            }
        }
        return super.provideLayerItemData(item);
    }

    @Override
    public boolean isNeedReCreate(LayerItem item) {
        if (mRouteControl != null && item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            return true;
        }
        return super.isNeedReCreate(item);
    }

    @Override
    public IUpdateBitmapViewProcessor provideUpdateBitmapViewProcessor(LayerItem item) {
        if (item.getBusinessType() == BizRouteType.BizRouteTypeEndPoint) {
            if (item instanceof RoutePathPointItem) {
                return new IUpdateBitmapViewProcessor<LayerItemRouteEndPoint>() {
                    @SuppressLint("StringFormatInvalid")
                    @Override
                    public void onNormalProcess(View rootView, LayerItemRouteEndPoint data) {
                        Logger.d(TAG, "更新终点扎标信息");
                        if (rootView != null && data != null) {
                            TextView text = rootView.findViewById(R.id.route_end_detail);
                            Logger.d(TAG, "更新终点扎标信息 data " + data.toString());
                            int pointType = data.getEndPointType();
                            if (pointType == 1) {
                                String string = rootView.getContext().getString(R.string.layer_route_end_pop_detail, rootView.getContext().getString(R.string.layer_route_end_pop_type_battery), data.getRestNum());
                                Spanned spanned = Html.fromHtml(string);
                                text.setText(spanned);
                            } else if (pointType == 2) {
                                String string = rootView.getContext().getString(R.string.layer_route_end_pop_detail, rootView.getContext().getString(R.string.layer_route_end_pop_type_oil), data.getRestNum());
                                Spanned spanned = Html.fromHtml(string);
                                text.setText(spanned);
                            }
                        }
                    }
                };
            }
        }
        return super.provideUpdateBitmapViewProcessor(item);
    }

}
