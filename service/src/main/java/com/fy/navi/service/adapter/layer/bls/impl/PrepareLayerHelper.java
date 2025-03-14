package com.fy.navi.service.adapter.layer.bls.impl;


import android.annotation.SuppressLint;

import com.android.utils.log.Logger;
import com.autonavi.gbl.layer.GuideTrafficSignalLightLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.BizAGroupType;
import com.autonavi.gbl.layer.model.BizAreaType;
import com.autonavi.gbl.layer.model.BizCustomTypePoint;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.layer.model.BizRoadCrossType;
import com.autonavi.gbl.layer.model.BizRoadFacilityType;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.layer.LayerItem;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.bls.manager.SearchLayerStyle;
import com.fy.navi.service.adapter.layer.bls.parser.ParserStyleLayerUtils;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/20
 */
public class PrepareLayerHelper {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public PrepareLayerHelper() {

    }

    /**
     * 判断图层Item是否要采用动态纹理
     *
     * @param pItem 图层item
     * @return 是否是动态图层
     */
    public boolean isDynamicMarkerLayerType(LayerItem pItem) {
        if (null == pItem) {
            return false;
        }
        //充电扎标为动态扎标
        int businessType = pItem.getBusinessType();
        if (businessType == BizSearchType.BizSearchTypePoiParentPoint) {
            SearchParentLayerItem parentItem = (SearchParentLayerItem) pItem;
            int poiType = parentItem.getPoiType();
            if (poiType == SearchLayerStyle.INFORMATION_TYPE_CHARGE) {
                return true;
            }
        }
        return (businessType == BizAGroupType.BizAGroupTypeAGroup ||
                businessType == BizCustomTypePoint.BizCustomTypePoint3 ||
                businessType == BizAreaType.BizAreaTypeEndAreaParentPoint ||
                businessType == BizRouteType.BizRouteTypeViaETA ||
                businessType == BizRouteType.BizRouteTypeRestArea ||
                businessType == BizRouteType.BizRouteTypeViaRoad ||
                businessType == BizRouteType.BizRouteTypeWeather ||
                businessType == BizRouteType.BizRouteTypeRouteJamBubbles ||
                businessType == BizRouteType.BizRouteTypeEnergyEmptyPoint);
    }

    @SuppressLint("SwitchIntDef")
    protected String getLayerItemPointType(int businessType, LayerItem layerItem) {
        switch (businessType) {
            case BizRouteType.BizRouteTypeStartPoint -> {
                return ParserStyleLayerUtils.getRouteTypePointJson("route_type_start_point");
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                return ParserStyleLayerUtils.getRouteTypePointJson("route_type_end_point");
            }
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                SearchParentLayerItem parentItem = (SearchParentLayerItem) layerItem;
                int poiType = parentItem.getPoiType();
                Logger.d(TAG, "search marker poi type: " + poiType);
                // TODO: 2024/12/22 目前没有UI暂不确定是否需要区分点的类型 暂时先不区分统一打点
                StringBuilder typeStyle = new StringBuilder("search_result_parent_point_");
               switch (poiType) {
                   case SearchLayerStyle.INFORMATION_TYPE_GAS:
                       typeStyle.append("gas");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_CHARGE:
                       typeStyle.append("charge");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_CAR_WASHING:
                       typeStyle.append("car_washing");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_FOOD:
                       typeStyle.append("food");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_PARKING:
                       typeStyle.append("parking");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_SCENIC:
                       typeStyle.append("scenic");
                       break;
                   case SearchLayerStyle.INFORMATION_TYPE_SERVICE_ZONE:
                       typeStyle.append("service_zone");
                       break;
                   default:
                       typeStyle.append(Math.min(parentItem.getMIndex(), 10));
                       break;
               }

                return ParserStyleLayerUtils.getSearchResultPointLayer(typeStyle.toString());
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                StringBuilder typeStyle = new StringBuilder("point_search_parking_");
                Logger.i(TAG, "naviparking " + layerItem.getID().trim());
                typeStyle.append(layerItem.getID().trim());
                return ParserStyleLayerUtils.getSearchParkingResultPointLayer(typeStyle.toString());
            }
            case BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight -> {
                return ParserStyleLayerUtils.getTrafficLightLayer(layerItem);
            }
            default -> {
                return null;
            }
        }
    }

    protected String getLayerItemCrossType(int businessType) {
        Logger.i(TAG, "CrossImage_tag getLayerItemCrossType " + businessType);
        switch (businessType) {
            case BizRoadCrossType.BizRoadCrossTypeRasterImage -> {
                return ParserStyleLayerUtils.getLayerItemRasterImageType();
            }
            case BizRoadCrossType.BizRoadCrossTypeVector -> {//矢量路口
                return ParserStyleLayerUtils.getLayerItemVectorCrossType();
            }
            default -> {
                return null;
            }
        }
    }
}
