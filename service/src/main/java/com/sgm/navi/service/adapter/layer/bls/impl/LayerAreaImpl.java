package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.model.GCoord3DDouble;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaDataCityAllRuleRes;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaDataRuleRes;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaResponseParam;
import com.autonavi.gbl.aosclient.model.GRestrictCity;
import com.autonavi.gbl.aosclient.model.GRestrictRule;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizAreaType;
import com.autonavi.gbl.layer.model.BizRouteEndAreasInfo;
import com.autonavi.gbl.layer.model.BizRouteRestrictInfo;
import com.autonavi.gbl.layer.model.RouteEndAreaPointInfo;
import com.autonavi.gbl.layer.model.RouteEndAreaType;
import com.autonavi.gbl.map.MapView;
import com.sgm.navi.service.adapter.layer.bls.style.LayerAreaStyleAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;

public class LayerAreaImpl extends BaseLayerImpl<LayerAreaStyleAdapter> {

    public LayerAreaImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        this.className = "LayerAreaImpl";
        getLayerAreaControl().setStyle(this);
        getLayerAreaControl().addClickObserver(this);
    }


    @Override
    protected LayerAreaStyleAdapter createStyleAdapter() {
        return new LayerAreaStyleAdapter(getEngineId(), getLayerAreaControl());
    }

    /**
     * 清除限行线
     */
    public void clearRestrictPolyline() {
        getLayerAreaControl().clearAllItems(BizAreaType.BizAreaTypeRestrictPolyline);
    }

    /**
     * 绘制限行区域
     */
    public void showLimitRestrictionView(ArrayList<GRestrictCity> cities, int position) {
        if (ConvertUtils.isEmpty(cities)) {
            getLayerAreaControl().clearAllItems(BizAreaType.BizAreaTypeRestrictPolygon);
            getLayerAreaControl().clearAllItems(BizAreaType.BizAreaTypeRestrictPolyline);
            return;
        }
        for (int t = NumberUtils.NUM_0; t < cities.get(position).rules.size(); t++) {
            GRestrictRule pRestrictRule = cities.get(position).rules.get(t);
            BizRouteRestrictInfo bizPolygonDataLine = new BizRouteRestrictInfo();
            int linePointsListNum = pRestrictRule.linepoints.size();
            for (int i = NumberUtils.NUM_0; i < linePointsListNum; i++) {
                int linePointsNum = pRestrictRule.linepoints.get(i).lstPoints.size();
                ArrayList<Coord3DDouble> lineData = new ArrayList<>();
                for (int j = NumberUtils.NUM_0; j < linePointsNum; j++) {
                    GCoord3DDouble point = pRestrictRule.linepoints.get(i).lstPoints.get(j);
                    Coord3DDouble linePoint = new Coord3DDouble();
                    linePoint.lon = point.lon;
                    linePoint.lat = point.lat;
                    linePoint.z = point.z;
                    lineData.add(linePoint);
                }

                bizPolygonDataLine.lineInfos.add(lineData);
            }

            if (0 < bizPolygonDataLine.lineInfos.size()) {
                bizPolygonDataLine.isDrawPolygonRim = true;
                getLayerAreaControl().updateRouteRestrict(bizPolygonDataLine);
            }

            int areaPointsListNum = pRestrictRule.areapoints.size();

            for (int i = NumberUtils.NUM_0; i < areaPointsListNum; i++) {
                BizRouteRestrictInfo bizPolygonDataPolygon = new BizRouteRestrictInfo();
                int areaPointsNum = pRestrictRule.areapoints.get(i).lstPoints.size();

                for (int j = NumberUtils.NUM_0; j < areaPointsNum; j++) {
                    GCoord3DDouble point = pRestrictRule.areapoints.get(i).lstPoints.get(j);
                    Coord3DDouble areaPoint = new Coord3DDouble();
                    areaPoint.lon = point.lon;
                    areaPoint.lat = point.lat;
                    areaPoint.z = point.z;
                    bizPolygonDataPolygon.polygonPoints.add(areaPoint);
                    bizPolygonDataPolygon.isDrawPolygonRim = true;
                }

                if (0 < bizPolygonDataPolygon.polygonPoints.size()) {
                    getLayerAreaControl().updateRouteRestrict(bizPolygonDataPolygon);
                }
            }
        }
    }


    /**
     * 绘制限行区域
     */
    public void showRestrictionView(Object object, int position) {
        if (ConvertUtils.isEmpty(object)) {
            getLayerAreaControl().clearAllItems(BizAreaType.BizAreaTypeRestrictPolygon);
            getLayerAreaControl().clearAllItems(BizAreaType.BizAreaTypeRestrictPolyline);
            return;
        }
        GReStrictedAreaResponseParam param = (GReStrictedAreaResponseParam) object;
        GReStrictedAreaDataRuleRes ruleRes = param.data.mDataRule;
        if (ruleRes.citynums <= NumberUtils.NUM_0 || ConvertUtils.isEmpty(ruleRes.cities)) {
            GReStrictedAreaDataCityAllRuleRes cityAllRuleRes = param.data.mCityAllRule;
            showLimitRestrictionView(cityAllRuleRes.typelist, position);
            return;
        }
        for (int t = NumberUtils.NUM_0; t < ruleRes.cities.get(position).rules.size(); t++) {
            GRestrictRule pRestrictRule = ruleRes.cities.get(position).rules.get(t);
            BizRouteRestrictInfo bizPolygonDataLine = new BizRouteRestrictInfo();
            int linePointsListNum = pRestrictRule.linepoints.size();
            for (int i = NumberUtils.NUM_0; i < linePointsListNum; i++) {
                int linePointsNum = pRestrictRule.linepoints.get(i).lstPoints.size();
                ArrayList<Coord3DDouble> lineData = new ArrayList<>();
                for (int j = NumberUtils.NUM_0; j < linePointsNum; j++) {
                    GCoord3DDouble point = pRestrictRule.linepoints.get(i).lstPoints.get(j);
                    Coord3DDouble linePoint = new Coord3DDouble();
                    linePoint.lon = point.lon;
                    linePoint.lat = point.lat;
                    linePoint.z = point.z;
                    lineData.add(linePoint);
                }

                bizPolygonDataLine.lineInfos.add(lineData);
            }

            if (0 < bizPolygonDataLine.lineInfos.size()) {
                bizPolygonDataLine.isDrawPolygonRim = true;
                getLayerAreaControl().updateRouteRestrict(bizPolygonDataLine);
            }

            int areaPointsListNum = pRestrictRule.areapoints.size();

            for (int i = NumberUtils.NUM_0; i < areaPointsListNum; i++) {
                BizRouteRestrictInfo bizPolygonDataPolygon = new BizRouteRestrictInfo();
                int areaPointsNum = pRestrictRule.areapoints.get(i).lstPoints.size();

                for (int j = NumberUtils.NUM_0; j < areaPointsNum; j++) {
                    GCoord3DDouble point = pRestrictRule.areapoints.get(i).lstPoints.get(j);
                    Coord3DDouble areaPoint = new Coord3DDouble();
                    areaPoint.lon = point.lon;
                    areaPoint.lat = point.lat;
                    areaPoint.z = point.z;
                    bizPolygonDataPolygon.polygonPoints.add(areaPoint);
                    bizPolygonDataPolygon.isDrawPolygonRim = true;
                }

                if (0 < bizPolygonDataPolygon.polygonPoints.size()) {
                    getLayerAreaControl().updateRouteRestrict(bizPolygonDataPolygon);
                }
            }
        }
    }

    /**
     *  显示终点名称
     *  xml:end_area_parent_point.xml
     * */
    public void showEndAreaPoint(RouteParam poiInfo) {
        if (ConvertUtils.isEmpty(poiInfo)) {
            Logger.e(TAG, getMapType(), " poiInfo is null");
            return;
        }
        RectInt rectInt = new RectInt(400, 900, 100, 600);
        BizRouteEndAreasInfo bizRouteEndAreasInfo = new BizRouteEndAreasInfo();
        RouteEndAreaPointInfo majorPointData = new RouteEndAreaPointInfo();
        majorPointData.id = "parent";
        if (ConvertUtils.isEmpty(poiInfo.getRealPos())) {
            Logger.e(TAG, getMapType(), " poiInfo.getRealPos is null");
            return;
        }
        majorPointData.mPos3D.lon = poiInfo.getRealPos().getLon();
        majorPointData.mPos3D.lat = poiInfo.getRealPos().getLat();
        majorPointData.mPos3D.z = 0.0f;
        majorPointData.poiName = poiInfo.getName();
        bizRouteEndAreasInfo.vecParentPointInfo.add(majorPointData);
        getLayerAreaControl().updateRouteEndAreas(bizRouteEndAreasInfo, rectInt);
        Logger.d(TAG, getMapType(), " poiName ", poiInfo.getName());
    }

    /* 清除终点名称 */
    public void clearEndAreaPoint() {
        Logger.d(TAG, "clearEndAreaPoint");
        getLayerAreaControl().clearRouteEndArea(RouteEndAreaType.RouteEndAreaTypeAll);
    }
}
