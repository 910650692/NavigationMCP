package com.fy.navi.service.adapter.layer.bls.manager;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.aosclient.model.GCoord3DDouble;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaDataCityAllRuleRes;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaDataRuleRes;
import com.autonavi.gbl.aosclient.model.GReStrictedAreaResponseParam;
import com.autonavi.gbl.aosclient.model.GRestrictCity;
import com.autonavi.gbl.aosclient.model.GRestrictRule;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizAreaType;
import com.autonavi.gbl.layer.model.BizRouteRestrictInfo;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/9
 */
public class AreaLayerStyle extends BaseLayerStyle {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    public AreaLayerStyle(BizControlService bizService, MapView mapView) {
        super(bizService, mapView);
    }

    @Override
    protected void unInit() {
        if (!ConvertUtils.isEmpty(mBizAreaControl)) mBizAreaControl = null;
    }

    /**
     *
     * 绘制限行区域
     * */
    public void showLimitRestrictionView(ArrayList<GRestrictCity> cities, int position) {
        if (ConvertUtils.isEmpty(cities)) {
            mBizAreaControl.clearAllItems(BizAreaType.BizAreaTypeRestrictPolygon);
            mBizAreaControl.clearAllItems(BizAreaType.BizAreaTypeRestrictPolyline);
            return;
        }
        for (int t = NumberUtils.NUM_0; t < cities.get(position).rules.size(); t++)
        {
            GRestrictRule pRestrictRule = cities.get(position).rules.get(t);
            BizRouteRestrictInfo bizPolygonDataLine = new BizRouteRestrictInfo();
            int linePointsListNum = pRestrictRule.linepoints.size();
            for (int i = NumberUtils.NUM_0; i < linePointsListNum; i++)
            {
                int linePointsNum = pRestrictRule.linepoints.get(i).lstPoints.size();
                ArrayList<Coord3DDouble> lineData = new ArrayList<>();
                for (int j = NumberUtils.NUM_0; j < linePointsNum; j++)
                {
                    GCoord3DDouble point = pRestrictRule.linepoints.get(i).lstPoints.get(j);
                    Coord3DDouble linePoint = new Coord3DDouble();
                    linePoint.lon = point.lon;
                    linePoint.lat = point.lat;
                    linePoint.z = point.z;
                    lineData.add(linePoint);
                }

                bizPolygonDataLine.lineInfos.add(lineData);
            }

            if (0 < bizPolygonDataLine.lineInfos.size())
            {
                bizPolygonDataLine.isDrawPolygonRim = true;
                mBizAreaControl.updateRouteRestrict(bizPolygonDataLine);
            }

            int areaPointsListNum = pRestrictRule.areapoints.size();

            for (int i = NumberUtils.NUM_0; i < areaPointsListNum; i++)
            {
                BizRouteRestrictInfo bizPolygonDataPolygon = new BizRouteRestrictInfo();
                int areaPointsNum = pRestrictRule.areapoints.get(i).lstPoints.size();

                for (int j = NumberUtils.NUM_0; j < areaPointsNum; j++)
                {
                    GCoord3DDouble point = pRestrictRule.areapoints.get(i).lstPoints.get(j);
                    Coord3DDouble areaPoint = new Coord3DDouble();
                    areaPoint.lon = point.lon;
                    areaPoint.lat = point.lat;
                    areaPoint.z = point.z;
                    bizPolygonDataPolygon.polygonPoints.add(areaPoint);
                    bizPolygonDataPolygon.isDrawPolygonRim = true;
                }

                if (0 < bizPolygonDataPolygon.polygonPoints.size())
                {
                    mBizAreaControl.updateRouteRestrict(bizPolygonDataPolygon);
                }
            }
        }
    }

    /**
     *
     * 绘制限行区域
    * */
    public void showRestrictionView(Object object, int position) {
        if (ConvertUtils.isEmpty(object)) {
            mBizAreaControl.clearAllItems(BizAreaType.BizAreaTypeRestrictPolygon);
            mBizAreaControl.clearAllItems(BizAreaType.BizAreaTypeRestrictPolyline);
            return;
        }
        GReStrictedAreaResponseParam param = (GReStrictedAreaResponseParam) object;
        GReStrictedAreaDataRuleRes ruleRes = param.data.mDataRule;
        if (ruleRes.citynums <= NumberUtils.NUM_0 || ConvertUtils.isEmpty(ruleRes.cities)) {
            GReStrictedAreaDataCityAllRuleRes cityAllRuleRes = param.data.mCityAllRule;
            showLimitRestrictionView(cityAllRuleRes.typelist, position);
            return;
        }
        for (int t = NumberUtils.NUM_0; t < ruleRes.cities.get(position).rules.size(); t++)
        {
            GRestrictRule pRestrictRule = ruleRes.cities.get(position).rules.get(t);
            BizRouteRestrictInfo bizPolygonDataLine = new BizRouteRestrictInfo();
            int linePointsListNum = pRestrictRule.linepoints.size();
            for (int i = NumberUtils.NUM_0; i < linePointsListNum; i++)
            {
                int linePointsNum = pRestrictRule.linepoints.get(i).lstPoints.size();
                ArrayList<Coord3DDouble> lineData = new ArrayList<>();
                for (int j = NumberUtils.NUM_0; j < linePointsNum; j++)
                {
                    GCoord3DDouble point = pRestrictRule.linepoints.get(i).lstPoints.get(j);
                    Coord3DDouble linePoint = new Coord3DDouble();
                    linePoint.lon = point.lon;
                    linePoint.lat = point.lat;
                    linePoint.z = point.z;
                    lineData.add(linePoint);
                }

                bizPolygonDataLine.lineInfos.add(lineData);
            }

            if (0 < bizPolygonDataLine.lineInfos.size())
            {
                bizPolygonDataLine.isDrawPolygonRim = true;
                mBizAreaControl.updateRouteRestrict(bizPolygonDataLine);
            }

            int areaPointsListNum = pRestrictRule.areapoints.size();

            for (int i = NumberUtils.NUM_0; i < areaPointsListNum; i++)
            {
                BizRouteRestrictInfo bizPolygonDataPolygon = new BizRouteRestrictInfo();
                int areaPointsNum = pRestrictRule.areapoints.get(i).lstPoints.size();

                for (int j = NumberUtils.NUM_0; j < areaPointsNum; j++)
                {
                    GCoord3DDouble point = pRestrictRule.areapoints.get(i).lstPoints.get(j);
                    Coord3DDouble areaPoint = new Coord3DDouble();
                    areaPoint.lon = point.lon;
                    areaPoint.lat = point.lat;
                    areaPoint.z = point.z;
                    bizPolygonDataPolygon.polygonPoints.add(areaPoint);
                    bizPolygonDataPolygon.isDrawPolygonRim = true;
                }

                if (0 < bizPolygonDataPolygon.polygonPoints.size())
                {
                    mBizAreaControl.updateRouteRestrict(bizPolygonDataPolygon);
                }
            }
        }
    }
}
