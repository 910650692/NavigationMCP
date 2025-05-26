package com.fy.navi.service.adapter.layer.bls.impl;


import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.SearchAlongWayLayerItem;
import com.autonavi.gbl.layer.SearchChargeStationLayerItem;
import com.autonavi.gbl.layer.SearchChildLayerItem;
import com.autonavi.gbl.layer.SearchParentLayerItem;
import com.autonavi.gbl.layer.model.AlongWayLabelType;
import com.autonavi.gbl.layer.model.BizLineBusinessInfo;
import com.autonavi.gbl.layer.model.BizPointBusinessInfo;
import com.autonavi.gbl.layer.model.BizPolygonBusinessInfo;
import com.autonavi.gbl.layer.model.BizSearchAlongWayPoint;
import com.autonavi.gbl.layer.model.BizSearchBeginEndPoint;
import com.autonavi.gbl.layer.model.BizSearchChargeStationInfo;
import com.autonavi.gbl.layer.model.BizSearchChildPoint;
import com.autonavi.gbl.layer.model.BizSearchExitEntrancePoint;
import com.autonavi.gbl.layer.model.BizSearchParentPoint;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.PointLayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerSearchStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.search.ChargeInfo;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerSearchImpl extends BaseLayerImpl<LayerSearchStyleAdapter> {

    private static final String LINE_TYPE_ROAD = "Road";
    private static final String LINE_TYPE_PARK = "Park";

    public LayerSearchImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerSearchControl().setStyle(this);
        getLayerSearchControl().addClickObserver(this);
    }

    @Override
    protected LayerSearchStyleAdapter createStyleAdapter() {
        return new LayerSearchStyleAdapter(getEngineId(), getLayerSearchControl());
    }

    public void setSelect(LayerPointItemType type, int index) {
        Logger.d(TAG, "setSelect type " + type + " index " + index);
        if (getLayerSearchControl() != null) {
            switch (type) {
                case SEARCH_PARENT_POINT -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiParentPoint, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParentPoint:" + result);
                }
                case SEARCH_CHILD_POINT -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiChildPoint, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiChildPoint:" + result);
                }
                case SEARCH_PARENT_PARK -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiParkRoute, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParkRoute:" + result);
                }
                case SEARCH_PARENT_CHARGE_STATION -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypeChargeStation, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-BizSearchTypeChargeStation:" + result);
                }
                case SEARCH_POI_ALONG_ROUTE -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiAlongRoute, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiAlongRoute:" + result);
                }
            }
        }
    }

    @Override
    protected void dispatchItemClickEvent(LayerItem item) {
        int index = 0;
        LayerPointItemType type = LayerPointItemType.NULL;
        switch (item.getBusinessType()) {
            // 搜索图层内容点击
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                index = Integer.parseInt(item.getID());
                type = LayerPointItemType.SEARCH_PARENT_POINT;
            }
            case BizSearchType.BizSearchTypePoiChildPoint -> {
                index = Integer.parseInt(item.getID());
                type = LayerPointItemType.SEARCH_CHILD_POINT;
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                index = Integer.parseInt(item.getID());
                type = LayerPointItemType.SEARCH_PARENT_PARK;
            }
            case BizSearchType.BizSearchTypeChargeStation -> {
                index = Integer.parseInt(item.getID());
                type = LayerPointItemType.SEARCH_PARENT_CHARGE_STATION;
            }
            case BizSearchType.BizSearchTypePoiAlongRoute -> {
                index = Integer.parseInt(item.getID());
                type = LayerPointItemType.SEARCH_POI_ALONG_ROUTE;
            }
            default -> {
                // TODO 扩展新的需求
            }
        }
        Logger.d(TAG, "dispatchItemClickEvent type = " + type + " ; index = " + index);
        for (ILayerAdapterCallBack callback : getCallBacks()) {
            callback.onSearchItemClick(getMapType(), type, index);
        }
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(LayerPointItemType type, LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchMarker searchResult == null");
            return false;
        }
        Logger.d(TAG, "updateSearchMarker type " + type);
        boolean result = false;
        switch (type) {
            case SEARCH_PARENT_Line_Road -> {
                ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
                PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
                ArrayList<ArrayList<GeoPoint>> mRoadPolygonBounds = parentPoint.getMRoadPolygonBounds();
                result = updateSearchLine(mRoadPolygonBounds, LINE_TYPE_ROAD);
            }
            case SEARCH_PARENT_Line_Park -> {
                ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
                PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
                ArrayList<ArrayList<GeoPoint>> mRoadPolygonBounds = parentPoint.getMRoadPolygonBounds();
                result = updateSearchLine(mRoadPolygonBounds, LINE_TYPE_PARK);
            }
            case SEARCH_PARENT_AREA -> {
                ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
                PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
                ArrayList<ArrayList<GeoPoint>> mPoiAoiBounds = parentPoint.getMPoiAoiBounds();
                result = updateSearchPolygon(mPoiAoiBounds);
            }
            case SEARCH_PARENT_POINT -> {
                result = updateSearchParentPoi(searchResult);
            }
            case SEARCH_CHILD_POINT -> {
                result = updateSearchChildPoi(searchResult);
            }
            case SEARCH_POI_CENTRAL -> {
                result = updateSearchCentralPoi(searchResult);
            }
            case SEARCH_POI_BEGIN_END -> {
                result = updateSearchBeginEndPoi(searchResult);
            }
            case SEARCH_POI_ALONG_ROUTE -> {
                result = updateSearchAlongRoutePoi(searchResult);
                updateSearchAlongRoutePoiPop(searchResult);
            }
            case SEARCH_PARENT_PARK -> {
                updateSearchResult(searchResult);
                result = updateSearchParkPoi(searchResult);
            }
            case SEARCH_POI_LABEL -> {
                result = updateSearchPoiLabel(searchResult);
            }
            case SEARCH_PARENT_CHARGE_STATION -> {
                result = updateSearchChargeStation(searchResult);
            }
        }
        return result;
    }

    /* 搜索路线图层业务 */
    public boolean updateSearchLine(ArrayList<ArrayList<GeoPoint>> roadPolygonBounds, String typeId) {
        if (ConvertUtils.isEmpty(roadPolygonBounds)) {
            Logger.e(TAG, "updateSearchLine poiAoiBounds == null");
            return false;
        }
        Logger.d(TAG, "updateSearchLine roadPolygonBounds " + roadPolygonBounds.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeLine, true);
        ArrayList<BizLineBusinessInfo> pointListBl = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < roadPolygonBounds.size(); i++) {
            ArrayList<GeoPoint> geoPoints = roadPolygonBounds.get(i);
            if (ConvertUtils.isEmpty(geoPoints)) {
                continue;
            }
            BizLineBusinessInfo info = new BizLineBusinessInfo();
            info.id = typeId;
            for (int j = NumberUtils.NUM_0; j < geoPoints.size(); j++) {
                Coord3DDouble coord3DDouble = new Coord3DDouble();
                coord3DDouble.lon = geoPoints.get(j).getLon();
                coord3DDouble.lat = geoPoints.get(j).getLat();
                info.mVecPoints.add(coord3DDouble);
            }
            Logger.d(TAG, "updateSearchLine i " + i + " info.size " + info.mVecPoints.size());
            pointListBl.add(info);
        }
        Logger.d(TAG, "updateSearchLine pointListBl " + pointListBl.size());
        getLayerSearchControl().updateSearchLine(pointListBl);
        return true;
    }

    /* 搜索区域图层业务，多区域面 */
    public boolean updateSearchPolygon(ArrayList<ArrayList<GeoPoint>> poiAoiBounds) {
        if (ConvertUtils.isEmpty(poiAoiBounds)) {
            Logger.e(TAG, "updateSearchPolygon poiAoiBounds == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolygon, true);
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolyline, true);
        BizPolygonBusinessInfo polygonBusinessInfo = new BizPolygonBusinessInfo();
        ArrayList<GeoPoint> geoPointArrayList = poiAoiBounds.get(0);
        ArrayList<Coord3DDouble> pointListBl = new ArrayList<>();
        if (ConvertUtils.isEmpty(geoPointArrayList)) {
            Logger.d(TAG, "updateSearchPolygon geoPointArrayList is null");
            return false;
        }
        Logger.d(TAG, "updateSearchPolygon geoPointArrayList " + geoPointArrayList.size());
        for (int i = NumberUtils.NUM_0; i < geoPointArrayList.size(); i++) {
            GeoPoint geoPoint = geoPointArrayList.get(i);
            if (ConvertUtils.isEmpty(geoPoint)) {
                continue;
            }
            Coord3DDouble coord3DDouble = new Coord3DDouble();
            coord3DDouble.lon = geoPoint.getLon();
            coord3DDouble.lat = geoPoint.getLat();
            pointListBl.add(coord3DDouble);
        }
        polygonBusinessInfo.mDrawPolygonRim = true;
        polygonBusinessInfo.mVecPoints = pointListBl;
        Logger.d(TAG, "updateSearchPolygon pointListBl " + pointListBl.size());
        getLayerSearchControl().updateSearchPolygon(polygonBusinessInfo);
        return true;
    }

    /* 搜索POI父点图层业务*/
    public boolean updateSearchParentPoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchParentPoi searchResult == null");
            return false;
        }
        //画父点
        ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parentList)) {
            Logger.e(TAG, "updateSearchParentPoi parentList == null");
            return false;
        }
        Logger.d(TAG, "updateSearchParentPoi parentList " + parentList.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> parentPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < parentList.size(); i++) {
            PoiInfoEntity poiInfoEntity = parentList.get(i);
            if (ConvertUtils.isEmpty(poiInfoEntity)) {
                Logger.e(TAG, "updateSearchParentPoi poiInfoEntity index " + i + " is null");
                continue;
            }
            int pointTypeCode = getPointTypeCode(poiInfoEntity.getPointTypeCode());
            int poiType = poiInfoEntity.getPoiType();
            Logger.d(TAG, "poiInfoEntity pointTypeCode " + poiInfoEntity.getPointTypeCode() +
                    " pointTypeCode " + pointTypeCode + " poiType " + poiType + " pid " + poiInfoEntity.getPid());
            BizSearchParentPoint parent = new BizSearchParentPoint();
            parent.poiName = poiInfoEntity.getName();
            parent.poiType = pointTypeCode;
            parent.mPos3D.lat = poiInfoEntity.getPoint().getLat();
            parent.mPos3D.lon = poiInfoEntity.getPoint().getLon();
            // TODO: 2025/3/24
//            parent.markerBGRes = "";
            parentPoints.add(parent);
        }
        boolean updateSearchParentPoi = getLayerSearchControl().updateSearchParentPoi(parentPoints);
        Logger.d(TAG, "updateSearchParentPoi " + updateSearchParentPoi + " parentPoints " + parentPoints.size());
        updateSearchResult(searchResult);
        return updateSearchParentPoi;
    }

    /* 搜索POI子节点图层业务 */
    public boolean updateSearchChildPoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchChildPoi searchResult == null");
            return false;
        }
        //画子点
        ArrayList<PoiInfoEntity> searchResultList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(searchResultList)) {
            Logger.e(TAG, "updateSearchChildPoi searchChildList == null");
            return false;
        }
        PoiInfoEntity poiInfoEntity = searchResultList.get(NumberUtils.NUM_0);
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.e(TAG, "updateSearchChildPoi poiInfoEntity == null");
            return false;
        }
        List<ChildInfo> searchChildList = poiInfoEntity.getChildInfoList();
        if (ConvertUtils.isEmpty(searchChildList)) {
            Logger.e(TAG, "updateSearchChildPoi searchChildList == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiChildPoint, true);
        ArrayList<BizSearchChildPoint> childPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < searchChildList.size(); i++) {
            ChildInfo childInfo = searchChildList.get(i);
            BizSearchChildPoint childPoint = new BizSearchChildPoint();
            childPoint.childType = childInfo.getChildType();
            childPoint.shortName = childInfo.getShortName();
            childPoint.mPos3D.lon = childInfo.getLocation().getLon();
            childPoint.mPos3D.lat = childInfo.getLocation().getLat();
            childPoints.add(childPoint);
        }
        boolean updateSearchChildPoi = getLayerSearchControl().updateSearchChildPoi(childPoints);
        Logger.d(TAG, "updateSearchChildPoi " + updateSearchChildPoi + " childPoints " + childPoints.size());
        return updateSearchChildPoi;
    }

    /* 搜索POI中心点图层业务 */
    public boolean updateSearchCentralPoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchCentralPoi searchResult == null");
            return false;
        }
        //画中心点
        ArrayList<PoiInfoEntity> parentPoints = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parentPoints)) {
            Logger.e(TAG, "updateSearchCentralPoi parentPoints == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiCentralPos, true);
        ArrayList<BizPointBusinessInfo> centerPoints = new ArrayList<>();
        BizPointBusinessInfo center = new BizPointBusinessInfo();
        PoiInfoEntity poiInfoEntity = parentPoints.get(NumberUtils.NUM_0);
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.e(TAG, "updateSearchCentralPoi poiInfoEntity == null");
            return false;
        }
        GeoPoint point = poiInfoEntity.getPoint();
        center.mPos3D.lon = point.getLon();
        center.mPos3D.lat = point.getLat();
        centerPoints.add(center);
        return getLayerSearchControl().updateSearchCentralPoi(centerPoints);
    }

    /* 搜索POI出入口图层业务*/
    public boolean updateSearchExitEntrancePoi(LayerItemSearchResult searchResult) {
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiExitEntrance, true);
        ArrayList<BizSearchExitEntrancePoint> pointListBl = new ArrayList<>();
        return getLayerSearchControl().updateSearchExitEntrancePoi(pointListBl);
    }

    /* 搜索POI起点、终点、途经点图层业务 */
    public boolean updateSearchBeginEndPoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchBeginEndPoi searchResult == null");
            return false;
        }
        ArrayList<PoiInfoEntity> searchResultPoints = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(searchResultPoints)) {
            Logger.e(TAG, "updateSearchBeginEndPoi searchResultPoints == null");
            return false;
        }
        ArrayList<BizSearchBeginEndPoint> bizSearchBeginEndPoints = new ArrayList<>();
        for (PoiInfoEntity poi : searchResultPoints) {
            BizSearchBeginEndPoint bizSearchBeginEndPoint = new BizSearchBeginEndPoint();
            bizSearchBeginEndPoint.pointType = poi.getPoiType();
            bizSearchBeginEndPoint.mPos3D.lat = poi.getPoint().getLat();
            bizSearchBeginEndPoint.mPos3D.lon = poi.getPoint().getLon();
        }
        boolean result = getLayerSearchControl().updateSearchBeginEndPoi(bizSearchBeginEndPoints);
        Logger.d(TAG, "updateSearchBeginEndPoi result " + result +
                " bizSearchBeginEndPoints " + bizSearchBeginEndPoints.size());
        return result;
    }

    /* 沿途搜索图层业务 */
    public boolean updateSearchAlongRoutePoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchAlongRoutePoi searchResult == null");
            return false;
        }
        //画沿途搜扎点
        ArrayList<PoiInfoEntity> parentPoints = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parentPoints)) {
            Logger.e(TAG, "updateSearchAlongRoutePoi parentPoints == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoute, true);
        //开启碰撞
        getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypePoiAlongRoute).enableCollision(true);

        ArrayList<BizSearchAlongWayPoint> alongWayPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < parentPoints.size(); i++) {
            PoiInfoEntity poi = parentPoints.get(i);
            if (ConvertUtils.isEmpty(poi)) {
                Logger.e(TAG, "updateSearchAlongRoutePoi poi is empty");
                continue;
            }
            BizSearchAlongWayPoint viaPoint = new BizSearchAlongWayPoint();
            viaPoint.mPos3D.lat = poi.getPoint().getLat();
            viaPoint.mPos3D.lon = poi.getPoint().getLon();
            //viaPoint.travelTime = poi.eta_to_via;
            // TODO: 2025/3/26  labelType无此字段
            viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeNone;
            viaPoint.name = poi.getName();
            //自定义标签名
            viaPoint.labelName = "";
            int pointTypeCode = getAlongRouteTypeCode(poi.getPointTypeCode());
            Logger.d(TAG, "updateSearchAlongRoutePoi pointTypeCode " + pointTypeCode);
            switch (pointTypeCode) {
                //填充沿途搜充电站数据
                case LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CHARGE -> {
                    List<ChargeInfo> chargeInfoList = poi.getChargeInfoList();
                    if (!ConvertUtils.isEmpty(chargeInfoList)) {
                        ChargeInfo chargeInfo = chargeInfoList.get(NumberUtils.NUM_0);
                        if (!ConvertUtils.isEmpty(chargeInfo)) {
                            viaPoint.mExtraData.chargeStationInfo.fastFree = chargeInfo.getMFastFree();
                            viaPoint.mExtraData.chargeStationInfo.fastTotal = chargeInfo.getMFastTotal();
                            viaPoint.mExtraData.chargeStationInfo.slowFree = chargeInfo.getMSlowFree();
                            viaPoint.mExtraData.chargeStationInfo.slowTotal = chargeInfo.getMSlowTotal();
                        } else {
                            Logger.e(TAG, "updateSearchAlongRoutePoi pointTypeCode " + pointTypeCode +
                                    " chargeInfo == null");
                        }
                    } else {
                        Logger.e(TAG, "updateSearchAlongRoutePoi pointTypeCode " + pointTypeCode +
                                " chargeInfoList is Empty");
                    }
                }
            }
            viaPoint.typeCode = pointTypeCode;
            alongWayPoints.add(viaPoint);
        }
        boolean result = getLayerSearchControl().updateSearchAlongRoutePoi(alongWayPoints);
        Logger.d(TAG, "updateSearchAlongRoutePoi result " + result +
                " alongWayPoints " + alongWayPoints.size());
        return result;
    }

    /* 沿途搜索气泡图层业务 */
    public boolean updateSearchAlongRoutePoiPop(LayerItemSearchResult searchResult) {
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoutePop, true);
        ArrayList<BizSearchAlongWayPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchAlongRoutePoiPop(pointListBl);
    }

    /**
     * 获取POI 类型
     *
     * @param typeCode poi的typeCode
     * @return POI类型
     */
    public int getPointTypeCode(final String typeCode) {
        if (ConvertUtils.isEmpty(typeCode)) {
            return LayerSearchPOIType.SEARCH_NONE;
        }
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return LayerSearchPOIType.SEARCH_GAS;
            }
            if (code.startsWith("0111")) {
                return LayerSearchPOIType.SEARCH_CHARGE_STATION;
            }
            if ("010500".equals(code)) {
                return LayerSearchPOIType.SEARCH_CAR_WASH;
            }
            if (code.startsWith("05")) {
                return LayerSearchPOIType.SEARCH_DELICIOUS_FOOD;
            }
            if (code.startsWith("11")) {
                return LayerSearchPOIType.SEARCH_SCENIC_SPOT;
            }
            if (code.startsWith("1509")) {
                return LayerSearchPOIType.SEARCH_PARK;
            }
        }
        return LayerSearchPOIType.SEARCH_NONE;
    }

    /**
     * 获取沿途搜POI 类型
     *
     * @param typeCode poi的typeCode
     * @return POI类型
     */
    public int getAlongRouteTypeCode(final String typeCode) {
        if (ConvertUtils.isEmpty(typeCode)) {
            return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_NONE;
        }
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_GAS;
            }
            if (code.startsWith("0111")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CHARGE;
            }
            if ("010500".equals(code)) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CAR_WASH;
            }
            if (code.startsWith("05")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_FOOD;
            }
            if (code.startsWith("1509")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_PARK;
            }
            if (code.startsWith("1803")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_REST_AREA;
            }
            if (code.startsWith("11")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_SCENIC_SPOT;
            }
            if (code.startsWith("15")) {
                return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_ROAD_INFO;
            }
        }
        return LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_NONE;
    }

    /* 停车场图层业务 */
    public boolean updateSearchParkPoi(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchParkPoi searchResult == null");
            return false;
        }
        List<PoiInfoEntity> parkingInfoList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parkingInfoList)) {
            Logger.e(TAG, "updateSearchParkPoi parkingInfoList == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParkRoute, true);
        //开启碰撞
        getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypePoiParkRoute).enableCollision(true);

        //画停车场
        ArrayList<BizPointBusinessInfo> parkPoints = new ArrayList<>();
        for (PoiInfoEntity poiInfoEntity : parkingInfoList) {
            BizPointBusinessInfo park = new BizPointBusinessInfo();
            Coord2DDouble poiLoc = new Coord2DDouble();
            poiLoc.lat = poiInfoEntity.getMPoint().getLat();
            poiLoc.lon = poiInfoEntity.getMPoint().getLon();
            park.mPos3D.lat = poiLoc.lat;
            park.mPos3D.lon = poiLoc.lon;
            parkPoints.add(park);
        }
        boolean result = getLayerSearchControl().updateSearchParkPoi(parkPoints);
        Logger.d(TAG, "updateSearchParkPoi result " + result + " parkPoints " + parkPoints.size());
        return result;
    }

    /* POI扎标图层业务 */
    public boolean updateSearchPoiLabel(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchPoiLabel searchResult == null");
            return false;
        }
        ArrayList<PoiInfoEntity> searchResultPoints = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(searchResultPoints)) {
            Logger.e(TAG, "updateSearchPoiLabel searchResultPoints == null");
            return false;
        }
        PoiInfoEntity poiInfoEntity = searchResultPoints.get(NumberUtils.NUM_0);
        if (poiInfoEntity == null) {
            Logger.e(TAG, "updateSearchPoiLabel poiInfoEntity == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiLabel, true);
        // 新的扎点开始设置
        BizPointBusinessInfo bizBusinessInfos = new BizPointBusinessInfo();
        bizBusinessInfos.mPos3D = new Coord3DDouble(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat(), poiInfoEntity.getPoint().getZ());
        bizBusinessInfos.mTypeCode = poiInfoEntity.getTypeCode();

        boolean result = getLayerSearchControl().updateSearchPoiLabel(bizBusinessInfos);
        Logger.d(TAG, "updateSearchPoiLabel result " + result);
        return result;
    }

    /* 充电桩扎标图层业务 */
    public boolean updateSearchChargeStation(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchChargeStation searchResult == null");
            return false;
        }
        List<PoiInfoEntity> chargeList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(chargeList)) {
            Logger.e(TAG, "updateSearchChargeStation parkingInfoList == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeChargeStation, true);
        //开启碰撞
        getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypeChargeStation).enableCollision(true);
        //画充电桩
        ArrayList<BizSearchChargeStationInfo> chargeStationInfos = new ArrayList<>();
        for (PoiInfoEntity poiInfoEntity : chargeList) {
            BizSearchChargeStationInfo chargeStation = new BizSearchChargeStationInfo();
            Coord2DDouble poiLoc = new Coord2DDouble();
            poiLoc.lat = poiInfoEntity.getMPoint().getLat();
            poiLoc.lon = poiInfoEntity.getMPoint().getLon();
            chargeStation.mPos3D.lat = poiLoc.lat;
            chargeStation.mPos3D.lon = poiLoc.lon;
            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
            if (!ConvertUtils.isEmpty(chargeInfoList) && !ConvertUtils.isEmpty(chargeInfoList.get(NumberUtils.NUM_0))) {
                ChargeInfo chargeInfo = chargeInfoList.get(NumberUtils.NUM_0);
                chargeStation.chargeStationInfo.fastTotal = chargeInfo.getMFastTotal();
                chargeStation.chargeStationInfo.fastFree = chargeInfo.getMFastFree();
                chargeStation.chargeStationInfo.slowTotal = chargeInfo.getMSlowTotal();
                chargeStation.chargeStationInfo.slowFree = chargeInfo.getMSlowFree();
                chargeStation.chargeStationInfo.brandDesc = chargeInfo.getMBrand();
            }
            chargeStationInfos.add(chargeStation);
        }
        boolean result = getLayerSearchControl().updateSearchChargeStation(chargeStationInfos);
        Logger.d(TAG, "updateSearchChargeStation result " + result +
                " chargeStationInfos " + chargeStationInfos.size());
        updateSearchResult(searchResult);
        return result;
    }

    /* 更新搜索结果数据 */
    private void updateSearchResult(LayerItemSearchResult result) {
        if (ConvertUtils.isEmpty(result)) {
            Logger.e(TAG, "updateSearchResult result == null");
            return;
        }
        Logger.d(TAG, "updateSearchResult");
        getStyleAdapter().updateSearchResult(result.getSearchResultPoints());
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(LayerPointItemType type, LayerItemSearchResult result) {
        if (ConvertUtils.isEmpty(result)) {
            Logger.e(TAG, "updateSearchResult result == null");
            return;
        }
        Logger.d(TAG, "updateSearchResult type " + type + " searchResultPoints " + result.getSearchResultPoints().size());
        getStyleAdapter().updateSearchResult(type, result.getSearchResultPoints());
    }

    /* 清除所有搜索扎标*/
    public void clearAllItems() {
        Logger.d(TAG, "LayerSearch -> clearAllItems");
        getLayerSearchControl().clearAllItems();
        getLayerSearchControl().setVisible(false);
    }

    /**
     * 清除指定搜索类型扎标
     */
    public void clearSearchItemByType(LayerPointItemType searchItemType) {
        Logger.d(TAG, "clearSearchItemByType searchItemType " + searchItemType);
        switch (searchItemType) {
            case SEARCH_PARENT_Line_Road -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypeLine);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeLine, false);
            }
            case SEARCH_PARENT_AREA -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiEndAreaPolygon);
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiEndAreaPolyline);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolygon, false);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolyline, false);
            }
            case SEARCH_PARENT_POINT -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiParentPoint);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, false);
            }
            case SEARCH_PARENT_PARK -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiParkRoute);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParkRoute, false);
            }
            case SEARCH_PARENT_CHARGE_STATION -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypeChargeStation);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeChargeStation, false);
            }
            case SEARCH_CHILD_POINT -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiChildPoint);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiChildPoint, false);
            }
            case SEARCH_POI_CENTRAL -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiCentralPos);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiCentralPos, false);
            }
            case SEARCH_POI_BEGIN_END -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiBeginEnd);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiBeginEnd, false);
            }
            case SEARCH_POI_ALONG_ROUTE -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiAlongRoute);
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiAlongRoutePop);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoute, false);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoutePop, false);
            }
            case SEARCH_POI_LABEL -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiLabel);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiLabel, false);
            }
        }
    }

}
