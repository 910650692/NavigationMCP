package com.fy.navi.service.adapter.layer.bls.impl;


import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
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
import com.autonavi.gbl.layer.model.PoiParentType;
import com.autonavi.gbl.layer.model.SearchAlongwayType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.PointLayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.SearchLayerStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemSearchBeginViaEnd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.route.RouteLinePoints;
import com.fy.navi.service.define.route.RoutePoint;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerSearchImpl extends BaseLayerImpl<SearchLayerStyleAdapter> {

    public LayerSearchImpl(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerSearchControl().setStyle(this);
        getLayerSearchControl().addClickObserver(this);
    }

    public void setSelect(GemLayerClickBusinessType type, String strID, boolean bFocus) {
        Logger.d(TAG, "setSelect", "strID:" + strID, "bFocus:" + bFocus);
        if (getLayerSearchControl() != null) {
            switch (type) {
                case BizSearchTypePoiParentPoint -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypePoiParentPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParentPoint:" + result);
                }
                case BizSearchTypePoiChildPoint -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypePoiChildPoint, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiChildPoint:" + result);
                }
            }
        }
    }

    @Override
    public void onNotifyClick(BaseLayer layer, LayerItem pItem, ClickViewIdInfo clickViewIds) {
        super.onNotifyClick(layer, pItem, clickViewIds);
        dispatchClick(pItem);
    }

    private void dispatchClick(LayerItem pItem) {
        List<ILayerAdapterCallBack> callBacks = getCallBacks();
        if (ConvertUtils.isEmpty(callBacks)) {
            Logger.e(TAG, "callBacks is null");
            return;
        }
        Logger.d(TAG, "dispatchClick " + callBacks.size());
        for (int i = NumberUtils.NUM_0; i < callBacks.size(); i++) {
            GemLayerItem clickResult = getClickResult(pItem);
            Logger.d(TAG, "dispatchClick clickResult:" + clickResult.toString());
            callBacks.get(i).onSearchItemClick(clickResult);
        }
    }

    private GemLayerItem getClickResult(LayerItem pItem) {
        GemLayerItem gemLayerItem = new GemLayerItem();
        switch (pItem.getBusinessType()) {
            // 搜索图层内容点击
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                Coord3DDouble coord3DDouble = ((SearchParentLayerItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setPid(pItem.getID());
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizSearchTypePoiParentPoint);
            }
            case BizSearchType.BizSearchTypePoiChildPoint -> {
                Coord3DDouble coord3DDouble = ((SearchChildLayerItem) pItem).getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setPid(pItem.getID());
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizSearchTypePoiChildPoint);
            }
            default -> {
                // TODO 扩展新的需求
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.UnKnown);
            }
        }
        return gemLayerItem;
    }

    @Override
    protected SearchLayerStyleAdapter createStyleAdapter() {
        return new SearchLayerStyleAdapter();
    }

    /* 搜索路线图层业务 */
    public boolean updateSearchLine(ArrayList<ArrayList<GeoPoint>> roadPolygonBounds) {
        if (ConvertUtils.isEmpty(roadPolygonBounds)) {
            Logger.e(TAG, "updateSearchParentPoi poiAoiBounds == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeLine, true);
        ArrayList<BizLineBusinessInfo> pointListBl = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < roadPolygonBounds.size(); i++) {
            ArrayList<GeoPoint> geoPoints = roadPolygonBounds.get(i);
            if (ConvertUtils.isEmpty(geoPoints)) {
                continue;
            }
            BizLineBusinessInfo info = new BizLineBusinessInfo();
            info.id = i + "";
            for (int j = NumberUtils.NUM_0; j < geoPoints.size(); j++) {
                Coord3DDouble coord3DDouble = new Coord3DDouble();
                coord3DDouble.lon = geoPoints.get(j).getLon();
                coord3DDouble.lat = geoPoints.get(j).getLat();
                info.mVecPoints.add(coord3DDouble);
            }
            pointListBl.add(info);
        }
        getLayerSearchControl().updateSearchLine(pointListBl);
        return true;
    }

    /* 搜索区域图层业务，多区域面 */
    public boolean updateSearchPolygon(ArrayList<ArrayList<GeoPoint>> poiAoiBounds) {
        if (ConvertUtils.isEmpty(poiAoiBounds)) {
            Logger.e(TAG, "updateSearchParentPoi poiAoiBounds == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolygon, true);
        ArrayList<BizPolygonBusinessInfo> pointListBl = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < poiAoiBounds.size(); i++) {
            ArrayList<GeoPoint> geoPoints = poiAoiBounds.get(i);
            if (ConvertUtils.isEmpty(geoPoints)) {
                continue;
            }
            BizPolygonBusinessInfo info = new BizPolygonBusinessInfo();
            info.id = i + "";
            for (int j = NumberUtils.NUM_0; j < geoPoints.size(); j++) {
                Coord3DDouble coord3DDouble = new Coord3DDouble();
                coord3DDouble.lon = geoPoints.get(j).getLon();
                coord3DDouble.lat = geoPoints.get(j).getLat();
                info.mVecPoints.add(coord3DDouble);
            }
            pointListBl.add(info);
        }
        getLayerSearchControl().updateSearchPolygon(pointListBl);
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
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> parentPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < parentList.size(); i++) {
            BizSearchParentPoint parent = new BizSearchParentPoint();
            PoiInfoEntity poiInfoEntity = parentList.get(i);
            Logger.d(TAG, "poiInfoEntity poiType:" + poiInfoEntity.getPoiType());
            parent.id = poiInfoEntity.getPid();
            parent.poiName = poiInfoEntity.getName();
            parent.poiType = getPointTypeCode(poiInfoEntity.getTypeCode());
            parent.mPos3D.lat = poiInfoEntity.getPoint().getLat();
            parent.mPos3D.lon = poiInfoEntity.getPoint().getLon();
            // TODO: 2025/3/24
//            parent.markerBGRes = "";
            parentPoints.add(parent);
        }
        //搜索路线、区域逻辑 此处需要等待点击事件添加完成后处理Focus态 暂且仅处理第一个index的数据
        PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
        if (!ConvertUtils.isEmpty(parentPoint)) {
            ArrayList<ArrayList<GeoPoint>> mPoiAoiBounds = parentPoint.getMPoiAoiBounds();
            if (!ConvertUtils.isEmpty(mPoiAoiBounds)) {
                updateSearchPolygon(mPoiAoiBounds);
            }
            ArrayList<ArrayList<GeoPoint>> mRoadPolygonBounds = parentPoint.getMRoadPolygonBounds();
            if (!ConvertUtils.isEmpty(mRoadPolygonBounds)) {
                updateSearchLine(mRoadPolygonBounds);
            }
        }
        return getLayerSearchControl().updateSearchParentPoi(parentPoints);
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
        int parentFocusIndex = searchResult.getParentFocusIndex();
        parentFocusIndex = parentFocusIndex == NumberUtils.NUM_ERROR ? NumberUtils.NUM_0 : parentFocusIndex;
        Logger.d(TAG, "parentFocusIndex " + parentFocusIndex);
        PoiInfoEntity poiInfoEntity = searchResultList.get(parentFocusIndex);
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
            childPoint.shortName = childInfo.getMName();
            childPoint.id = childInfo.getPoiId();
            childPoint.mPos3D.lon = childInfo.getLocation().getLon();
            childPoint.mPos3D.lat = childInfo.getLocation().getLat();
            childPoints.add(childPoint);
        }
        getLayerSearchControl().updateSearchChildPoi(childPoints);
        return true;
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

        // TODO: 2025/3/25  暂无此需求
        /**
         //画出入口
         ArrayList<BizSearchExitEntrancePoint> exitEntrancePoints = new ArrayList<>();
         ArrayList<Coord2DDouble> entrancesList = poiBase == null ? null : poiBase.entrances_list;
         int entranceListSize = entrancesList == null ? 0 : entrancesList.size();

         if (entranceListSize > 0) {
         for (int i2 = 0; i2 < entranceListSize; i2++) {
         Coord2DDouble item = entrancesList.get(i2);
         BizSearchExitEntrancePoint exitEntrancePoint = new BizSearchExitEntrancePoint();
         exitEntrancePoint.type = 2;
         exitEntrancePoint.mPos3D.lon = item.lon;
         exitEntrancePoint.mPos3D.lat = item.lat;
         exitEntrancePoints.add(exitEntrancePoint);
         }
         }

         ArrayList<Coord2DDouble> exitList = poiBase == null ? null : poiBase.exit_list;
         int exitListSize = exitList == null ? 0 : exitList.size();
         for (int i2 = 0; i2 < exitListSize; i2++) {
         Coord2DDouble item = exitList.get(i2);
         BizSearchExitEntrancePoint exitEntrancePoint = new BizSearchExitEntrancePoint();
         exitEntrancePoint.type = 1;
         exitEntrancePoint.mPos3D.lon = item.lon;
         exitEntrancePoint.mPos3D.lat = item.lat;
         exitEntrancePoints.add(exitEntrancePoint);
         }
         if (exitEntrancePoints.size() > 0) {
         bizSearchControl.updateSearchExitEntrancePoi(exitEntrancePoints);
         }
         */
        return getLayerSearchControl().updateSearchExitEntrancePoi(pointListBl);
    }

    /* 搜索POI起点、终点、途经点图层业务 */
    public boolean updateSearchBeginEndPoi(LayerItemSearchBeginViaEnd searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchBeginEndPoi searchResult == null");
            return false;
        }
        RouteLineLayerParam routeLineLayerParam = searchResult.getRouteLineLayerParam();
        if (ConvertUtils.isEmpty(routeLineLayerParam)) {
            Logger.e(TAG, "updateSearchBeginEndPoi routeLineLayerParam == null");
            return false;
        }
        RouteLinePoints routeLinePoints = routeLineLayerParam.getMRouteLinePoints();
        if (ConvertUtils.isEmpty(routeLinePoints)) {
            Logger.e(TAG, "updateSearchBeginEndPoi routeLinePoints == null");
            return false;
        }
        ArrayList<RoutePoint> startPoints = routeLinePoints.getMStartPoints();
        ArrayList<RoutePoint> endPoints = routeLinePoints.getMEndPoints();
        ArrayList<RoutePoint> viaPoints = routeLinePoints.getMViaPoints();
        if (ConvertUtils.isEmpty(startPoints) || ConvertUtils.isEmpty(endPoints)) {
            Logger.e(TAG, "updateSearchBeginEndPoi startPoints || endPoints is empty");
            return false;
        } else {
            Logger.d(TAG, "updateSearchBeginEndPoi startPoints:" + startPoints.size() +
                    " endPoints:" + endPoints.size());
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiBeginEnd, true);

        ArrayList<BizSearchBeginEndPoint> bizSearchBeginEndPoints = new ArrayList<>();
        //添加搜索起点
        BizSearchBeginEndPoint begin = new BizSearchBeginEndPoint();
        begin.pointType = 0;
        begin.pointCount = 1;
        begin.mPos3D.lat = startPoints.get(NumberUtils.NUM_0).getMPos().getLat();
        begin.mPos3D.lon = startPoints.get(NumberUtils.NUM_0).getMPos().getLon();
        bizSearchBeginEndPoints.add(begin);
        //添加搜索终点
        BizSearchBeginEndPoint end = new BizSearchBeginEndPoint();
        end.pointType = 1;
        end.pointCount = 1;
        end.mPos3D.lat = endPoints.get(NumberUtils.NUM_0).getMPos().getLat();
        end.mPos3D.lon = endPoints.get(NumberUtils.NUM_0).getMPos().getLon();
        bizSearchBeginEndPoints.add(end);
        //添加搜索途径点
        if (!ConvertUtils.isEmpty(viaPoints)) {
            Logger.d(TAG, "updateSearchBeginEndPoi viaPoints:" + viaPoints.size());
            for (int i = NumberUtils.NUM_0; i < viaPoints.size(); i++) {
                RoutePoint routePoint = viaPoints.get(i);
                BizSearchBeginEndPoint via = new BizSearchBeginEndPoint();
                via.pointType = 2;
                via.pointCount = 2;
                via.mPos3D.lat = routePoint.getMPos().getLat();
                via.mPos3D.lon = routePoint.getMPos().getLon();
                bizSearchBeginEndPoints.add(via);
            }
        } else {
            Logger.d(TAG, "updateSearchBeginEndPoi viaPoints is empty");
        }
        return getLayerSearchControl().updateSearchBeginEndPoi(bizSearchBeginEndPoints);
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

        ArrayList<BizSearchAlongWayPoint> alongWayPoints = new ArrayList<>();
        for (PoiInfoEntity poi : parentPoints) {
            BizSearchAlongWayPoint viaPoint = new BizSearchAlongWayPoint();
            viaPoint.id = poi.getPid();
            viaPoint.mPos3D.lat = poi.getPoint().getLat();
            viaPoint.mPos3D.lon = poi.getPoint().getLon();
            //viaPoint.travelTime = poi.eta_to_via;
            // TODO: 2025/3/26  labelType无此字段
            viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeNone;
            viaPoint.name = poi.getName();
            //自定义标签名
            viaPoint.labelName = "";
            int pointTypeCode = getSearchAlongWayType(getPointTypeCode(poi.getTypeCode()));
            viaPoint.searchType = pointTypeCode;
            alongWayPoints.add(viaPoint);
        }
        return getLayerSearchControl().updateSearchAlongRoutePoi(alongWayPoints);
    }

    /* 沿途搜索气泡图层业务 */
    public boolean updateSearchAlongRoutePoiPop(LayerItemSearchResult searchResult) {
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoutePop, true);
        ArrayList<BizSearchAlongWayPoint> pointListBl = new ArrayList<>();

        return getLayerSearchControl().updateSearchAlongRoutePoiPop(pointListBl);
    }

    public int getSearchAlongWayType(int type) {
        int searchAlongWayType = -1;
        switch (type) {
            case AutoMapConstant.PointTypeCode.GAS_STATION:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeGas;
                break;
            case AutoMapConstant.PointTypeCode.CHARGING_STATION:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeCharge;
                break;
            case AutoMapConstant.PointTypeCode.CAR_WASH:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeCarWash;
                break;
            case AutoMapConstant.PointTypeCode.CATERING:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeFood;
                break;
            case AutoMapConstant.PointTypeCode.PARKING_LOT:
            case AutoMapConstant.PointTypeCode.SERVICE_AREA:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeRestArea;
                break;
            case AutoMapConstant.PointTypeCode.SCENIC_SPOT:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeScenicSpot;
                break;
            case AutoMapConstant.PointTypeCode.TRANSPORT_HUB:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeRoadInfo;
                break;
            default:
                searchAlongWayType = SearchAlongwayType.SearchAlongwayTypeNone;
                break;
        }
        return searchAlongWayType;
    }

    /**
     * 获取POI 类型
     * @param typeCode poi的typeCode
     * @return POI类型
     */
    public int getPoiParentType(final String typeCode) {
        if (ConvertUtils.isEmpty(typeCode)) {
            return AutoMapConstant.PointTypeCode.OTHERS;
        }
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return PoiParentType.PoiParentTypeGas;
            }
            if (code.startsWith("0111")) {
                return PoiParentType.PoiParentTypeChargeStation;
            }
            if ("010500".equals(code)) {
                return PoiParentType.PoiParentTypeCarWash;
            }
            if (code.startsWith("05")) {
                return PoiParentType.PoiParentTypeDeliciousFood;
            }
            if (code.startsWith("1509")) {
                return PoiParentType.PoiParentTypeNone;
            }
            if (code.startsWith("1803")) {
                return AutoMapConstant.PointTypeCode.SERVICE_AREA;
            }
            if (code.startsWith("11")) {
                return AutoMapConstant.PointTypeCode.SCENIC_SPOT;
            }
            if (code.startsWith("15")) {
                return AutoMapConstant.PointTypeCode.TRANSPORT_HUB;
            }
        }
        return AutoMapConstant.PointTypeCode.OTHERS;
    }

    /**
     * 获取POI 类型
     * @param typeCode poi的typeCode
     * @return POI类型
     */
    public int getPointTypeCode(final String typeCode) {
        if (ConvertUtils.isEmpty(typeCode)) {
            return AutoMapConstant.PointTypeCode.OTHERS;
        }
        for (String code : typeCode.split("\\|")) {
            if (code.startsWith("0101")) {
                return AutoMapConstant.PointTypeCode.GAS_STATION;
            }
            if (code.startsWith("0111")) {
                return AutoMapConstant.PointTypeCode.CHARGING_STATION;
            }
            if ("010500".equals(code)) {
                return AutoMapConstant.PointTypeCode.CAR_WASH;
            }
            if (code.startsWith("05")) {
                return AutoMapConstant.PointTypeCode.CATERING;
            }
            if (code.startsWith("1509")) {
                return AutoMapConstant.PointTypeCode.PARKING_LOT;
            }
            if (code.startsWith("1803")) {
                return AutoMapConstant.PointTypeCode.SERVICE_AREA;
            }
            if (code.startsWith("11")) {
                return AutoMapConstant.PointTypeCode.SCENIC_SPOT;
            }
            if (code.startsWith("15")) {
                return AutoMapConstant.PointTypeCode.TRANSPORT_HUB;
            }
        }
        return AutoMapConstant.PointTypeCode.OTHERS;
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

        //画停车场
        ArrayList<BizPointBusinessInfo> parkPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < parkingInfoList.size(); i++) {
            PoiInfoEntity poiInfoEntity = parkingInfoList.get(i);
            BizPointBusinessInfo park = new BizPointBusinessInfo();
            Coord2DDouble poiLoc = new Coord2DDouble();
            poiLoc.lat = poiInfoEntity.getMPoint().getLat();
            poiLoc.lon = poiInfoEntity.getMPoint().getLon();
            if (poiLoc != null) {
                park.mPos3D.lat = poiLoc.lat;
                park.mPos3D.lon = poiLoc.lon;
            }
            parkPoints.add(park);
        }
        return getLayerSearchControl().updateSearchParkPoi(parkPoints);
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
        bizBusinessInfos.id = poiInfoEntity.getPid();
        bizBusinessInfos.mPos3D = new Coord3DDouble(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat(), poiInfoEntity.getPoint().getZ());
        bizBusinessInfos.mTypeCode = poiInfoEntity.getTypeCode();

        return getLayerSearchControl().updateSearchPoiLabel(bizBusinessInfos);
    }

    /* 充电桩扎标图层业务 */
    public boolean updateSearchChargeStation(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchChargeStation searchResult == null");
            return false;
        }
        List<PoiInfoEntity> parkingInfoList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parkingInfoList)) {
            Logger.e(TAG, "updateSearchChargeStation parkingInfoList == null");
            return false;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeChargeStation, true);

        //画充电桩
        ArrayList<BizSearchChargeStationInfo> parkPoints = new ArrayList<>();
        for (int i = NumberUtils.NUM_0; i < parkingInfoList.size(); i++) {
            PoiInfoEntity poiInfoEntity = parkingInfoList.get(i);
            BizSearchChargeStationInfo chargeStation = new BizSearchChargeStationInfo();
            Coord2DDouble poiLoc = new Coord2DDouble();
            poiLoc.lat = poiInfoEntity.getMPoint().getLat();
            poiLoc.lon = poiInfoEntity.getMPoint().getLon();
            if (poiLoc != null) {
                chargeStation.mPos3D.lat = poiLoc.lat;
                chargeStation.mPos3D.lon = poiLoc.lon;
            }
            parkPoints.add(chargeStation);
        }
        return getLayerSearchControl().updateSearchChargeStation(parkPoints);
    }

    /* 清除所有搜索扎标*/
    public void clearAllItems() {
        getLayerSearchControl().clearAllItems();
        getLayerSearchControl().setVisible(false);
    }

    /**
     * 清除搜索POI扎标
     */
    public void clearSearchItem() {
        getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiLabel);
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiLabel, false);
    }

}
