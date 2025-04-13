package com.fy.navi.service.adapter.layer.bls.impl;


import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.layer.BizControlService;
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
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerSearchStyleAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerSearchItemType;
import com.fy.navi.service.define.layer.refix.LayerSearchPOIType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RouteLinePoints;
import com.fy.navi.service.define.route.RoutePoint;
import com.fy.navi.service.define.search.ChildInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class LayerSearchImpl extends BaseLayerImpl<LayerSearchStyleAdapter> {

    /**
     * 存放扎标数据
     * Key -> BizSearchType
     * Value -> LayerItemSearchResult
     */
    private static final Map<Integer, LayerItemSearchResult> sMarkerInfoMap = new ConcurrentHashMap<>();

    public LayerSearchImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
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
                case BizSearchTypePoiParkRoute -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypePoiParkRoute, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParkRoute:" + result);
                }
                case BizSearchTypeChargeStation -> {
                    int result = getLayerSearchControl().setFocus(BizSearchType.BizSearchTypeChargeStation, strID, bFocus);
                    Logger.d(TAG, "setSelect-BizSearchTypeChargeStation:" + result);
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
        Logger.d(TAG, "dispatchClick " + callBacks.size() + " BusinessType " + pItem.getBusinessType());
        for (int i = NumberUtils.NUM_0; i < callBacks.size(); i++) {
            LayerItemSearchResult clickResult = getClickResult(pItem);
            Logger.d(TAG, "dispatchClick clickResult:" + clickResult.toString());
            callBacks.get(i).onSearchItemClick(getMapType(), clickResult);
        }
    }

    private LayerItemSearchResult getClickResult(LayerItem pItem) {
        LayerItemSearchResult result = null;
        switch (pItem.getBusinessType()) {
            // 搜索图层内容点击
            case BizSearchType.BizSearchTypePoiParentPoint -> {
                int index = Integer.parseInt(pItem.getID());
                result = getMapDataByKey(BizSearchType.BizSearchTypePoiParentPoint, index);
                Logger.d(TAG, "getClickResult-BizSearchTypePoiParentPoint " + index);
            }
            case BizSearchType.BizSearchTypePoiChildPoint -> {
                int index = Integer.parseInt(pItem.getID());
                result = getMapDataByKey(BizSearchType.BizSearchTypePoiChildPoint, index);
                Logger.d(TAG, "getClickResult-BizSearchTypePoiChildPoint " + index);
            }
            case BizSearchType.BizSearchTypePoiParkRoute -> {
                int index = Integer.parseInt(pItem.getID());
                result = getMapDataByKey(BizSearchType.BizSearchTypePoiParkRoute, index);
                Logger.d(TAG, "getClickResult-BizSearchTypePoiParkRoute " + index);
            }
            case BizSearchType.BizSearchTypeChargeStation -> {
                int index = Integer.parseInt(pItem.getID());
                result = getMapDataByKey(BizSearchType.BizSearchTypeChargeStation, index);
                Logger.d(TAG, "getClickResult BizSearchTypeChargeStation " + index);
            }
            default -> {
                // TODO 扩展新的需求

            }
        }
        return result;
    }

    @Override
    protected LayerSearchStyleAdapter createStyleAdapter() {
        return new LayerSearchStyleAdapter(getEngineId(), getLayerSearchControl());
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchMarker searchResult == null");
            return false;
        }
        LayerSearchItemType type = searchResult.getType();
        Logger.d(TAG, "updateSearchMarker type " + type.ordinal());
        boolean result = false;
        switch (type) {
            case SEARCH_PARENT_Line -> {
                ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
                PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
                ArrayList<ArrayList<GeoPoint>> mRoadPolygonBounds = parentPoint.getMRoadPolygonBounds();
                result = updateSearchLine(mRoadPolygonBounds);
            }
            case SEARCH_PARENT_AREA -> {
                ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
                PoiInfoEntity parentPoint = parentList.get(NumberUtils.NUM_0);
                ArrayList<ArrayList<GeoPoint>> mPoiAoiBounds = parentPoint.getMPoiAoiBounds();
                result = updateSearchPolygon(mPoiAoiBounds);
            }
            case SEARCH_PARENT_POINT -> {
                result = updateSearchParentPoi(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiParentPoint, searchResult);
                }
            }
            case SEARCH_CHILD_POINT -> {
                result = updateSearchChildPoi(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiChildPoint, searchResult);
                }
            }
            case SEARCH_POI_CENTRAL -> {
                result = updateSearchCentralPoi(searchResult);
            }
            case SEARCH_POI_BEGIN_END -> {
                result = updateSearchBeginEndPoi(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiBeginEnd, searchResult);
                }
            }
            case SEARCH_POI_ALONG_ROUTE -> {
                result = updateSearchAlongRoutePoi(searchResult);
                updateSearchAlongRoutePoiPop(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiAlongRoute, searchResult);
                }
            }
            case SEARCH_PARENT_PARK -> {
                result = updateSearchParkPoi(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiParkRoute, searchResult);
                }
            }
            case SEARCH_POI_LABEL -> {
                result = updateSearchPoiLabel(searchResult);
            }
            case SEARCH_PARENT_CHARGE_STATION -> {
                result = updateSearchChargeStation(searchResult);
                if (result) {
                    sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiChildPoint, searchResult);
                }
            }
        }
        return result;
    }

    private LayerItemSearchResult getMapDataByKey(int key, int selectIndex) {
        LayerItemSearchResult result = null;
        result = sMarkerInfoMap.get(key);
        if (ConvertUtils.isEmpty(result)) {
            result = new LayerItemSearchResult();
            return result;
        }
        result.setSelectedIndex(selectIndex);
        return result;
    }

    /**
     * 删除扎标map数据
     */
    public boolean removeMapDataByKey(int key) {
        boolean removedIf = sMarkerInfoMap.entrySet().removeIf(entry -> entry.getKey() == key);
        Logger.d(TAG, "removeMapDataByKey " + removedIf);
        return removedIf;
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
        Logger.d(TAG, "updateSearchParentPoi parentList " + parentList.size());
        //此处区分是否充电站/停车场类型 需使用专用接口  后续全部替换为updateSearchMarker实现
        PoiInfoEntity entity = parentList.get(0);
        if (ConvertUtils.isEmpty(entity)) {
            Logger.e(TAG, "updateSearchParentPoi entity == null");
            return false;
        }
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
                    " pointTypeCode " + pointTypeCode + " poiType " + poiType);
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
        Logger.d(TAG, "updateSearchParentPoi " + updateSearchParentPoi);
        if (updateSearchParentPoi) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiParentPoint, searchResult);
        }
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
        int parentFocusIndex = searchResult.getSelectedIndex();
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
            childPoint.mPos3D.lon = childInfo.getLocation().getLon();
            childPoint.mPos3D.lat = childInfo.getLocation().getLat();
            childPoints.add(childPoint);
        }
        boolean updateSearchChildPoi = getLayerSearchControl().updateSearchChildPoi(childPoints);
        Logger.d(TAG, "updateSearchChildPoi " + updateSearchChildPoi);
        if (updateSearchChildPoi) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiChildPoint, searchResult);
        }
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
    public boolean updateSearchBeginEndPoi(LayerItemSearchResult searchResult) {
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
        end.mTypeCode = "11";
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
        boolean updateSearchBeginEndPoi = getLayerSearchControl().updateSearchBeginEndPoi(bizSearchBeginEndPoints);
        if (updateSearchBeginEndPoi) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiBeginEnd, searchResult);
        }
        return updateSearchBeginEndPoi;
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
            viaPoint.mPos3D.lat = poi.getPoint().getLat();
            viaPoint.mPos3D.lon = poi.getPoint().getLon();
            //viaPoint.travelTime = poi.eta_to_via;
            // TODO: 2025/3/26  labelType无此字段
            viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeNone;
            viaPoint.name = poi.getName();
            //自定义标签名
            viaPoint.labelName = "";
            int pointTypeCode = getPointTypeCode(poi.getTypeCode());
            viaPoint.searchType = pointTypeCode;
            alongWayPoints.add(viaPoint);
        }
        boolean updateSearchAlongRoutePoi = getLayerSearchControl().updateSearchAlongRoutePoi(alongWayPoints);
        Logger.d(TAG, "updateSearchAlongRoutePoi " + updateSearchAlongRoutePoi);
        if (updateSearchAlongRoutePoi) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiAlongRoute, searchResult);
        }
        return updateSearchAlongRoutePoi;
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
        boolean updateSearchParkPoi = getLayerSearchControl().updateSearchParkPoi(parkPoints);
        Logger.d(TAG, "updateSearchParkPoi " + updateSearchParkPoi + " parkPoints " + parkPoints.size());
        if (updateSearchParkPoi) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypePoiParkRoute, searchResult);
        }
        return updateSearchParkPoi;
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
        boolean searchChargeStation = getLayerSearchControl().updateSearchChargeStation(parkPoints);
        Logger.d(TAG, "updateSearchChargeStation " + searchChargeStation);
        if (searchChargeStation) {
            sMarkerInfoMap.put(BizSearchType.BizSearchTypeChargeStation, searchResult);
        }
        return searchChargeStation;
    }

    /* 清除所有搜索扎标*/
    public void clearAllItems() {
        getLayerSearchControl().clearAllItems();
        getLayerSearchControl().setVisible(false);
    }

    /**
     * 清除指定搜索类型扎标
     */
    public void clearSearchItemByType(LayerSearchItemType searchItemType) {
        Logger.d(TAG, "clearSearchItemByType searchItemType " + searchItemType.ordinal());
        switch (searchItemType) {
            case SEARCH_PARENT_Line -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypeLine);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeLine, false);
            }
            case SEARCH_PARENT_AREA -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiEndAreaPolygon);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolygon, false);
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
