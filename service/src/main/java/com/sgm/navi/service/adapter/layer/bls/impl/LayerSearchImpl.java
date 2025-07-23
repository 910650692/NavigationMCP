package com.sgm.navi.service.adapter.layer.bls.impl;


import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
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
import com.autonavi.gbl.layer.model.BizSearchParentPoint;
import com.autonavi.gbl.layer.model.BizSearchType;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.sgm.navi.service.adapter.layer.bls.style.LayerSearchStyleAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemSearchResult;
import com.sgm.navi.service.define.layer.refix.LayerSearchAlongRouteType;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.layer.refix.LayerSearchPOIType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.ChildInfo;
import com.sgm.navi.service.define.search.LabelInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerSearchImpl extends BaseLayerImpl<LayerSearchStyleAdapter> {

    private static final String LINE_TYPE_ROAD = "Road";
    private static final String LINE_TYPE_PARK = "Park";
    private static final int MAX_NUM = 20; // 沿途搜最大扎标数量
    private boolean isShowParkPoint = false;

    private static final int MARKER_STYLE_DEFAULT = 0;  // 比例尺 <=12 时的样式
    private static final int MARKER_STYLE_DETAIL = 1; // 比例尺 >12 时的样式
    private float mLastMarkerStyle  = MARKER_STYLE_DEFAULT;

    public LayerSearchImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerSearchControl().setStyle(this);
        getLayerSearchControl().addClickObserver(this);
    }

    @Override
    protected LayerSearchStyleAdapter createStyleAdapter() {
        return new LayerSearchStyleAdapter(getEngineId(), getLayerSearchControl());
    }

    /* 更新比例尺 */
    public void updateMapLevel(float mapLevel) {
        //大于12是2公里以下
        if (Logger.openLog) {
            Logger.d(TAG, "mapLevel ", mapLevel, " isShowParkPoint ", isShowParkPoint);
        }
        int targetStyle;
        if (mapLevel <= 12) {
            targetStyle = MARKER_STYLE_DEFAULT;
        } else {
            targetStyle = MARKER_STYLE_DETAIL;
        }
        if (isShowParkPoint && targetStyle != mLastMarkerStyle) {
            getStyleAdapter().updateMapLevel(mapLevel);
            getLayerSearchControl().updateStyle(BizSearchType.BizSearchTypePoiParkRoute);
            mLastMarkerStyle = targetStyle;
        }
    }

    /**
     * 清除搜索图层选中状态
     *
     * @param type
     */
    public void clearFocus(LayerPointItemType type) {
        Logger.v(TAG, "clearFocus type " + type);
        if (getLayerSearchControl() != null) {
            long bizType = switch (type) {
                case SEARCH_PARENT_POINT -> BizSearchType.BizSearchTypePoiParentPoint;
                case SEARCH_CHILD_POINT -> BizSearchType.BizSearchTypePoiChildPoint;
                case SEARCH_PARENT_PARK -> BizSearchType.BizSearchTypePoiParkRoute;
                case SEARCH_PARENT_CHARGE_STATION -> BizSearchType.BizSearchTypeChargeStation;
                case SEARCH_POI_ALONG_ROUTE -> BizSearchType.BizSearchTypePoiAlongRoute;
                default -> BizSearchType.AUTO_UNKNOWN_ERROR;
            };
            getLayerSearchControl().clearFocus(bizType);
        }
    }

    public void setSelect(LayerPointItemType type, int index) {
        if (Logger.openLog) {
            Logger.d(TAG, "setSelect type " + type + " index " + index);
        }
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
                    LayerItem item = getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypePoiAlongRoute).getItem(String.valueOf(index));
                    if (item != null) {
                        int result = getLayerSearchControl().setFocus(
                                BizSearchType.BizSearchTypePoiAlongRoute, String.valueOf(index), true);
                        Logger.d(TAG, "setSelect-BizSearchTypePoiAlongRoute:" + result);
                    }
                }
                case SEARCH_POI_ALONG_ROUTE_LIST_SINGLE_POINT -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiParentPoint, String.valueOf(index), true);
                    Logger.d(TAG, "setSelect-SEARCH_POI_ALONG_ROUTE_LIST_SINGLE_POINT:" + result);
                }
                case SEARCH_POI_ALONG_ROUTE_ADD -> {
                    LayerItem item = getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypePoiAlongRoute).getItem(String.valueOf(index));
                    if (item != null) {
                        int result = getLayerSearchControl().setFocus(
                                BizSearchType.BizSearchTypePoiAlongRoute, String.valueOf(index), true);
                        Logger.d(TAG, "setSelect-SEARCH_POI_ALONG_ROUTE_ADD:" + result);
                    }
                    getLayerSearchControl().updateStyle(BizSearchType.BizSearchTypePoiAlongRoute);
                }
            }
        }
    }

    public void setSelect(LayerPointItemType type, int index, boolean select) {
        if (Logger.openLog) {
            Logger.d(TAG, "setSelect type ", type, " index ", index, " select ", select);
        }
        if (getLayerSearchControl() != null) {
            switch (type) {
                case SEARCH_PARENT_PARK -> {
                    int result = getLayerSearchControl().setFocus(
                            BizSearchType.BizSearchTypePoiParkRoute, String.valueOf(index), select);
                    Logger.d(TAG, "setSelect-BizSearchTypePoiParkRoute:" + result);
                }
                default -> {
                }
            }
        }
    }

    public void setSelect(LayerPointItemType type, int index, List<PoiInfoEntity> poiInfoEntities) {
        if (Logger.openLog) {
            Logger.d(TAG, "setSelect type ", type, " index ", index, " poiInfoEntities is null -> ", ConvertUtils.isEmpty(poiInfoEntities));
        }
        if (getLayerSearchControl() != null) {
            switch (type) {
                case SEARCH_POI_ALONG_ROUTE_ADD -> {
                    LayerItem item = getLayerSearchControl().getSearchLayer(BizSearchType.BizSearchTypePoiAlongRoute).getItem(String.valueOf(index));
                    if (item != null) {
                        Logger.d(TAG, "setSelect-SEARCH_POI_ALONG_ROUTE_ADD index ", index);
                        item.updateStyle();
                    }
                }
                default -> {
                }
            }
        }
    }

    @Override
    protected void dispatchItemClickEvent(LayerItem item, ClickViewIdInfo clickViewIds) {
        int index = 0;
        LayerPointItemType type = LayerPointItemType.NULL;
        if (clickViewIds != null && "icon_add_click".equals(clickViewIds.poiMarkerClickViewId)) {
            Logger.d(TAG, "icon_add_click");
            index = Integer.parseInt(item.getID());
            type = LayerPointItemType.SEARCH_POI_ALONG_ROUTE_ADD;
            // 统一从搜索列表走选中/未选中状态，避免调用两次
//            setSelect(LayerPointItemType.SEARCH_POI_ALONG_ROUTE_ADD, index);
        } else {
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
        }
        Logger.d(TAG, "dispatchItemClickEvent type = " + type + " ; index = " + index);
        if (getCallBack() != null) {
            getCallBack().onSearchItemClick(getMapType(), type, index);
        }
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(LayerPointItemType type, LayerItemSearchResult searchResult) {
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchMarker searchResult == null");
            return false;
        }
        if (Logger.openLog) {
            Logger.d(TAG, "updateSearchMarker type " + type);
        }
        boolean result = false;
        switch (type) {
            case SEARCH_PARENT_Line_Road -> {
                ArrayList<PoiInfoEntity> parentList = new ArrayList<>(searchResult.getSearchResultPoints());
                for (PoiInfoEntity parentPoint : parentList) {
                    result = (result || updateSearchLine(parentPoint.getMRoadPolygonBounds(), LINE_TYPE_ROAD));
                }
            }
            case SEARCH_PARENT_Line_Park -> {
                ArrayList<PoiInfoEntity> parentList = new ArrayList<>(searchResult.getSearchResultPoints());
                for (PoiInfoEntity parentPoint : parentList) {
                    result = (result || updateSearchLine(parentPoint.getMRoadPolygonBounds(), LINE_TYPE_PARK));
                }
            }
            case SEARCH_PARENT_AREA -> {
                ArrayList<PoiInfoEntity> parentList = new ArrayList<>(searchResult.getSearchResultPoints());
                for (PoiInfoEntity parentPoint : parentList) {
                    result = (result || updateSearchPolygon(parentPoint.getMPoiAoiBounds()));
                }
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
                getStyleAdapter().setHangingCardType(false);
                result = updateSearchAlongRoutePoi(searchResult);
            }
            case SEARCH_PARENT_PARK -> {
                result = updateSearchParkPoi(searchResult);
                isShowParkPoint = true;
            }
            case SEARCH_POI_LABEL -> {
                result = updateSearchPoiLabel(searchResult);
            }
            case SEARCH_PARENT_CHARGE_STATION -> {
                result = updateSearchChargeStation(searchResult);
            }
            case SEARCH_POI_ALONG_ROUTE_LIST_SINGLE_POINT -> {
                result = updateSearchAlongRouteListSinglePoint(searchResult);
            }
            case SEARCH_POI_HANGING_CARD_CHARGE_STATION -> {
                getStyleAdapter().setHangingCardType(true);
                result = updateSearchAlongRoutePoi(searchResult);
            }
        }
        return result;
    }

    /* 搜索路线图层业务 */
    public boolean updateSearchLine(ArrayList<ArrayList<GeoPoint>> roadPolygonBounds, String typeId) {
        boolean result = false;
        if (ConvertUtils.isEmpty(roadPolygonBounds)) {
            Logger.e(TAG, "updateSearchLine poiAoiBounds == null");
            return result;
        }
        Logger.v(TAG, "updateSearchLine roadPolygonBounds size =  " + roadPolygonBounds.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeLine, true);
        ArrayList<BizLineBusinessInfo> pointListBl = new ArrayList<>();
        for (ArrayList<GeoPoint> line : roadPolygonBounds) {
            BizLineBusinessInfo info = new BizLineBusinessInfo();
            info.id = typeId;
            for (GeoPoint geoPoint : line) {
                Coord3DDouble coord3DDouble = new Coord3DDouble(geoPoint.getLon(), geoPoint.getLat(), geoPoint.getZ());
                info.mVecPoints.add(coord3DDouble);
            }
            pointListBl.add(info);
        }
        if (pointListBl.size() > 20) {

        } else {
            getLayerSearchControl().updateSearchLine(pointListBl);
            result = true;
        }
        Logger.d(TAG, "updateSearchLine pointListBl size = " + pointListBl.size());
        return result;
    }

    /* 搜索区域图层业务，多区域面 */
    public boolean updateSearchPolygon(ArrayList<ArrayList<GeoPoint>> poiAoiBounds) {
        boolean result = false;
        if (ConvertUtils.isEmpty(poiAoiBounds)) {
            Logger.e(TAG, "updateSearchPolygon poiAoiBounds == null");
            return result;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolygon, true);
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiEndAreaPolyline, true);
        ArrayList<BizPolygonBusinessInfo> polygonBusinessInfos = new ArrayList<>();
        for (ArrayList<GeoPoint> line : poiAoiBounds) {
            BizPolygonBusinessInfo polygonBusinessInfo = new BizPolygonBusinessInfo();
            for (GeoPoint geoPoint : line) {
                Coord3DDouble coord3DDouble = new Coord3DDouble(geoPoint.getLon(), geoPoint.getLat(), geoPoint.getZ());
                polygonBusinessInfo.mVecPoints.add(coord3DDouble);
                polygonBusinessInfo.mDrawPolygonRim = true;
            }
            polygonBusinessInfos.add(polygonBusinessInfo);
        }

        if (polygonBusinessInfos.size() > 20) {

        } else {
            getLayerSearchControl().updateSearchPolygon(polygonBusinessInfos);
        }
        Logger.d(TAG, "updateSearchPolygon pointListBl size = " + polygonBusinessInfos.size());
        return result;
    }

    /* 搜索POI父点图层业务*/
    private synchronized boolean updateSearchParentPoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchParentPoi searchResult == null");
            return result;
        }
        //画父点
        ArrayList<PoiInfoEntity> parentList = new ArrayList<>(searchResult.getSearchResultPoints());
        if (ConvertUtils.isEmpty(parentList)) {
            Logger.e(TAG, "updateSearchParentPoi parentList == null");
            return result;
        }
        Logger.d(TAG, "updateSearchParentPoi parentList size = " + parentList.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> parentPoints = new ArrayList<>();
        for (PoiInfoEntity poiInfoEntity : parentList) {
            BizSearchParentPoint parent = new BizSearchParentPoint();
            parent.poiName = poiInfoEntity.getName();
            parent.poiType = getPointTypeCode(poiInfoEntity.getPointTypeCode());
            parent.mPos3D.lat = poiInfoEntity.getPoint().getLat();
            parent.mPos3D.lon = poiInfoEntity.getPoint().getLon();
            parentPoints.add(parent);
            Logger.d(TAG, "添加搜索结果点 的详情 =" + GsonUtils.toJson(parent));
        }
        getStyleAdapter().updateSearchResult(searchResult.getSearchResultPoints());
        if (parentPoints.size() > 20) {
            //优化扎点方式

        } else {
            result = getLayerSearchControl().updateSearchParentPoi(parentPoints);
        }
        Logger.e(TAG, "updateSearchParentPoi result = " + result + " parentPoints " + parentPoints.size());
        return result;
    }

    /* 沿途搜列表单独扎标 */
    private synchronized boolean updateSearchAlongRouteListSinglePoint(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchParentPoi searchResult == null");
            return result;
        }
        //画父点
        ArrayList<PoiInfoEntity> parentList = new ArrayList<>(searchResult.getSearchResultPoints());
        if (ConvertUtils.isEmpty(parentList)) {
            Logger.e(TAG, "updateSearchParentPoi parentList == null");
            return result;
        }
        Logger.d(TAG, "updateSearchParentPoi parentList size = " + parentList.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, true);
        ArrayList<BizSearchParentPoint> parentPoints = new ArrayList<>();
        for (PoiInfoEntity poiInfoEntity : parentList) {
            BizSearchParentPoint parent = new BizSearchParentPoint();
            parent.poiName = poiInfoEntity.getName();
            parent.poiType = getPointTypeCode(poiInfoEntity.getPointTypeCode());
            parent.mPos3D.lat = poiInfoEntity.getPoint().getLat();
            parent.mPos3D.lon = poiInfoEntity.getPoint().getLon();
            parentPoints.add(parent);
        }
        getStyleAdapter().updateSearchResult(searchResult.getSearchResultPoints());
        result = getLayerSearchControl().updateSearchParentPoi(parentPoints);
        Logger.e(TAG, "updateSearchParentPoi result = " + result + " parentPoints " + parentPoints.size());
        return result;
    }

    /* 搜索POI子节点图层业务 */
    private synchronized boolean updateSearchChildPoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchChildPoi searchResult == null");
            return result;
        }
        //画父点
        ArrayList<PoiInfoEntity> parentList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parentList)) {
            Logger.e(TAG, "updateSearchParentPoi parentList == null");
            return result;
        }
        //画子点
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiChildPoint, true);
        Logger.d(TAG, "updateSearchChildPoi parentList size =  " + parentList.size());
        ArrayList<BizSearchChildPoint> childPoints = new ArrayList<>();
        int index;
        for (PoiInfoEntity poiInfoEntity : parentList) {
            List<ChildInfo> childInfos = poiInfoEntity.getChildInfoList();
            if (ConvertUtils.isEmpty(childInfos)) {
                continue;
            }
            Logger.d(TAG, "updateSearchChildPoi childInfos each size =" + childInfos.size());
            index = 0;
            for (ChildInfo childInfo : childInfos) {
                BizSearchChildPoint childPoint = new BizSearchChildPoint();
                childPoint.id = String.valueOf(index);
                childPoint.childType = childInfo.getChildType();
                childPoint.shortName = childInfo.getShortName();
                childPoint.mPos3D.lon = childInfo.getLocation().getLon();
                childPoint.mPos3D.lat = childInfo.getLocation().getLat();
                childPoints.add(childPoint);
                index++;
            }
        }
        if (childPoints.size() > 20) {

        } else {
            result = getLayerSearchControl().updateSearchChildPoi(childPoints);
        }
        Logger.e(TAG, "updateSearchChildPoi " + result + " childPoints size =  " + childPoints.size());
        return result;
    }

    /* 搜索POI中心点图层业务 */
    private synchronized boolean updateSearchCentralPoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchCentralPoi searchResult == null");
            return result;
        }
        //画中心点
        ArrayList<PoiInfoEntity> parentPoints = new ArrayList<>(searchResult.getSearchResultPoints());
        if (ConvertUtils.isEmpty(parentPoints)) {
            Logger.e(TAG, "updateSearchCentralPoi parentPoints == null");
            return result;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiCentralPos, true);

        ArrayList<BizPointBusinessInfo> centerPoints = new ArrayList<>();

        for (PoiInfoEntity poiInfoEntity : parentPoints) {
            BizPointBusinessInfo center = new BizPointBusinessInfo();
            GeoPoint point = poiInfoEntity.getPoint();
            center.mPos3D.lon = point.getLon();
            center.mPos3D.lat = point.getLat();
            centerPoints.add(center);
        }

        if (centerPoints.size() > 20) {

        } else {
            result = getLayerSearchControl().updateSearchCentralPoi(centerPoints);
        }
        Logger.e(TAG, "updateSearchCentralPoi " + result + " centerPoints size =  " + centerPoints.size());
        return result;
    }

    /* 搜索POI起点、终点、途经点图层业务 */
    private synchronized boolean updateSearchBeginEndPoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchBeginEndPoi searchResult == null");
            return result;
        }
        ArrayList<PoiInfoEntity> searchResultPoints = new ArrayList<>(searchResult.getSearchResultPoints());
        if (ConvertUtils.isEmpty(searchResultPoints)) {
            Logger.e(TAG, "updateSearchBeginEndPoi searchResultPoints == null");
            return result;
        }
        ArrayList<BizSearchBeginEndPoint> bizSearchBeginEndPoints = new ArrayList<>();

        for (PoiInfoEntity poi : searchResultPoints) {
            BizSearchBeginEndPoint bizSearchBeginEndPoint = new BizSearchBeginEndPoint();
            bizSearchBeginEndPoint.pointType = poi.getPoiType();
            bizSearchBeginEndPoint.mPos3D.lat = poi.getPoint().getLat();
            bizSearchBeginEndPoint.mPos3D.lon = poi.getPoint().getLon();
            bizSearchBeginEndPoints.add(bizSearchBeginEndPoint);
        }

        if (bizSearchBeginEndPoints.size() > 20) {

        } else {
            result = getLayerSearchControl().updateSearchBeginEndPoi(bizSearchBeginEndPoints);
        }
        Logger.e(TAG, "updateSearchBeginEndPoi result " + result +
                " bizSearchBeginEndPoints " + bizSearchBeginEndPoints.size());
        return result;
    }

    /* 沿途搜索图层业务 */
    private synchronized boolean updateSearchAlongRoutePoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchAlongRoutePoi searchResult == null");
            return result;
        }
        //画沿途搜扎点
        ArrayList<PoiInfoEntity> parentPoints = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parentPoints)) {
            Logger.e(TAG, "updateSearchAlongRoutePoi parentPoints == null");
            return result;
        }

        Logger.d(TAG, "updateSearchAlongRoutePoi  parentPoints size " + parentPoints.size());
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiAlongRoute, true);

        ArrayList<BizSearchAlongWayPoint> alongWayPoints = new ArrayList<>();

        if (parentPoints.size() <= MAX_NUM) {
            int index = 0;
            for (PoiInfoEntity poiInfoEntity : parentPoints) {
                alongWayPoints.add(convertAlongWayPoint(poiInfoEntity, index));
                index++;
            }
        } else {
            // 1. 添加前4个点
            int totalSize = parentPoints.size();
            ArrayList<BizSearchAlongWayPoint> alongWayPointsHead = new ArrayList<>();
            for (int i = 0; i < 4; i++) {
                PoiInfoEntity poiInfoEntity = parentPoints.get(i);
                alongWayPointsHead.add(convertAlongWayPoint(poiInfoEntity, i));
            }
            for (BizSearchAlongWayPoint point : alongWayPointsHead) {
                alongWayPoints.add(point);
            }

            // 2. 添加后3个点
            ArrayList<BizSearchAlongWayPoint> alongWayPointsTail = new ArrayList<>();
            for (int i = totalSize - 3; i < totalSize; i++) {
                if (i >= 4) { // 避免与前4个点重复
                    PoiInfoEntity poiInfoEntity = parentPoints.get(i);
                    alongWayPointsTail.add(convertAlongWayPoint(poiInfoEntity, i));
                }
            }
            for (BizSearchAlongWayPoint point : alongWayPointsTail) {
                alongWayPoints.add(point);
            }

            // 3. 计算剩余需要抽取的点数和可分配的区间
            ArrayList<BizSearchAlongWayPoint> alongWayPointsMiddle = new ArrayList<>();
            int remainingPoints = MAX_NUM - alongWayPoints.size();
            int availableRangeStart = 4;
            int availableRangeEnd = totalSize - 4;
            int availableRangeSize = availableRangeEnd - availableRangeStart;
            Logger.d(TAG, "updateSearchAlongRoutePoi alongWayPoints.size ", alongWayPoints.size(), " availableRangeSize ", availableRangeSize);

            if (remainingPoints > 0) {
                // 计算平均间隔
                double interval = (double) availableRangeSize / (remainingPoints + 1);

                // 均匀间隔采样
                for (int i = 1; i <= remainingPoints; i++) {
                    int index = availableRangeStart + (int) Math.round(i * interval);
                    if (index < availableRangeEnd) {
                        PoiInfoEntity poiInfoEntity = parentPoints.get(index);
                        alongWayPointsMiddle.add(convertAlongWayPoint(poiInfoEntity, index));
                    }
                }
            }
            for (BizSearchAlongWayPoint point : alongWayPointsMiddle) {
                alongWayPoints.add(point);
            }
        }
        getLayerSearchControl().updateSearchAlongRoutePoi(alongWayPoints);
        Logger.d(TAG, "updateSearchAlongRoutePoi  alongWayPoints size " + alongWayPoints.size());
        return true;
    }

    /* 转换沿途搜数据 */
    private BizSearchAlongWayPoint convertAlongWayPoint(PoiInfoEntity poiInfoEntity, int index) {
        BizSearchAlongWayPoint viaPoint = new BizSearchAlongWayPoint();
        viaPoint.id = String.valueOf(index);
        viaPoint.mPos3D.lat = poiInfoEntity.getPoint().getLat();
        viaPoint.mPos3D.lon = poiInfoEntity.getPoint().getLon();
        viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeNone;
        viaPoint.name = poiInfoEntity.getName();
        viaPoint.labelName = "";
        int strTypeCode = getAlongRouteTypeCode(poiInfoEntity.getPointTypeCode());
        viaPoint.typeCode = strTypeCode;
        viaPoint.searchType = strTypeCode;
        switch (viaPoint.searchType) {
            case LayerSearchAlongRouteType.SEARCH_ALONG_ROUTE_CHARGE: {
                List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
                for (ChargeInfo chargeInfo : chargeInfoList) {
                    viaPoint.mExtraData.chargeStationInfo.fastFree = chargeInfo.getMFastFree();
                    viaPoint.mExtraData.chargeStationInfo.fastTotal = chargeInfo.getMFastTotal();
                    viaPoint.mExtraData.chargeStationInfo.slowFree = chargeInfo.getMSlowFree();
                    viaPoint.mExtraData.chargeStationInfo.slowTotal = chargeInfo.getMSlowTotal();
                    viaPoint.mExtraData.chargeStationInfo.brandDesc = chargeInfo.getMBrand();
                }
                List<LabelInfo> labelInfos = poiInfoEntity.getMLableList();
                if (!ConvertUtils.isEmpty(labelInfos)) {
                    for (LabelInfo labelInfo : labelInfos) {
                        switch (labelInfo.getMType()) {
                            case 1:
                                viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeBestWay;
                                break;
                            case 2:
                                viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeFastWay;
                                break;
                            case 3:
                                viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeExtraETA;
                                break;
                            default:
                                viaPoint.labelType = AlongWayLabelType.AlongWayLabelTypeNone;
                                break;
                        }
                        break;
                    }
                }
                break;
            }
        }
        return viaPoint;
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
    private synchronized boolean updateSearchParkPoi(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchParkPoi searchResult == null");
            return result;
        }
        List<PoiInfoEntity> parkingInfoList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(parkingInfoList)) {
            Logger.e(TAG, "updateSearchParkPoi parkingInfoList == null");
            return result;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParkRoute, true);
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
        getStyleAdapter().updateSearchResult(searchResult.getSearchResultPoints());

        if (parkPoints.size() > 20) {

        } else {
            result = getLayerSearchControl().updateSearchParkPoi(parkPoints);
        }
        Logger.d(TAG, "updateSearchParkPoi result " + result + " parkPoints " + parkPoints.size());
        return result;
    }

    /* POI扎标图层业务 */
    private boolean updateSearchPoiLabel(LayerItemSearchResult searchResult) {
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
    private synchronized boolean updateSearchChargeStation(LayerItemSearchResult searchResult) {
        boolean result = false;
        if (ConvertUtils.isEmpty(searchResult)) {
            Logger.e(TAG, "updateSearchChargeStation searchResult == null");
            return result;
        }
        List<PoiInfoEntity> chargeList = searchResult.getSearchResultPoints();
        if (ConvertUtils.isEmpty(chargeList)) {
            Logger.e(TAG, "updateSearchChargeStation parkingInfoList == null");
            return result;
        }
        getLayerSearchControl().setVisible(BizSearchType.BizSearchTypeChargeStation, true);
        //画充电桩
        ArrayList<BizSearchChargeStationInfo> chargeStationInfos = new ArrayList<>();
        for (PoiInfoEntity poiInfoEntity : chargeList) {
            BizSearchChargeStationInfo chargeStation = new BizSearchChargeStationInfo();
            chargeStation.mPos3D.lat = poiInfoEntity.getMPoint().getLat();
            chargeStation.mPos3D.lon = poiInfoEntity.getMPoint().getLon();
            List<ChargeInfo> chargeInfoList = poiInfoEntity.getChargeInfoList();
            for (ChargeInfo chargeInfo : chargeInfoList) {
                chargeStation.chargeStationInfo.fastTotal = chargeInfo.getMFastTotal();
                chargeStation.chargeStationInfo.fastFree = chargeInfo.getMFastFree();
                chargeStation.chargeStationInfo.slowTotal = chargeInfo.getMSlowTotal();
                chargeStation.chargeStationInfo.slowFree = chargeInfo.getMSlowFree();
                chargeStation.chargeStationInfo.brandDesc = chargeInfo.getMBrand();
            }
            chargeStationInfos.add(chargeStation);
        }
        getStyleAdapter().updateSearchResult(searchResult.getSearchResultPoints());
        Logger.e(TAG, "updateSearchChargeStation chargeStationInfos " + chargeStationInfos.size());
        if (chargeStationInfos.size() > 20) {

        } else {
            result = getLayerSearchControl().updateSearchChargeStation(chargeStationInfos);
        }

        return result;
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
        isShowParkPoint = false;
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
            case SEARCH_PARENT_POINT, SEARCH_POI_ALONG_ROUTE_LIST_SINGLE_POINT -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiParentPoint);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParentPoint, false);
            }
            case SEARCH_PARENT_PARK -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiParkRoute);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiParkRoute, false);
                isShowParkPoint = false;
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
                clearSearchItemByType(LayerPointItemType.SEARCH_PARENT_POINT);
            }
            case SEARCH_POI_LABEL -> {
                getLayerSearchControl().clearAllItems(BizSearchType.BizSearchTypePoiLabel);
                getLayerSearchControl().setVisible(BizSearchType.BizSearchTypePoiLabel, false);
            }
        }
    }

}
