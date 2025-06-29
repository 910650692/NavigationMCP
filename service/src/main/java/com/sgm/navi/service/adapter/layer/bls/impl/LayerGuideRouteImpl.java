package com.sgm.navi.service.adapter.layer.bls.impl;

import android.content.Context;
import android.graphics.Rect;
import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.common.model.RectDouble;
import com.autonavi.gbl.common.path.model.RestAreaInfo;
import com.autonavi.gbl.common.path.model.RoutePoint;
import com.autonavi.gbl.common.path.model.RoutePoints;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.BizGuideRouteControl;
import com.autonavi.gbl.layer.GuideLabelLayerItem;
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.RouteTrafficEventTipsLayerItem;
import com.autonavi.gbl.layer.model.BizEnergyKeyInfo;
import com.autonavi.gbl.layer.model.BizLocalTrafficEventInfo;
import com.autonavi.gbl.layer.model.BizOddInfo;
import com.autonavi.gbl.layer.model.BizPathInfoAttrs;
import com.autonavi.gbl.layer.model.BizRoadFacilityType;
import com.autonavi.gbl.layer.model.BizRouteDrawCtrlAttrs;
import com.autonavi.gbl.layer.model.BizRouteMapMode;
import com.autonavi.gbl.layer.model.BizRouteRestAreaInfo;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizRouteViaRoadInfo;
import com.autonavi.gbl.layer.model.BizRouteWeatherInfo;
import com.autonavi.gbl.layer.model.BizThreeUrgentInfo;
import com.autonavi.gbl.layer.model.DynamicLevelParam;
import com.autonavi.gbl.layer.model.DynamicLevelType;
import com.autonavi.gbl.layer.model.ODDDrawMode;
import com.autonavi.gbl.layer.model.RouteDrawStyle;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RoutePathLayer;
import com.autonavi.gbl.map.layer.model.RouteLayerScene;
import com.autonavi.gbl.map.model.PreviewParam;
import com.autonavi.gbl.route.model.WeatherLabelItem;
import com.sgm.navi.service.R;
import com.sgm.navi.service.adapter.layer.bls.style.LayerGuideRouteStyleAdapter;
import com.sgm.navi.service.adapter.layer.bls.texture.TexturePoolManager;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEnergyKey;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePathInfo;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteRestArea;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteThreeUrgent;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteViaRoad;
import com.sgm.navi.service.define.layer.refix.LayerItemTrafficEvent;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteChargeStationParam;
import com.sgm.navi.service.define.route.RouteLinePoints;
import com.sgm.navi.service.define.screen.ScreenType;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.service.define.search.ChargeInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.BevPowerCarUtils;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

public class LayerGuideRouteImpl extends BaseLayerImpl<LayerGuideRouteStyleAdapter> {

    private PreviewParam mPreviewParam;
    private ArrayList<PathInfo> mPathInfoList = new ArrayList<>();
    private ArrayList<RoutePoint> mViaList = new ArrayList<>();
    private RoutePoints mPathPoints;

    public LayerGuideRouteImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context, mapType);
        getLayerGuideRouteControl().setStyle(this);
        getLayerRoadCrossControl().setStyle(this);
        getLayerRoadFacilityControl().setStyle(this);
        getLayerGuideRouteControl().addClickObserver(this);
        getLayerRoadFacilityControl().addClickObserver(this);
        initDynamicLevel();
        initBizTypeVisible();
        setRouteJamBubblesVisible(true);
        setGuideTrafficVisible(true);
        setRouteEnergyEmptyPointVisible(true);
        Logger.d(TAG, "LayerGuideRouteImpl init");
    }

    @Override
    protected LayerGuideRouteStyleAdapter createStyleAdapter() {
        return new LayerGuideRouteStyleAdapter(getEngineId(), getLayerRoadCrossControl(), getLayerGuideRouteControl(), getLayerRoadFacilityControl(), getLayerLabelControl());
    }

    @Override
    protected void dispatchItemClickEvent(LayerItem item) {
        dispatchItemClick(item);
    }

    private void dispatchItemClick(LayerItem item) {
        LayerPointItemType type = LayerPointItemType.NULL;
        final LayerItemRoutePointClickResult result = new LayerItemRoutePointClickResult();
        switch (item.getBusinessType()) {
            case BizRouteType.BizRouteTypeStartPoint -> {
                //起点扎标无需下发点击事件
                return;
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                if (item instanceof RoutePathPointItem endPoint) {
                    int pathId = (int) endPoint.getPathId();
                    if (pathId == LayerPointItemType.ROUTE_POINT_END_PARK.ordinal()) {
                        type = LayerPointItemType.ROUTE_POINT_END_PARK;
                    } else {
                        //除终点停车场扎标外 其余终点类型扎标无需下发点击事件
                        return;
                    }
                    Coord3DDouble coord3DDouble = endPoint.getPosition();
                    if (!ConvertUtils.isEmpty(coord3DDouble)) {
                        result.setLat(coord3DDouble.lat);
                        result.setLog(coord3DDouble.lon);
                    }
                }
            }
            case BizRouteType.BizRouteTypeViaPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) item).getPosition();
                if (!ConvertUtils.isEmpty(coord3DDouble)) {
                    result.setLat(coord3DDouble.lat);
                    result.setLog(coord3DDouble.lon);
                }
                result.setIndex(Integer.parseInt(item.getID()));
                long pathId = ((RoutePathPointItem) item).getPathId();
                if (pathId == LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal()) {
                    //途经点点击-自定义替换补能点扎标
                    type = LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE;
                } else if (pathId == LayerPointItemType.ROUTE_POINT_VIA_CHARGE.ordinal()) {
                    //途经点点击-充电站扎标
                    type = LayerPointItemType.ROUTE_POINT_VIA_CHARGE;
                } else {
                    //途经点点击-默认扎标
                    type = LayerPointItemType.ROUTE_POINT_VIA;
                }
            }
            case BizRouteType.BizRouteTypePath -> {
                result.setIndex(Long.parseLong(item.getID()));
                type = LayerPointItemType.ROUTE_PATH;
            }
            case BizRouteType.BizRouteTypeWeather -> {
                result.setIndex(Long.parseLong(item.getID()));
                type = LayerPointItemType.ROUTE_POINT_WEATHER;
            }
            case BizRouteType.BizRouteTypeRestArea -> {
                result.setIndex(Long.parseLong(item.getID()));
                type = LayerPointItemType.ROUTE_POINT_REST_AREA;
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                result.setIndex(Integer.parseInt(item.getID()));
                type = LayerPointItemType.ROUTE_POINT_VIA_CHARGE_STATION;
            }
            case BizRouteType.BizRouteTypeTrafficEventTip -> {
                RouteTrafficEventTipsLayerItem trafficEventTipsLayerItem = (RouteTrafficEventTipsLayerItem) item;
                long eventID = trafficEventTipsLayerItem.getMTrafficEventTipsInfo().mTrafficIncident.ID;
                Coord3DDouble coord3DDouble = trafficEventTipsLayerItem.getPosition();
                if (!ConvertUtils.isEmpty(coord3DDouble)) {
                    result.setLat(coord3DDouble.lat);
                    result.setLog(coord3DDouble.lon);
                }
                result.setEventID(eventID);
                type = LayerPointItemType.ROUTE_POINT_TRAFFIC_EVENT;
            }
            case BizRouteType.BizRouteTypeGuideLabel -> {
                GuideLabelLayerItem guideLabelLayerItem = (GuideLabelLayerItem) item;
                int pathIndex = guideLabelLayerItem.getMAlterPathIndx();
                result.setIndex(pathIndex);
                result.setEventID(guideLabelLayerItem.getMPathId());
                type = LayerPointItemType.ROUTE_GUIDE_LABEL;
            }
        }
        Logger.d(TAG, "dispatchItemClick type = " + type + " ; result = " + result);
        if (getCallBack() != null) {
            getCallBack().onRouteItemClick(getMapType(), type, result);
        }
    }

    /*全览转换 */
    public RectDouble getPathResultBound() {
        if (ConvertUtils.isEmpty(mPathInfoList)) {
            Logger.e(TAG, "getPathResultBound mPathInfoList is Empty");
            return new RectDouble();
        }
        RectDouble rectDouble = BizGuideRouteControl.getPathResultBound(mPathInfoList);
        Logger.d(TAG, "path info 转换为预览巨型区域参数：", rectDouble);
        if (!ConvertUtils.isEmpty(rectDouble)) {
            return rectDouble;
        }
        return new RectDouble();
    }

    /* 路线全览 */
    public void showPreviewView() {
        String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        boolean isNaving = currentNaviStatus.equals(NaviStatus.NaviStatusType.NAVING);
        boolean isRoute = currentNaviStatus.equals(NaviStatus.NaviStatusType.ROUTING) || currentNaviStatus.contains(NaviStatus.NaviStatusType.SELECT_ROUTE);
        if (!isRoute && !isNaving) {
            if (Logger.openLog) {
                Logger.d(TAG, "have no path");
            }
            return;
        }
        if (ConvertUtils.isEmpty(mPreviewParam)) {
            mPreviewParam = new PreviewParam();
            mPreviewParam.mapBound = getPathResultBound();
            mPreviewParam.bUseRect = true;
            if (Logger.openLog) {
                Logger.d(TAG, "mPreviewParam init");
            }
        }
        int left;
        int right;
        int top;
        int bottom;
        ResourceUtils instance = ResourceUtils.Companion.getInstance();
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            left = instance.getDimensionPixelSize(R.dimen.margin_screen_left_one_to_three);
            right = instance.getDimensionPixelSize(R.dimen.margin_screen_right_one_to_three);
            top = instance.getDimensionPixelSize(R.dimen.margin_screen_top_one_to_three);
            bottom = instance.getDimensionPixelSize(R.dimen.margin_screen_bottom_one_to_three);
        } else {
            if (isNaving) {
                left = instance.getDimensionPixelSize(R.dimen.margin_screen_left);
                right = instance.getDimensionPixelSize(R.dimen.margin_screen_right);
                top = instance.getDimensionPixelSize(R.dimen.margin_screen_top);
                bottom = instance.getDimensionPixelSize(R.dimen.margin_screen_bottom);
            } else {
                left = instance.getDimensionPixelSize(R.dimen.route_margin_screen_left);
                right = instance.getDimensionPixelSize(R.dimen.route_margin_screen_right);
                top = instance.getDimensionPixelSize(R.dimen.route_margin_screen_top);
                bottom = instance.getDimensionPixelSize(R.dimen.route_margin_screen_bottom);
            }
        }
        Logger.d("screen_change_used", left,right,top,bottom);
        mPreviewParam.screenLeft = left;
        mPreviewParam.screenTop = top;
        mPreviewParam.screenRight = right;
        mPreviewParam.screenBottom = bottom;

        if (null != getMapView()) {
            getMapView().showPreview(mPreviewParam, true, 500, -1);
            if (Logger.openLog) {
                Logger.d(TAG, "showPreviewView mPreviewParam ", mPreviewParam);
            }
        } else {
            Logger.e(TAG, "getMapView == null");
        }
    }

    /**
     * 绘制路线以及路线上的元素.
     */
    private void updatePaths() {
        getLayerGuideRouteControl().updatePathArrow();
        setRouteJamBubblesVisible(true);
        setRouteEnergyEmptyPointVisible(true);
        int result = getLayerGuideRouteControl().updatePaths();
        Logger.d(TAG, "LayerGuideRouteImpl updatePaths result : " + result);
    }

    /**
     * 绘制路线
     *
     * @param routeResult
     */
    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        if (ConvertUtils.isEmpty(routeResult)) {
            Logger.e(TAG, "路线绘制参数为空，无法进行路线渲染");
            return;
        }
        Logger.d(TAG, "drawRouteLine ");
        //更新路线图层数据
        getStyleAdapter().updateRouteResult(routeResult);
        //设置路线信息
        setPathInfoByRouteResult(routeResult);
        //设置起点终点途经点
        setPathPoints(routeResult);
        //设置路线样式风格
        setMainMapPathDrawStyle(false, false, true);
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeEnergyRemainPoint, true);
        updatePaths();
        //设置全览  只对主屏生效
        if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {
            showPreviewView();
        }
    }

    /* 更新终点扎标数据 */
    public void updateRouteEndPoint(LayerItemRouteEndPoint endPoint) {
        Logger.d(TAG, "updateRoutePoints endPoint " + endPoint.toString());
        getStyleAdapter().updateRoutePoints(endPoint);
        getLayerGuideRouteControl().updateStyle(BizRouteType.BizRouteTypeEndPoint);
    }

    /**
     * 设置路线样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        setMainMapPathDrawStyle(isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 设置样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    private void setMainMapPathDrawStyle(boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        // 路线绘制风格
        RouteDrawStyle drawParam = new RouteDrawStyle();
        drawParam.mIsNavi = isStartNavi; // 是否导航 画导航路径时要设为true
        drawParam.mIsOffLine = isOffLine; // 是否离线
        drawParam.mRouteMapMode = BizRouteMapMode.BizRouteMapModeMain; // 图模式 主图 或 鹰眼
        drawParam.mRouteScene = RouteLayerScene.RouteLayerSceneNormal; // 路线业务场景 单指线
        drawParam.mIsMultipleMode = isMultipleMode; // 是否是多备选模式
        Logger.i(TAG, "设置主图路线风格 -> " + drawParam);
        getLayerGuideRouteControl().setPathDrawStyle(drawParam);
        getLayerGuideRouteControl().setPassGreyMode(true);
    }

    /**
     * 更新路线行程点信息.
     *
     * @param routeResult 路线图层参数
     */
    private void setPathPoints(RequestRouteResult routeResult) {
        if (ConvertUtils.isEmpty(routeResult.getMLineLayerParam()) ||
                ConvertUtils.isEmpty(routeResult.getMLineLayerParam().getMRouteLinePoints())) {
            Logger.e(TAG, "setPathPoints getMRouteLinePoints is Empty");
            return;
        }
        RouteLinePoints routeLinePoints = routeResult.getMLineLayerParam().getMRouteLinePoints();
        Logger.i(TAG, "设置路线行程点的信息 -> " + routeLinePoints);
        mPathPoints = getRoutePoints(routeLinePoints);
        int points = getLayerGuideRouteControl().setPathPoints(mPathPoints);
        Logger.d(TAG, "setPathPoints points " + points);
    }

    /* 途经点扎标设置是否选中 */
    public void setRouteViaPointSelectStatus(boolean isSelect, int index) {
        Logger.d(TAG, "isSelect ", isSelect, " index ", index);
        LayerItem item = getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).getItem(String.valueOf(index));
        if (ConvertUtils.isEmpty(item)) {
            return;
        }
        RoutePathPointItem viaPointItem = (RoutePathPointItem) item;
        long pathId = viaPointItem.getPathId();
        if (pathId == 0) {
            int result = getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).setFocus(String.valueOf(index), isSelect);
            Logger.d(TAG, "viaPoint setFocus result ",  result, " pathId ", pathId);
        }
    }

    /**
     * 更新途径点信息
     * @param viaPointList
     */
    public void updateViaPointList(List<PoiInfoEntity> viaPointList) {
        if (ConvertUtils.isNull(viaPointList)) {
            Logger.e(TAG, "via point list is null");
            return;
        }
        mPathPoints.mViaPoints.clear();
        for (PoiInfoEntity poiInfoEntity : viaPointList) {
            final RoutePoint routePoint = new RoutePoint();
            routePoint.mType = 2;
            routePoint.mIsDraw = true;
            routePoint.mPathId = isChargeStation(poiInfoEntity) ? LayerPointItemType.ROUTE_POINT_VIA_CHARGE.ordinal() : 0;
            routePoint.mPos = new Coord3DDouble(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat(), 0);
            mPathPoints.mViaPoints.add(routePoint);
        }
        mViaList = new ArrayList<>(mPathPoints.mViaPoints);
        getStyleAdapter().updateViaPointList(viaPointList);
        getLayerGuideRouteControl().setPathPoints(mPathPoints);
        updatePaths();
    }

    /**
     * 是否是充电站
     * @param poiInfo
     * @return
     */
    private boolean isChargeStation(PoiInfoEntity poiInfo) {
        String currentNaviStatus = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        if (!TextUtils.equals(currentNaviStatus, NaviStatus.NaviStatusType.NAVING)) {
            return false;
        }
        if (ConvertUtils.isEmpty(poiInfo) || ConvertUtils.isEmpty(poiInfo.getChargeInfoList())) {
            return false;
        }
        ChargeInfo chargeInfo = poiInfo.getChargeInfoList().get(0);
        return chargeInfo.getMFastTotal() > 0 || chargeInfo.getMSlowTotal() >= 0;
    }

    /* 路线替换补能扎标 */
    public void updateRouteReplaceChargePoints(ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        Logger.d(TAG, "updateRouteReplaceChargePoints");
        if (ConvertUtils.isEmpty(chargeStationInfos)) {
            Logger.e(TAG, "updateRouteReplaceChargePoints chargeStationInfos is Empty");
            return;
        }
        ArrayList<RouteAlterChargeStationInfo> chargeStationInfoList = new ArrayList<>(chargeStationInfos);
        Logger.d(TAG, "updateRouteReplaceChargePoints chargeStationInfos " + chargeStationInfoList.size());
        RoutePoints routePoints = getRouteViaReplaceChargePoints(chargeStationInfoList);
        //途经点数量
        int viaCount = 0;
        //填充途经点空数据
        if (routePoints.mViaPoints.size() != chargeStationInfoList.size()) {
            Logger.d(TAG, "updateRouteReplaceChargePoints mViaPoints " + routePoints.mViaPoints.size());
            ArrayList<RoutePoint> mViaPoints = routePoints.mViaPoints;
            for (int index = 0; index < mViaPoints.size(); index++) {
                RoutePoint routePoint = mViaPoints.get(index);
                if (routePoint.mPathId != LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal()) {
                    RouteAlterChargeStationInfo info = new RouteAlterChargeStationInfo();
                    chargeStationInfoList.add(index, info);
                    viaCount++;
                }
            }
        }
        int points = getLayerGuideRouteControl().setPathPoints(routePoints);
        Logger.d(TAG, "updateRouteReplaceChargePoints points " + points + " chargeStationInfoList.size " + chargeStationInfoList.size());
        getStyleAdapter().updateRouteReplaceChargeInfo(chargeStationInfoList, viaCount);
        getLayerGuideRouteControl().updatePaths();
    }

    /*自动添加的补能站数据*/
    public void updateRouteChargeStation(RouteChargeStationParam routeChargeStation) {
        getStyleAdapter().updateRouteChargeStation(routeChargeStation);
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaChargeStationPoint).updateStyle();
    }

    //转换替换补能扎标数据
    private RoutePoints getRouteViaReplaceChargePoints(ArrayList<RouteAlterChargeStationInfo> info) {
        RoutePoints infos = new RoutePoints();
        if (ConvertUtils.isEmpty(info)) {
            Logger.e(TAG, "getRouteViaReplaceChargePoints info is Empty");
            return infos;
        }
        ArrayList<RoutePoint> via = new ArrayList<>();
        //途径点扎标添加
        if (!ConvertUtils.isEmpty(mViaList)) {
            Logger.d(TAG, "getRouteViaReplaceChargePoints size " + mViaList.size());
            for (RoutePoint routePoint : mViaList) {
                RoutePoint point = new RoutePoint();
                point.mType = 2;
                point.mPos = new Coord3DDouble(routePoint.mPos.lon
                        , routePoint.mPos.lat
                        , routePoint.mPos.z);
                via.add(point);
            }
        }
        for (int t = 0; t < info.size(); t++) {
            RoutePoint point = new RoutePoint();
            point.mType = 2;
            point.mPathId = LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal();
            point.mPos = new Coord3DDouble(info.get(t).getMPos().getLon()
                    , info.get(t).getMPos().getLat()
                    , info.get(t).getMPos().getZ());
            via.add(point);
        }
        infos.mViaPoints = via;
        RoutePathPointItem startPoint = (RoutePathPointItem) getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeStartPoint).getItem("0");
        ArrayList<RoutePoint> startList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(startPoint)) {
            Coord3DDouble startPointPosition = startPoint.getPosition();
            RoutePoint point = new RoutePoint();
            point.mPos = startPointPosition;
            startList.add(point);
            infos.mStartPoints = startList;
        }
        RoutePathPointItem endPoint = (RoutePathPointItem) getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeEndPoint).getItem("0");
        ArrayList<RoutePoint> endList = new ArrayList<>();
        if (!ConvertUtils.isEmpty(endPoint)) {
            Coord3DDouble endPointPosition = endPoint.getPosition();
            RoutePoint point = new RoutePoint();
            point.mPos = endPointPosition;
            endList.add(point);
            infos.mEndPoints = endList;
        }
        Logger.d(TAG, "getRouteViaReplaceChargePoints routeReplaceChargePoints " + via.size());
        return infos;
    }

    /**
     * 删除途经点
     *
     * @param pid 途经点id
     */
    public void removeViaPoint(String pid) {
        if (ConvertUtils.isEmpty(pid)) {
            Logger.e(TAG, "removeViaPoint pid is null");
            return;
        }
        Logger.d(TAG, "removeViaPoint pid " + pid);
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).removeItem(pid);
    }

    private RoutePoints getRoutePoints(RouteLinePoints info) {
        RoutePoints infos = new RoutePoints();
        if (!ConvertUtils.isEmpty(info.getMStartPoints())) {
            int size = info.getMStartPoints().size();
            for (int t = 0; t < size; t++) {
                RoutePoint point = new RoutePoint();
                point.mIsDraw = info.getMStartPoints().get(t).isMIsDraw();
                point.mPathId = info.getMStartPoints().get(t).getMPathId();
                point.mType = info.getMStartPoints().get(t).getMType();
                point.mPos = new Coord3DDouble(info.getMStartPoints().get(t).getMPos().getLon()
                        , info.getMStartPoints().get(t).getMPos().getLat()
                        , info.getMStartPoints().get(t).getMPos().getZ());
                infos.mStartPoints.add(point);
            }
        }
        if (!ConvertUtils.isEmpty(info.getMViaPoints())) {
            int size = info.getMViaPoints().size();
            for (int t = 0; t < size; t++) {
                com.sgm.navi.service.define.route.RoutePoint routePoint = info.getMViaPoints().get(t);
                if (routePoint != null && routePoint.mAddressType != 1) {
                    RoutePoint point = new RoutePoint();
                    point.mIsDraw =routePoint.isMIsDraw();
                    point.mPathId = routePoint.getMPathId();
                    point.mType = routePoint.getMType();
                    point.mPos = new Coord3DDouble(routePoint.getMPos().getLon()
                        , routePoint.getMPos().getLat()
                        , routePoint.getMPos().getZ());
                    infos.mViaPoints.add(point);
                }
            }
        }
        if (!ConvertUtils.isEmpty(info.getMEndPoints())) {
            for (int t = 0; t < info.getMEndPoints().size(); t++) {
                RoutePoint point = new RoutePoint();
                point.mIsDraw = info.getMEndPoints().get(t).isMIsDraw();
                point.mPathId = info.getMEndPoints().get(t).getMPathId();
                point.mType = info.getMEndPoints().get(t).getMType();
                point.mPos = new Coord3DDouble(info.getMEndPoints().get(t).getMPos().getLon()
                        , info.getMEndPoints().get(t).getMPos().getLat()
                        , info.getMEndPoints().get(t).getMPos().getZ());
                infos.mEndPoints.add(point);
            }
        }
        mViaList = new ArrayList<>(infos.mViaPoints);
        return infos;
    }

    //更新路线上的箭头
    public void updatePathArrow() {
        Logger.d(TAG, "updatePathArrow");
        getLayerGuideRouteControl().updatePathArrow();
    }

    //设置路线是否对比模式
    public void setCompareRouteMode(boolean compareMode) {
        Logger.d(TAG, "setCompareRouteMode compareMode" + compareMode);
        getLayerGuideRouteControl().setCompareRouteMode(compareMode);
    }

    //更新电量关键点信息
    public void updateEnergyKeyInfo(ArrayList<LayerItemRouteEnergyKey> energyKeyInfos) {
        Logger.d(TAG, "updateEnergyKeyInfo energyKeyInfos" + energyKeyInfos);
        ArrayList<BizEnergyKeyInfo> energyKeyInfosBls = new ArrayList<BizEnergyKeyInfo>();
        getLayerGuideRouteControl().updateEnergyKeyInfo(energyKeyInfosBls);
    }

    /* 更新Odd信息 */
    public void updateOddInfo(ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        if (ConvertUtils.isEmpty(oddInfoList)) {
            Logger.e(TAG, "updateOddInfo oddInfoList is Empty");
            return;
        }
        //判断当前odd数据是否与当前选中路线绑定
        int selectedPathIndex = getLayerGuideRouteControl().getSelectedPathIndex();
        if (ConvertUtils.isEmpty(mPathInfoList)) {
            Logger.e(TAG, "updateOddInfo mPathInfoList is Empty");
            return;
        }
        if (selectedPathIndex < 0 || selectedPathIndex >= mPathInfoList.size()) {
            Logger.e(TAG, "updateOddInfo selectedPathIndex < 0 || selectedPathIndex >= mPathInfoList.size()");
            return;
        }
        PathInfo pathInfo = mPathInfoList.get(selectedPathIndex);
        long pathID = pathInfo.getPathID();
        Logger.d(TAG, "updateOddInfo pathID " + pathID + " pathId " + pathId);
        if (pathID != pathId) {
            Logger.e(TAG, "updateOddInfo pathID != pathId");
            return;
        }
        //判断当前是否导航态
        String currentNaviStatus = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        if (!TextUtils.equals(currentNaviStatus, NaviStatus.NaviStatusType.NAVING)) {
            Logger.e(TAG, "updateOddInfo 非导航态");
            return;
        }
        //转换odd数据
        ArrayList<BizOddInfo> oddInfoListBls = new ArrayList<>();
        for (LayerItemRouteOdd routeOdd : oddInfoList) {
            ArrayList<GeoPoint> vecPoints = routeOdd.getVecPoints();
            BizOddInfo oddInfo = new BizOddInfo();
            ArrayList<Coord3DDouble> bizVecPoints = new ArrayList<>();
            for (GeoPoint point : vecPoints) {
                Coord3DDouble coord3DDouble = new Coord3DDouble();
                coord3DDouble.lat = point.getLat();
                coord3DDouble.lon = point.getLon();
                bizVecPoints.add(coord3DDouble);
            }
            oddInfo.vecPoints = bizVecPoints;
            oddInfoListBls.add(oddInfo);
        }
        getLayerGuideRouteControl().setODDDrawMode(ODDDrawMode.ODDDrawModeStartEndPoint);
        Logger.d(TAG, "updateOddInfo oddInfoList" + oddInfoList.size() + " oddInfoListBls " + oddInfoListBls.size());
        getLayerGuideRouteControl().updateOddInfo(oddInfoListBls);
    }

    //更新避让路线图层
    public void updateRouteDodgeLine(int groupSegIndex) {
        Logger.d(TAG, "updateRouteDodgeLine groupSegIndex" + groupSegIndex);
        getLayerGuideRouteControl().updateRouteDodgeLine(groupSegIndex);
    }

    //更新服务区扎标信息
    public void updateRouteRestAreaInfo(ArrayList<LayerItemRouteRestArea> restAreaInfos) {
        Logger.d(TAG, "updateRouteRestAreaInfo restAreaInfos " + restAreaInfos);
        ArrayList<BizRouteRestAreaInfo> restAreaInfosBls = new ArrayList<BizRouteRestAreaInfo>();
        getLayerGuideRouteControl().updateRouteRestAreaInfo(restAreaInfosBls);
    }

    //更新途经路扎标信息
    public void updateRouteViaRoadInfo(ArrayList<LayerItemRouteViaRoad> viaRoadInfos) {
        Logger.d(TAG, "updateRouteViaRoadInfo viaRoadInfos " + viaRoadInfos.size());
        ArrayList<BizRouteViaRoadInfo> viaRoadInfosBls = new ArrayList<BizRouteViaRoadInfo>();
        getLayerGuideRouteControl().updateRouteViaRoadInfo(viaRoadInfosBls);
    }

    //设置彩虹线绘制范围，用于象限的计算
    public void updateThreeUrgentInfo(ArrayList<LayerItemRouteThreeUrgent> threeUrgentInfos) {
        Logger.d(TAG, "updateThreeUrgentInfo threeUrgentInfos " + threeUrgentInfos.size());
        ArrayList<BizThreeUrgentInfo> threeUrgentInfosBls = new ArrayList<BizThreeUrgentInfo>();
        getLayerGuideRouteControl().updateThreeUrgentInfo(threeUrgentInfosBls);
    }


    //显示第几个中途点ETA和剩余电量，默认显示第一个(如果等于-1，则显示全部途经点，包括服务器推荐的途经充电站)
    public void showViaETAByIndex(int viaIndex) {
        Logger.d(TAG, "showViaETAByIndex");
        getLayerGuideRouteControl().showViaETAByIndex(viaIndex);
    }


    //设置熟悉路线
    public void setFamiliarRoute(LayerItemRoutePathInfo pathInfo) {
        Logger.d(TAG, "setFamiliarRoute");
        PathInfo pathInfoBls = new PathInfo();
        getLayerGuideRouteControl().setFamiliarRoute(pathInfoBls);
    }

    //楼层切换
    public void setParkFloor(int index, int floor) {
        Logger.d(TAG, "setParkFloor index " + index + " floor " + floor);
        getLayerGuideRouteControl().setParkFloor(index, floor);
    }

    //初始化动态比例尺
    public void initDynamicLevel() {
        Logger.d(TAG, "initDynamicLevel");
        getLayerGuideRouteControl().initDynamicLevel(assembleDynamicLevelParam());
    }

    // TODO 参照现代的代码实现，如果有问题请适当修改
    private DynamicLevelParam assembleDynamicLevelParam() {
        DynamicLevelParam param = new DynamicLevelParam();
        param.mLongDisToNaviPoint = 600;
        param.mSpeedWayMinLevel2DCarUp = 14.0f;
        param.mSpeedWayMinLevel3DCarUp = 14.0f;
        param.mPitchFar3DCarUp = 50.0f;
        param.mNaviPointOffsetToScreenTop = GetDynamicLevelTopOffsetValue(false);
        return param;
    }

    /**
     * @return 导航点偏移位置
     * @brief 计算导航点偏移位置
     * @param[in] isShowCrossImage     是否显示路口图
     */
    private int GetDynamicLevelTopOffsetValue(boolean isShowCrossImage) {
        long screenWidth = getMapView().getMapviewPort().screenWidth;
        long screenHeight = getMapView().getMapviewPort().screenHeight;
        float percent = 0.26f;
        if (screenWidth < screenHeight) {
            if (screenWidth < 930) {
                percent = isShowCrossImage ? 0.5f : 0.26f;
            } else {
                if (screenHeight > 930) {
                    percent = 0.05f;
                }
            }
        } else {
            if (screenHeight > 930) {
                percent = 0.05f;
            } else if (screenHeight >= 640) {
                percent = 0.1f;
            } else {
                percent = 0.26f;
            }
        }
        int result = (int) (screenHeight * percent);
        Logger.d(TAG, "GetDynamicLevelTopOffsetValue result " + result);
        return result;
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(DynamicLevelMode dynamicLevelMode) {
        getLayerGuideRouteControl().resetDynamicLevel();
        switch (dynamicLevelMode) {
            case DYNAMIC_LEVEL_GUIDE -> {
                getLayerGuideRouteControl().openDynamicLevel(true, DynamicLevelType.DynamicLevelGuide);
                getLayerGuideRouteControl().openDynamicLevel(false, DynamicLevelType.DynamicLevelCruise);
            }
            case DYNAMIC_LEVEL_CRUISE -> {
                getLayerGuideRouteControl().openDynamicLevel(false, DynamicLevelType.DynamicLevelGuide);
                getLayerGuideRouteControl().openDynamicLevel(true, DynamicLevelType.DynamicLevelCruise);
            }
        }
        Logger.d(TAG, "openDynamicLevel dynamicLevelMode " + dynamicLevelMode);
        openDynamicCenter(false);
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel() {
        getLayerGuideRouteControl().openDynamicLevel(false, DynamicLevelType.DynamicLevelGuide);
        getLayerGuideRouteControl().openDynamicLevel(false, DynamicLevelType.DynamicLevelCruise);
        Logger.d(TAG, "closeDynamicLevel");
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(DynamicLevelMode dynamicLevelMode, boolean isLock) {
        switch (dynamicLevelMode) {
            case DYNAMIC_LEVEL_GUIDE -> {
                getLayerGuideRouteControl().setDynamicLevelLock(isLock, DynamicLevelType.DynamicLevelGuide);
            }
            case DYNAMIC_LEVEL_CRUISE -> {
                getLayerGuideRouteControl().setDynamicLevelLock(isLock, DynamicLevelType.DynamicLevelCruise);
            }
        }
        Logger.d(TAG, "setDynamicLevelLock dynamicLevelMode" + dynamicLevelMode + " isLock " + isLock);
    }

    /* 设置自动比例尺是否主动调整地图中心 */
    public void openDynamicCenter(boolean mIsDynaCenterLock) {
        Logger.d(TAG, "openDynamicCenter mIsDynaCenterLock" + mIsDynaCenterLock);
        if (mIsDynaCenterLock) {
            getLayerGuideRouteControl().openDynamicCenter(true);
        } else {
            /* 清除中心变化缓存 */
            getLayerGuideRouteControl().resetDynamicCenter();
            /* 此时设置不可改变中心 */
            getLayerGuideRouteControl().openDynamicCenter(false);
            /* 按需调用mapView.setMapLeftTop接口恢复视口锚点 */
        }
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index     隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(int index, boolean isVisible) {
        ArrayList<RoutePathLayer> routePathLayers = getLayerGuideRouteControl().getRoutePathLayers();
        if (ConvertUtils.isEmpty(routePathLayers)) {
            Logger.e(TAG, "setPathVisible routePathLayers == null");
            return false;
        }
        if (index > routePathLayers.size()) {
            Logger.e(TAG, "setPathVisible index 越界");
            return false;
        }
        RoutePathLayer routePathLayer = routePathLayers.get(index);
        if (ConvertUtils.isEmpty(routePathLayer)) {
            Logger.d(TAG, "setPathVisible routePathLayer == null");
            return false;
        }
        routePathLayer.setVisible(isVisible);
        Logger.d(TAG, "setPathVisible success index " + index);
        return true;
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param pathId    隐藏路线id -> 通过getPathId获取
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(long pathId, boolean isVisible) {
        Logger.i(TAG, "setPathVisible pathId " + pathId + " isVisible " + isVisible);
        ArrayList<RoutePathLayer> routePathLayers = getLayerGuideRouteControl().getRoutePathLayers();
        if (ConvertUtils.isEmpty(routePathLayers)) {
            Logger.e(TAG, "setPathVisible routePathLayers == null");
            return false;
        }
        for (RoutePathLayer routePathLayer : routePathLayers) {
            if (ConvertUtils.isEmpty(routePathLayer)) {
                continue;
            }
            if (routePathLayer.getPathID() == pathId) {
                routePathLayer.setVisible(isVisible);
                Logger.d(TAG, "setPathVisible success pathId " + pathId);
                return true;
            }
        }
        return false;
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList 路线数据
     * @param selectIndex  选中下标
     */
    public boolean updatePathInfo(ArrayList<?> pathInfoList, int selectIndex) {
        Logger.d(TAG, "LayerGuideRouteImpl updatePaths setPathInfo");
        setPathInfos(pathInfoList, selectIndex);
        updatePaths();
        return true;
    }

    // 更新路线数据
    private void setPathInfoByRouteResult(RequestRouteResult routeResult) {
        if (ConvertUtils.isEmpty(routeResult.getMLineLayerParam()) ||
                ConvertUtils.isEmpty(routeResult.getMLineLayerParam().getMPathInfoList())) {
            Logger.e(TAG, "setPathInfoByRouteResult getMPathInfoList is Empty");
            return;
        }
        ArrayList<?> pathInfoList = routeResult.getMLineLayerParam().getMPathInfoList();
        setPathInfos(pathInfoList, 0);
        Logger.d(TAG, "setPathInfoByRouteResult pathInfoList " + pathInfoList.size());
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList
     * @param selectIndex
     */
    private void setPathInfos(ArrayList<?> pathInfoList, int selectIndex) {
        if (ConvertUtils.isEmpty(pathInfoList)) {
            Logger.e(TAG, "setPathInfos pathInfoList is Empty");
            return;
        }
        long pathCount = pathInfoList == null ? 0 : pathInfoList.size();
        ArrayList<BizPathInfoAttrs> bizPathInfoAttrs = new ArrayList<>();
        mPathInfoList = (ArrayList<PathInfo>) pathInfoList;
        for (int i = 0; i < pathCount; i++) {
            if (pathInfoList.get(i) instanceof PathInfo) {
                PathInfo pathInfo = (PathInfo) pathInfoList.get(i);
                if (null != pathInfo && pathInfo.isValid()) {
                    BizPathInfoAttrs bizPath = new BizPathInfoAttrs();
                    bizPath.mPathInfo = pathInfo;
                    bizPath.mDrawAtts = new BizRouteDrawCtrlAttrs();
                    bizPath.mDrawAtts.mIsDrawPath = true;//是否要绘制
                    bizPath.mDrawAtts.mIsDrawPathCamera = true;//是否绘制电子眼
                    bizPath.mDrawAtts.mIsDrawPathTrafficLight = true; //是否要绘制路线上的交通灯
                    bizPath.mDrawAtts.mIsDrawArrow = true;//是否要绘制转向箭头
                    bizPath.mDrawAtts.mIsVisible = true;//是否要显示
                    bizPath.mDrawAtts.mIsNewRouteForCompareRoute = true;
                    bizPath.mDrawAtts.mIsTrafficEventOpen = true;//是否要打开交通事件显示开关，默认为开
                    bizPath.mDrawAtts.mIsHighLightRoadName = true;
                    bizPathInfoAttrs.add(bizPath);
                }
            }
        }
        Logger.i(TAG, "设置路线 更新引导路线数据 -> " + bizPathInfoAttrs, "默认选中路线 -> " + selectIndex);
        int result = getLayerGuideRouteControl().setPathInfos(bizPathInfoAttrs, selectIndex);
        Logger.d(TAG, "设置路线 result : " + result);
    }

    /**
     * 设置行前拥堵气泡是否显示
     */
    public boolean setRouteJamBubblesVisible(boolean isShow) {
        Logger.d(TAG, "setRouteJamBubblesVisible isShow " + isShow);
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeRouteJamBubbles, isShow);
        return true;
    }

    /* 设置能量耗尽点扎标是否显示 只在全览态展示 */
    public void setRouteEnergyEmptyPointVisible(boolean isShow) {
        Logger.d(TAG, "isShow ", isShow);
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeEnergyEmptyPoint, isShow);
    }

    public void setGuideTrafficVisible(boolean isShow) {
        getLayerRoadFacilityControl().enableLayer(BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight, isShow);
        getLayerRoadFacilityControl().setVisible(BizRoadFacilityType.BizRoadFacilityTypeGuideTrafficSignalLight, isShow);
    }

    /*设置图元显示、开启碰撞*/
    private void initBizTypeVisible() {
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeGuideLabel, true);
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).enableCollision(true);
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeWeather).enableCollision(true);
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaChargeStationPoint).enableCollision(true);
    }

    /**
     * 设置选中路线.
     *
     * @param index 被选路线id
     */
    public void setSelectedPathIndex(int index) {
        boolean b = getLayerGuideRouteControl().switchSelectedPath(index);
        Logger.d(TAG, "LayerGuideRouteImpl updatePaths setSelectedPathIndex b" + b + " index " + index);
    }

    /**
     * 清除路线以及缓存的路线数据(包括路线，路线上的点，箭头，交通事件tip，拥堵事件，封路事件等).
     */
    public void clearPaths() {
        Logger.d(TAG, "clearPaths");
        getLayerGuideRouteControl().clearPaths();
        getLayerRoadFacilityControl().clearAllItems();
        mPreviewParam = null;
    }

    /**
     * 展示路线上的服务区数据
     */
    public void showRestArea(ArrayList<?> pathInfoList, int index) {
        ArrayList<BizRouteRestAreaInfo> restAreaInfos = new ArrayList<>();
        if (ConvertUtils.isEmpty(pathInfoList)) {
            getLayerGuideRouteControl().updateRouteRestAreaInfo(restAreaInfos);
            Logger.e(TAG, "showRestArea pathInfoList is Empty");
            return;
        }
        Logger.d(TAG, "showRestArea pathInfoList" + pathInfoList + " index" + index);

        PathInfo pathInfo = (PathInfo) pathInfoList.get(index);
        ArrayList<RestAreaInfo> restAreas = pathInfo.getRestAreas(0, 100);
        for (RestAreaInfo info : restAreas) {
            BizRouteRestAreaInfo bizRouteRestAreaInfo = new BizRouteRestAreaInfo();
            bizRouteRestAreaInfo.restAreaLabelInfo = info;
            restAreaInfos.add(bizRouteRestAreaInfo);
        }
        getLayerGuideRouteControl().updateRouteRestAreaInfo(restAreaInfos);
    }

    /**
     * 展示路线上的天气数据
     */
    public void showWeatherView(ArrayList<?> weatherLabelItem) {
        ArrayList<BizRouteWeatherInfo> weatherInfos = new ArrayList<>();
        if (ConvertUtils.isEmpty(weatherLabelItem)) {
            getLayerGuideRouteControl().updateRouteWeatherInfo(weatherInfos);
            Logger.e(TAG, "showWeatherView weatherLabelItem is Empty");
            return;
        }
        Logger.d(TAG, "showWeatherView weatherLabelItem" + weatherLabelItem.size());
        for (int i = 0; i < weatherLabelItem.size(); i++) {
            BizRouteWeatherInfo info = new BizRouteWeatherInfo();
            info.weatherLabelInfo = (WeatherLabelItem) weatherLabelItem.get(i);
            weatherInfos.add(info);
        }
        getLayerGuideRouteControl().updateRouteWeatherInfo(weatherInfos);
    }

    /**
     * 切换选中的路线，同时改变当前选中路线的样式
     *
     * @param index 第几条路线
     * @return 切换是否成功，超过当前路线个数返回false @thread multi
     */
    public boolean switchSelectedPath(int index) {
        boolean b = getLayerGuideRouteControl().switchSelectedPath(index);
        Logger.d(TAG, "switchSelectedPath b" + b + " index:" + index);
        return b;
    }

    /**
     * @param segmentsIndexs 设置转向箭头要显示导航段
     */
    public void setPathArrowSegment(ArrayList<Long> segmentsIndexs) {
        Logger.d(TAG, "setPathArrowSegment segmentsIndexs" + segmentsIndexs.size());
        getLayerGuideRouteControl().setPathArrowSegment(segmentsIndexs);
    }


    //更新导航交通事件图层
    void updateLocalTrafficEventInfo(ArrayList<LayerItemTrafficEvent> vecTrafficEventInfo) {
        Logger.d(TAG, "updateLocalTrafficEventInfo vecTrafficEventInfo" + vecTrafficEventInfo.size());
        ArrayList<BizLocalTrafficEventInfo> vecTrafficEventInfoBls = new ArrayList<BizLocalTrafficEventInfo>();
        getLayerRoadFacilityControl().updateLocalTrafficEventInfo(vecTrafficEventInfoBls);
    }

    public boolean showCross(CrossImageEntity crossInfo) {
        if (ConvertUtils.isEmpty(crossInfo)) {
            Logger.e(TAG, "showCross crossInfo == null");
            return false;
        }
        Logger.d(TAG, "the type is  " + crossInfo.getType()
                + ", DataBuf is empty : " + ConvertUtils.isEmpty(crossInfo.getDataBuf())
                + ", ArrowDataBuf is empty: " + ConvertUtils.isEmpty(crossInfo.getArrowDataBuf()));
        boolean ret = false;
        if (crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_VECTOR || crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_3_D) {
            //矢量图或者三维图
            ret = getLayerRoadCrossControl().updateCross(crossInfo.getDataBuf(), getCrossType(crossInfo.getType()));
        } else if (crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_GRID) {
            ret = getLayerRoadCrossControl().setRasterImageData(
                    TexturePoolManager.get().getArrowRoadImage(true, crossInfo),
                    TexturePoolManager.get().getArrowRoadImage(false, crossInfo));
        }
        Logger.i(TAG, "showCross ret：" + ret);
        getLayerRoadCrossControl().setVisible(getCrossType(crossInfo.getType()), ret);
        return ret;
    }

    public boolean hideCross(int type) {
        Logger.d(TAG, "hideCross");
        getLayerRoadCrossControl().hideCross(type);
        return true;
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(Rect rect) {
        if (ConvertUtils.isEmpty(rect)) {
            Logger.e(TAG, "updateRoadCrossRect rect == null");
            return;
        }
        Logger.d(TAG, "updateRoadCrossRect");
        getStyleAdapter().updateRoadCrossRect(rect);
    }

    public Rect getRoadCrossRect() {
        Rect rect = new Rect();
        if (!ConvertUtils.isEmpty(getStyleAdapter())) {
            return getStyleAdapter().getRoadCrossRect();
        }
        return rect;
    }

    @CrossType.CrossType1
    private int getCrossType(int type) {
        return switch (type) {
            case NaviConstant.CrossType.CROSS_TYPE_GRID -> CrossType.CrossTypeGrid;
            case NaviConstant.CrossType.CROSS_TYPE_VECTOR -> CrossType.CrossTypeVector;
            case NaviConstant.CrossType.CROSS_TYPE_3_D -> CrossType.CrossType3D;
            default -> CrossType.AUTO_UNKNOWN_ERROR;
        };
    }

    /* 设置起点扎标是否显示 */
    public void setStartPointVisible(boolean visible) {
        Logger.d(TAG, "setStartPointVisible visible " + visible);
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeStartPoint, visible);
    }

    /**
     * 清除指定类型扎标
     */
    public void clearRouteItemByType(LayerPointItemType type) {
        Logger.d(TAG, "clearRouteItemByType type " + type);
        switch (type) {
            //删除自定义替换补能扎标
            case ROUTE_POINT_VIA_REPLACE_CHARGE -> {
                ArrayList<LayerItem> allItems = getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).getAllItems();
                for (LayerItem allItem : allItems) {
                    if (allItem instanceof RoutePathPointItem viaPoint) {
                        long pathId = viaPoint.getPathId();
                        String id = allItem.getID();
                        Logger.d(TAG, "clearRouteItemByType pathId " + pathId + " id " + id);
                        if (pathId == LayerPointItemType.ROUTE_POINT_VIA_REPLACE_CHARGE.ordinal()) {
                            getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).removeItem(id);
                        }
                    }
                }
            }
        }
    }
}
