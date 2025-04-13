package com.fy.navi.service.adapter.layer.bls.impl;

import android.content.Context;

import com.android.utils.ConvertUtils;
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
import com.autonavi.gbl.layer.RoutePathPointItem;
import com.autonavi.gbl.layer.RouteTrafficEventTipsLayerItem;
import com.autonavi.gbl.layer.model.BizEnergyKeyInfo;
import com.autonavi.gbl.layer.model.BizLabelType;
import com.autonavi.gbl.layer.model.BizLocalTrafficEventInfo;
import com.autonavi.gbl.layer.model.BizOddInfo;
import com.autonavi.gbl.layer.model.BizPathInfoAttrs;
import com.autonavi.gbl.layer.model.BizPopPointBusinessInfo;
import com.autonavi.gbl.layer.model.BizRouteDrawCtrlAttrs;
import com.autonavi.gbl.layer.model.BizRouteMapMode;
import com.autonavi.gbl.layer.model.BizRouteRestAreaInfo;
import com.autonavi.gbl.layer.model.BizRouteType;
import com.autonavi.gbl.layer.model.BizRouteViaRoadInfo;
import com.autonavi.gbl.layer.model.BizRouteWeatherInfo;
import com.autonavi.gbl.layer.model.BizThreeUrgentInfo;
import com.autonavi.gbl.layer.model.DynamicLevelParam;
import com.autonavi.gbl.layer.model.DynamicLevelType;
import com.autonavi.gbl.layer.model.RouteDrawStyle;
import com.autonavi.gbl.map.MapView;
import com.autonavi.gbl.map.layer.BaseLayer;
import com.autonavi.gbl.map.layer.LayerItem;
import com.autonavi.gbl.map.layer.RoutePathLayer;
import com.autonavi.gbl.map.layer.model.ClickViewIdInfo;
import com.autonavi.gbl.map.layer.model.RouteLayerScene;
import com.autonavi.gbl.pos.model.LocInfo;
import com.autonavi.gbl.pos.observer.IPosLocInfoObserver;
import com.autonavi.gbl.route.model.WeatherLabelItem;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.bls.style.LayerGuideRouteStyleAdapter;
import com.fy.navi.service.adapter.layer.bls.texture.LayerTextureManager;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEnergyKey;
import com.fy.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePathInfo;
import com.fy.navi.service.define.layer.refix.LayerItemRouteRestArea;
import com.fy.navi.service.define.layer.refix.LayerItemRouteThreeUrgent;
import com.fy.navi.service.define.layer.refix.LayerItemRouteViaRoad;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerItemTrafficEvent;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.route.RouteLinePoints;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;

public class LayerGuideRouteImpl extends BaseLayerImpl<LayerGuideRouteStyleAdapter> {

    private ArrayList<String> mEstimatedTimeOfArrival = new ArrayList<>();
    private String mCurrentRouteTime = "";

    public LayerGuideRouteImpl(BizControlService bizService, MapView mapView, Context context, MapType mapType) {
        super(bizService, mapView, context,mapType);
        getLayerGuideRouteControl().setStyle(this);
        getLayerRoadCrossControl().setStyle(this);
        getLayerRoadCrossControl().setStyle(this);
        getLayerGuideRouteControl().addClickObserver(this);
        getLayerGuideRouteControl().addFocusChangeObserver(this);
        getLayerRoadFacilityControl().addClickObserver(this);
        getLayerRoadFacilityControl().addFocusChangeObserver(this);
        getLayerRoadFacilityControl().addObserver(this);
        initDynamicLevel();
    }

    @Override
    protected LayerGuideRouteStyleAdapter createStyleAdapter() {
        return new LayerGuideRouteStyleAdapter(getEngineId(), getLayerRoadCrossControl(), getLayerGuideRouteControl(), getLayerRoadFacilityControl(), getLayerLabelControl());
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
            callBacks.get(i).onRouteItemClick(getMapType(), clickResult);
        }
    }

    private GemLayerItem getClickResult(LayerItem pItem) {
        GemLayerItem gemLayerItem = new GemLayerItem();
        switch (pItem.getBusinessType()) {
            // 路线图层内容点击
            case BizRouteType.BizRouteTypeStartPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) pItem).getPosition();
                if (ConvertUtils.isEmpty(coord3DDouble)) {
                    Logger.d(TAG, "BizRouteTypeStartPoint coord3DDouble is null");
                    return gemLayerItem;
                }
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeStartPoint);
            }
            case BizRouteType.BizRouteTypeEndPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) pItem).getPosition();
                if (ConvertUtils.isEmpty(coord3DDouble)) {
                    Logger.d(TAG, "BizRouteTypeEndPoint coord3DDouble is null");
                    return gemLayerItem;
                }
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeEndPoint);
            }
            case BizRouteType.BizRouteTypeViaPoint -> {
                Coord3DDouble coord3DDouble = ((RoutePathPointItem) pItem).getPosition();
                if (ConvertUtils.isEmpty(coord3DDouble)) {
                    Logger.d(TAG, "BizRouteTypeViaPoint coord3DDouble is null");
                    return gemLayerItem;
                }
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setLayerItemId(Integer.parseInt(pItem.getID()));
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeViaPoint);
            }
            case BizRouteType.BizRouteTypePath -> {
                Logger.d(TAG, "getClickResult BizRouteTypePath id:" + pItem.getID());
                gemLayerItem.setLayerItemId(Long.parseLong(pItem.getID()));
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypePath);
            }
            case BizRouteType.BizRouteTypeWeather -> {
                gemLayerItem.setLayerItemId(Integer.parseInt(pItem.getID()));
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeWeather);
            }
            case BizRouteType.BizRouteTypeRestArea -> {
                gemLayerItem.setLayerItemId(Integer.parseInt(pItem.getID()));
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeRestArea);
            }
            case BizRouteType.BizRouteTypeViaChargeStationPoint -> {
                gemLayerItem.setLayerItemId(Integer.parseInt(pItem.getID()));
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeViaChargeStationPoint);
            }
            case BizRouteType.BizRouteTypeTrafficEventTip -> {
                RouteTrafficEventTipsLayerItem trafficEventTipsLayerItem = (RouteTrafficEventTipsLayerItem) pItem;
                long eventID = trafficEventTipsLayerItem.getMTrafficEventTipsInfo().mTrafficIncident.ID;
                Coord3DDouble coord3DDouble = trafficEventTipsLayerItem.getPosition();
                gemLayerItem.setLat(coord3DDouble.lat);
                gemLayerItem.setLog(coord3DDouble.lon);
                gemLayerItem.setZ(coord3DDouble.z);
                gemLayerItem.setEventId(eventID);
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.BizRouteTypeTrafficEventTip);
            }
            default -> {
                // TODO 扩展新的需求
                gemLayerItem.setClickBusinessType(GemLayerClickBusinessType.UnKnown);
            }
        }
        return gemLayerItem;
    }

    /**
     * 全览参数句柄转换
     *
     * @param pathResult
     * @return
     */
    public PreviewParams getPathResultBound(ArrayList<?> pathResult) {
        ArrayList<PathInfo> pathInfos = (ArrayList<PathInfo>) pathResult;
        RectDouble rectDouble = BizGuideRouteControl.getPathResultBound(pathInfos);
        Logger.d(TAG, "path info 转换为预览巨型区域参数：", rectDouble);
        PreviewParams previewParams = new PreviewParams();
        previewParams.setRouteLine(true);
        previewParams.setbUseRect(true);
        if (!ConvertUtils.isEmpty(rectDouble)) {
            previewParams.setMapBound(new PreviewParams.RectDouble(rectDouble.left, rectDouble.right, rectDouble.top, rectDouble.bottom));
        }
        return previewParams;
    }

    /**
     * 绘制路线以及路线上的元素.
     */
    private void updatePaths() {
        int result = getLayerGuideRouteControl().updatePaths();
        Logger.d(TAG, "updatePaths result : " + result);
        getLayerGuideRouteControl().updatePathArrow();
    }

    /**
     * 绘制路线
     *
     * @param routeLineLayer
     */
    public void drawRouteLine(RouteLineLayerParam routeLineLayer) {
        Logger.d(TAG, "drawRouteLine");
        ConvertUtils.isNullRequire(routeLineLayer, "路线绘制参数为空，无法进行路线渲染");
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeTrafficEventTip).enableCollision(true);
        setPassGreyMode(routeLineLayer.isMPassGrey());
        setMainMapPathDrawStyle(false, false, true);
        setPathPoints(routeLineLayer.getMRouteLinePoints());
        getLayerGuideRouteControl().setVisible(BizRouteType.BizRouteTypeEnergyRemainPoint, true);
        setPathInfos(routeLineLayer.getMPathInfoList(), routeLineLayer.getMSelectIndex());
        if (routeLineLayer.getMEstimatedTimeOfArrival() != null) {
            mEstimatedTimeOfArrival = routeLineLayer.getMEstimatedTimeOfArrival();
        }
        updatePaths();
    }

    /**
     * 设置路线样式风格
     * @param isStartNavi 是否开始导航
     * @param isOffLine 是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        setMainMapPathDrawStyle(isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 走过的路线置灰.
     *
     * @param bPassGrey 是否置灰
     */
    public void setPassGreyMode(boolean bPassGrey) {
        Logger.d(TAG, "setPassGreyMode");
        getLayerGuideRouteControl().setPassGreyMode(true);
    }

    /**
     * 设置样式风格
     * @param isStartNavi 是否开始导航
     * @param isOffLine 是否离线
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
    }

    /**
     * 更新路线行程点信息.
     *
     * @param routeLinePoints 路线图层参数
     */
    private void setPathPoints(RouteLinePoints routeLinePoints) {
        Logger.i(TAG, "设置路线行程点的信息 -> " + routeLinePoints);
        RoutePoints pathPoints = getRoutePoints(routeLinePoints);
        int points = getLayerGuideRouteControl().setPathPoints(pathPoints);
        Logger.d(TAG, "setPathPoints points " + points);
    }

    /**
     * 删除途经点
     * @param pid 途经点id
     */
    public void removeViaPoint(String pid) {
        if (ConvertUtils.isEmpty(pid)) {
            Logger.e(TAG, "removeViaPoint pid is null");
            return;
        }
        getLayerGuideRouteControl().getRouteLayer(BizRouteType.BizRouteTypeViaPoint).removeItem(pid);
    }

    /* * 更新终点扎标样式
     * @param endPoint 终点扎标样式
     */
    public void updateEndPoint(LayerItemRouteEndPoint endPoint) {
        if (ConvertUtils.isEmpty(endPoint)) {
            Logger.d(TAG, "updateEndPoint endPoint == null");
            return;
        }
        Logger.d(TAG, "updateEndPoint");
        getStyleAdapter().updateLabelPoint(endPoint);
    }

    private RoutePoints getRoutePoints(RouteLinePoints info) {
        RoutePoints infos = new RoutePoints();
        ArrayList<RoutePoint> start = new ArrayList<>();
        ArrayList<RoutePoint> end = new ArrayList<>();
        ArrayList<RoutePoint> via = new ArrayList<>();
        if (!ConvertUtils.isEmpty(info.getMStartPoints())) {
            for (int t = 0; t < info.getMStartPoints().size(); t++) {
                RoutePoint point = new RoutePoint();
                point.mIsDraw = info.getMStartPoints().get(t).isMIsDraw();
                point.mPathId = info.getMStartPoints().get(t).getMPathId();
                point.mType = info.getMStartPoints().get(t).getMType();
                point.mPos = new Coord3DDouble(info.getMStartPoints().get(t).getMPos().getLon()
                        , info.getMStartPoints().get(t).getMPos().getLat()
                        , info.getMStartPoints().get(t).getMPos().getZ());
                start.add(point);
            }
        }
        if (!ConvertUtils.isEmpty(info.getMViaPoints())) {
            for (int t = 0; t < info.getMViaPoints().size(); t++) {
                RoutePoint point = new RoutePoint();
                point.mIsDraw = info.getMViaPoints().get(t).isMIsDraw();
                point.mPathId = info.getMViaPoints().get(t).getMPathId();
                point.mType = info.getMViaPoints().get(t).getMType();
                point.mPos = new Coord3DDouble(info.getMViaPoints().get(t).getMPos().getLon()
                        , info.getMViaPoints().get(t).getMPos().getLat()
                        , info.getMViaPoints().get(t).getMPos().getZ());
                via.add(point);
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
                end.add(point);
            }
        }
        infos.mStartPoints = start;
        infos.mViaPoints = via;
        infos.mEndPoints = end;
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


    //更新Odd信息
    public void updateOddInfo(ArrayList<LayerItemRouteOdd> oddInfoList) {
        Logger.d(TAG, "updateOddInfo oddInfoList" + oddInfoList);
        ArrayList<BizOddInfo> oddInfoListBls = new ArrayList<BizOddInfo>();
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

    /**
     * 是否打开自动比例尺
     *
     * @param isOpen 开关状态
     */
    public void openDynamicLevel(boolean isOpen) {
        int openDynamicLevel = getLayerGuideRouteControl().openDynamicLevel(isOpen);
        //设置地图中心点，不根据自动比例尺移动
        int openedDynamicCenter = getLayerGuideRouteControl().openDynamicCenter(false);
        Logger.d(TAG, "openDynamicLevel openDynamicLevel" + openDynamicLevel +
                " openedDynamicCenter" + openedDynamicCenter + " isOpen" + isOpen);
        /**
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
         */
    }

    public void openLockDynamicLevel(DynamicLevelMode dynamicLevelMode, boolean bLock) {
        switch (dynamicLevelMode) {
            case DYNAMIC_LEVEL_CRUISE -> {
                getLayerGuideRouteControl().setDynamicLevelLock(bLock, DynamicLevelType.DynamicLevelCruise);
            }
            case DYNAMIC_LEVEL_GUIDE -> {
                getLayerGuideRouteControl().setDynamicLevelLock(bLock, DynamicLevelType.DynamicLevelGuide);
            }
        }
    }

    public void openDynaCenterLock(boolean mIsDynaCenterLock) {
        Logger.d(TAG, "openDynaCenterLock mIsDynaCenterLock" + mIsDynaCenterLock);
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
     * @param index 隐藏路线下标 -> list下标 默认0开始
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
     * 更新引导路线数据
     * @param pathInfoList 路线数据
     * @param selectIndex 选中下标
     */
    public boolean updatePathInfo(ArrayList<?> pathInfoList, int selectIndex) {
        Logger.d(TAG, "setPathInfo");
        setPathInfos(pathInfoList, selectIndex);
        updatePaths();
        return true;
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList
     * @param selectIndex
     */
    public void setPathInfos(ArrayList<?> pathInfoList, int selectIndex) {
        getLayerGuideRouteControl().clearPaths();
        long pathCount = pathInfoList == null ? 0 : pathInfoList.size();
        ArrayList<BizPathInfoAttrs> bizPathInfoAttrs = new ArrayList<>();
        for (int i = 0; i < pathCount; i++) {
            if (pathInfoList.get(i) instanceof PathInfo) {
                PathInfo pathInfo = (PathInfo) pathInfoList.get(i);
                if (null != pathInfo && pathInfo.isValid()) {
                    BizPathInfoAttrs bizPath = new BizPathInfoAttrs();
                    bizPath.mPathInfo = pathInfo;
                    bizPath.mDrawAtts = new BizRouteDrawCtrlAttrs();
                    bizPath.mDrawAtts.mIsDrawPath = true;//是否要绘制
                    bizPath.mDrawAtts.mIsDrawPathCamera = false;//是否绘制电子眼
                    bizPath.mDrawAtts.mIsDrawPathTrafficLight = true; //是否要绘制路线上的交通灯
                    bizPath.mDrawAtts.mIsDrawArrow = true;//是否要绘制转向箭头
                    bizPath.mDrawAtts.mIsVisible = true;//是否要显示
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

    /**
     * 设置选中路线.
     *
     * @param index 被选路线id
     */
    public void setSelectedPathIndex(int index) {
        if (mEstimatedTimeOfArrival != null && mEstimatedTimeOfArrival.size() > index) {
            mCurrentRouteTime = mEstimatedTimeOfArrival.get(index);
        } else {
            mCurrentRouteTime = "";
        }
        boolean b = getLayerGuideRouteControl().setSelectedPathIndex(index);
        Logger.d(TAG, "setSelectedPathIndex b" + b + " index " + index);
        updatePaths();
    }

    /**
     * 获取预计到达时间.
     */
    public String getCurrentRouteTime() {
        Logger.d(TAG, "getCurrentRouteTime" + mCurrentRouteTime);
        return mCurrentRouteTime;
    }

    //获取选中路线index
    int getSelectedPathIndex() {
        int selectedPathIndex = getLayerGuideRouteControl().getSelectedPathIndex();
        Logger.d(TAG, "getSelectedPathIndex " + selectedPathIndex);
        return selectedPathIndex;
    }

    /**
     * 清除路线以及缓存的路线数据(包括路线，路线上的点，箭头，交通事件tip，拥堵事件，封路事件等).
     */
    public void clearPaths() {
        Logger.d(TAG, "clearPaths");
        getLayerGuideRouteControl().clearPaths();
        getMapView().destroyTexture(getDynamicsMarkerId(1));
    }

    /**
     * 展示路线上的服务区数据
     */
    public void showRestArea(ArrayList<?> pathInfoList, int index) {
        ArrayList<BizRouteRestAreaInfo> restAreaInfos = new ArrayList<>();
        if (ConvertUtils.isEmpty(pathInfoList)) {
            getLayerGuideRouteControl().updateRouteRestAreaInfo(restAreaInfos);
            return;
        }
        Logger.d(TAG, "showRestArea pathInfoList" + pathInfoList + " index" + index);

        PathInfo pathInfo = (PathInfo) pathInfoList.get(index);
        ArrayList<RestAreaInfo> restAreas = pathInfo.getRestAreas(0, 20);
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

    /**
     * 释放终点动态纹理
     *
     * @return markerId
     * @brief 获取动态markerId
     */
    private int getDynamicsMarkerId(int index) {
        return 0x660000 + 0x60000 + index;
    }

    //更新导航交通事件图层
    void updateLocalTrafficEventInfo(ArrayList<LayerItemTrafficEvent> vecTrafficEventInfo) {
        Logger.d(TAG, "updateLocalTrafficEventInfo vecTrafficEventInfo" + vecTrafficEventInfo.size());
        ArrayList<BizLocalTrafficEventInfo> vecTrafficEventInfoBls = new ArrayList<BizLocalTrafficEventInfo>();
        getLayerRoadFacilityControl().updateLocalTrafficEventInfo(vecTrafficEventInfoBls);
    }

    public boolean showCross(CrossImageEntity crossInfo) {
        boolean ret = false;
        if (crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_VECTOR || crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_3_D) {
            //矢量图或者三维图
            ret = getLayerRoadCrossControl().updateCross(crossInfo.getDataBuf(), getCrossType(crossInfo.getType()));
        } else if (crossInfo.getType() == NaviConstant.CrossType.CROSS_TYPE_GRID) {
            ret = getLayerRoadCrossControl().setRasterImageData(
                    LayerTextureManager.get().getArrowRoadImage(true, crossInfo),
                    LayerTextureManager.get().getArrowRoadImage(false, crossInfo));
        }
        Logger.i(TAG, "showCross ret：" + ret);
        getLayerRoadCrossControl().setVisible(getCrossType(crossInfo.getType()), ret);
        return ret;
    }

    public boolean hideCross(int type) {
        getLayerRoadCrossControl().hideCross(type);
        return true;
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
}
