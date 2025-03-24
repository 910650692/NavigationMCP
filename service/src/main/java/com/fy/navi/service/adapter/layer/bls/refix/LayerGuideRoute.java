package com.fy.navi.service.adapter.layer.bls.refix;

import android.content.Context;

import com.autonavi.gbl.common.model.Coord3DDouble;
import com.autonavi.gbl.common.model.RectDouble;
import com.autonavi.gbl.common.path.model.RoutePoint;
import com.autonavi.gbl.common.path.model.RoutePoints;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.guide.model.CruiseFacilityInfo;
import com.autonavi.gbl.guide.model.TrafficEventInfo;
import com.autonavi.gbl.layer.BizControlService;
import com.autonavi.gbl.layer.model.BizEnergyKeyInfo;
import com.autonavi.gbl.layer.model.BizLocalTrafficEventInfo;
import com.autonavi.gbl.layer.model.BizOddInfo;
import com.autonavi.gbl.layer.model.BizPathInfoAttrs;
import com.autonavi.gbl.layer.model.BizRouteDrawCtrlAttrs;
import com.autonavi.gbl.layer.model.BizRouteRestAreaInfo;
import com.autonavi.gbl.layer.model.BizRouteViaRoadInfo;
import com.autonavi.gbl.layer.model.BizThreeUrgentInfo;
import com.autonavi.gbl.layer.model.DynamicLevelParam;
import com.autonavi.gbl.layer.model.DynamicLevelType;
import com.autonavi.gbl.layer.model.RouteDrawStyle;
import com.autonavi.gbl.map.MapView;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.DynamicLevelMode;
import com.fy.navi.service.define.layer.refix.LayerItemRouteDrawStyle;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEnergyKey;
import com.fy.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePathInfo;
import com.fy.navi.service.define.layer.refix.LayerItemRoutePoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteRestArea;
import com.fy.navi.service.define.layer.refix.LayerItemRouteThreeUrgent;
import com.fy.navi.service.define.layer.refix.LayerItemRouteViaRoad;
import com.fy.navi.service.define.layer.refix.LayerItemTrafficEvent;

import java.util.ArrayList;
import java.util.List;

public class LayerGuideRoute extends BaseLayerImpl {

    public LayerGuideRoute(BizControlService bizService, MapView mapView, Context context) {
        super(bizService, mapView, context);
        getLayerGuideRouteControl().setStyle(this);
        getLayerGuideRouteControl().addClickObserver(this);
        getLayerGuideRouteControl().addFocusChangeObserver(this);
        getLayerRoadFacilityControl().addClickObserver(this);
        getLayerRoadFacilityControl().addFocusChangeObserver(this);
        getLayerRoadFacilityControl().addObserver(this);
    }


    //更新路线上的箭头
    public void updatePathArrow() {
        getLayerGuideRouteControl().updatePathArrow();
    }

    //设置路线是否对比模式
    public void setCompareRouteMode(boolean compareMode) {
        getLayerGuideRouteControl().setCompareRouteMode(compareMode);
    }

    //更新电量关键点信息
    public void updateEnergyKeyInfo(ArrayList<LayerItemRouteEnergyKey> energyKeyInfos) {
        ArrayList<BizEnergyKeyInfo> energyKeyInfosBls = new ArrayList<BizEnergyKeyInfo>();
        getLayerGuideRouteControl().updateEnergyKeyInfo(energyKeyInfosBls);
    }


    //更新Odd信息
    public void updateOddInfo(ArrayList<LayerItemRouteOdd> oddInfoList) {
        ArrayList<BizOddInfo> oddInfoListBls = new ArrayList<BizOddInfo>();
        getLayerGuideRouteControl().updateOddInfo(oddInfoListBls);
    }

    //更新避让路线图层
    public void updateRouteDodgeLine(int groupSegIndex) {
        getLayerGuideRouteControl().updateRouteDodgeLine(groupSegIndex);
    }

    //更新服务区扎标信息
    public void updateRouteRestAreaInfo(ArrayList<LayerItemRouteRestArea> restAreaInfos) {
        ArrayList<BizRouteRestAreaInfo> restAreaInfosBls = new ArrayList<BizRouteRestAreaInfo>();
        getLayerGuideRouteControl().updateRouteRestAreaInfo(restAreaInfosBls);
    }

    //更新途经路扎标信息
    public void updateRouteViaRoadInfo(ArrayList<LayerItemRouteViaRoad> viaRoadInfos) {
        ArrayList<BizRouteViaRoadInfo> viaRoadInfosBls = new ArrayList<BizRouteViaRoadInfo>();
        getLayerGuideRouteControl().updateRouteViaRoadInfo(viaRoadInfosBls);
    }

    //设置彩虹线绘制范围，用于象限的计算
    public void updateThreeUrgentInfo(ArrayList<LayerItemRouteThreeUrgent> threeUrgentInfos) {
        ArrayList<BizThreeUrgentInfo> threeUrgentInfosBls = new ArrayList<BizThreeUrgentInfo>();
        getLayerGuideRouteControl().updateThreeUrgentInfo(threeUrgentInfosBls);
    }


    //显示第几个中途点ETA和剩余电量，默认显示第一个(如果等于-1，则显示全部途经点，包括服务器推荐的途经充电站)
    public void showViaETAByIndex(int viaIndex) {
        getLayerGuideRouteControl().showViaETAByIndex(viaIndex);
    }


    //设置熟悉路线
    public void setFamiliarRoute(LayerItemRoutePathInfo pathInfo) {
        PathInfo pathInfoBls = new PathInfo();
        getLayerGuideRouteControl().setFamiliarRoute(pathInfoBls);
    }

    //楼层切换
    public void setParkFloor(int index, int floor) {
        getLayerGuideRouteControl().setParkFloor(index, floor);
    }

    //初始化动态比例尺
    public void initDynamicLevel() {
        DynamicLevelParam dynamicLevelParam = new DynamicLevelParam();
        getLayerGuideRouteControl().initDynamicLevel(dynamicLevelParam);
    }

    public void openDynamicLevel(DynamicLevelMode dynamicLevelMode) {
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


    //更新引导路线数据
    public void drawPathInfos(LayerItemRouteDrawStyle drawStyleParam, LayerItemRoutePathInfo routePathInfo, int selectIndex) {
        getLayerGuideRouteControl().clearPaths();
        ArrayList<BizPathInfoAttrs> pathInfoBls = new ArrayList<BizPathInfoAttrs>();
        List<?> pathInfoList = routePathInfo.pathInfoList;
        long pathCount = pathInfoList == null ? 0 : pathInfoList.size();
        ArrayList<BizPathInfoAttrs> paths = new ArrayList<>(); // 准备线数据
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
                    paths.add(bizPath);
                }
            }
        }

        // 路线绘制风格
        RouteDrawStyle mainRouteDrawStyle = new RouteDrawStyle();
        mainRouteDrawStyle.mIsNavi = true; // 是否导航 画导航路径时要设为true
        mainRouteDrawStyle.mIsOffLine = drawStyleParam.mIsOffLine; // 是否离线
        mainRouteDrawStyle.mRouteMapMode = drawStyleParam.mRouteMapMode; // 图模式 主图 或 鹰眼
        mainRouteDrawStyle.mRouteScene = drawStyleParam.mRouteScene; // 路线业务场景 单指线
        mainRouteDrawStyle.mIsMultipleMode = drawStyleParam.mIsMultipleMode; // 是否是多备选模式
        getLayerGuideRouteControl().setPathDrawStyle(mainRouteDrawStyle);// 主图路线风格
        // 更新路线行程点信息
        RoutePoints pathPoints = new RoutePoints();
        for (LayerItemRoutePoint routePoint : routePathInfo.mStartPoints) {
            if (routePoint.getVecPoints() == null) break;
            for (GeoPoint point : routePoint.getVecPoints()) {
                pathPoints.mStartPoints.add(new RoutePoint(routePoint.mIsDraw, routePoint.mPathId, routePoint.mType,
                        new Coord3DDouble(point.getLon(), point.getLat(), point.getZ())));
            }
        }

        for (LayerItemRoutePoint routePoint : routePathInfo.mViaPoints) {
            if (routePoint.getVecPoints() == null) break;
            for (GeoPoint point : routePoint.getVecPoints()) {
                pathPoints.mStartPoints.add(new RoutePoint(routePoint.mIsDraw, routePoint.mPathId, routePoint.mType,
                        new Coord3DDouble(point.getLon(), point.getLat(), point.getZ())));
            }
        }

        for (LayerItemRoutePoint routePoint : routePathInfo.mEndPoints) {
            if (routePoint.getVecPoints() == null) break;
            for (GeoPoint point : routePoint.getVecPoints()) {
                pathPoints.mStartPoints.add(new RoutePoint(routePoint.mIsDraw, routePoint.mPathId, routePoint.mType,
                        new Coord3DDouble(point.getLon(), point.getLat(), point.getZ())));
            }
        }

        getLayerGuideRouteControl().setPathPoints(pathPoints);
        getLayerGuideRouteControl().setPathInfos(paths, 0);// 更新引导路线数据
        getLayerGuideRouteControl().setPathInfos(pathInfoBls, selectIndex);
        getLayerGuideRouteControl().setPassGreyMode(drawStyleParam.bPassGrey);

    }

    //获取选中路线index
    int getSelectedPathIndex() {
        return getLayerGuideRouteControl().getSelectedPathIndex();
    }


    //更新导航交通事件图层
    void updateLocalTrafficEventInfo(ArrayList<LayerItemTrafficEvent> vecTrafficEventInfo) {
        ArrayList<BizLocalTrafficEventInfo> vecTrafficEventInfoBls = new ArrayList<BizLocalTrafficEventInfo>();
        getLayerRoadFacilityControl().updateLocalTrafficEventInfo(vecTrafficEventInfoBls);
    }


}
