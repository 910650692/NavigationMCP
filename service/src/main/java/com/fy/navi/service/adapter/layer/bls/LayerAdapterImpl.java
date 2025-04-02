package com.fy.navi.service.adapter.layer.bls;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.autonavi.gbl.servicemanager.ServiceMgr;
import com.autonavi.gbl.util.model.SingleServiceID;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.ILayerApi;
import com.fy.navi.service.adapter.layer.bls.impl.LayersPoolManager;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemCar;
import com.fy.navi.service.define.layer.refix.LayerItemSearchBeginViaEnd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserReceive;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2024/12/8
 */
public class LayerAdapterImpl implements ILayerApi {
    private static final String TAG = MapDefaultFinalTag.LAYER_SERVICE_TAG;

    private LayersPoolManager layersPoolManager;

    public LayerAdapterImpl() {
        layersPoolManager = LayersPoolManager.getInstance();
    }

    @Override
    public boolean initLayerService(final MapType mapTypeId) {
        return layersPoolManager.initLayerService(mapTypeId);
    }

    @Override
    public void registerLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer) {
        layersPoolManager.get(mapTypeId).getLayer(layerId).addLayerClickCallback(observer);
    }

    @Override
    public void unRegisterLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer) {
        layersPoolManager.get(mapTypeId).getLayer(layerId).removeClickCallback(observer);
    }

    @Override
    public void unInitLayerService() {
        layersPoolManager.unInitLayerService();
    }

    @Override
    public void setDefaultCarMode(MapType mapTypeId) {
        setCarPosition(mapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0));
        setCarMode(mapTypeId, new LayerItemCar(CarModeType.CAR_MODEL_TYPE_SKELETON));
    }

    @Override
    public void setCarMode(MapType mapTypeId, LayerItemCar carMode) {
        layersPoolManager.get(mapTypeId).getLayerCar().setCarMode(carMode);
    }

    @Override
    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {
        layersPoolManager.get(mapTypeId).getLayerCar().setCarPosition(geoPoint);
    }

    @Override
    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        layersPoolManager.get(mapTypeId).getLayerCar().setFollowMode(bFollow);
        return 1;
    }

    /**
     * 全览参数句柄转换
     *
     * @param mapTypeId
     * @param pathResult
     * @return
     */
    @Override
    public PreviewParams getPathResultBound(MapType mapTypeId, ArrayList<?> pathResult) {
        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().getPathResultBound(pathResult);
    }

    /**
     * 绘制路线
     *
     * @param mapTypeId
     * @param routeLineLayer
     */
    @Override
    public void drawRouteLine(MapType mapTypeId, RouteLineLayerParam routeLineLayer) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().drawRouteLine(routeLineLayer);
    }

    /**
     * 选择路线
     *
     * @param mapTypeId
     * @param routeIndex
     */
    @Override
    public void setSelectedPathIndex(MapType mapTypeId, int routeIndex) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().setSelectedPathIndex(routeIndex);
    }

    /**
     * 清除路线
     *
     * @param mapTypeId
     */
    @Override
    public void clearRouteLine(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().clearPaths();
    }

    /**
     * 展示路线的服务区
     *
     * @param mapTypeId
     * @param pathInfoList
     * @param index
     */
    @Override
    public void showRestArea(MapType mapTypeId, ArrayList<?> pathInfoList, int index) {

        //多屏处理
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().showRestArea(pathInfoList, index);
    }

    /**
     * 展示路线的天气
     *
     * @param mapTypeId
     * @param weatherLabelItem
     */
    @Override
    public void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().showWeatherView(weatherLabelItem);
    }

    /**
     * 绘制限行区域图层
     *
     * @param mapTypeId
     * @param object
     * @param position
     */
    @Override
    public void showRestrictionView(MapType mapTypeId, Object object, int position) {
        layersPoolManager.get(mapTypeId).getLayerArea().showRestrictionView(object, position);
    }

    /**
     * 切换路线
     *
     * @param mapTypeId
     * @param index
     * @return
     */
    @Override
    public boolean switchSelectedPath(MapType mapTypeId, int index) {
        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().switchSelectedPath(index);
    }

    /**
     * 更新路线上的箭头
     *
     * @param mapTypeId
     */
    @Override
    public void updatePathArrow(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().updatePathArrow();
    }

    /**
     * 获取预计到达时间
     *
     * @param mapTypeId
     * @return
     */
    @Override
    public String getCurrentRouteTime(MapType mapTypeId) {

        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().getCurrentRouteTime();
    }

    /**
     * 设置转向箭头要显示导航段
     *
     * @param mapTypeId
     * @param segmentsIndexs
     */
    @Override
    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().setPathArrowSegment(segmentsIndexs);
    }

    /**
     * 是否打开自动比例尺
     *
     * @param mapTypeId 地图类型
     * @param isOpen    开关状态
     */
    @Override
    public void openDynamicLevel(MapType mapTypeId, boolean isOpen) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().openDynamicLevel(isOpen);
    }

    @Override
    public void updateSearchParkPoi(MapType mapTypeId, ArrayList<NaviParkingEntity> parkList) {
    }

    @Override
    public void clearSearchParkPoi(MapType mapTypeId) {
    }

    @Override
    public void setParkFocus(MapType mapTypeId, String strID, boolean bFocus) {
    }

    @Override
    public void updateGuideCarStyle(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerCar().updateGuideCarStyle();
    }

    @Override
    public void setVisibleCruiseSignalLight(MapType mapTypeId, boolean isVisible) {
    }

    @Override
    public void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible) {
    }

    @Override
    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        Coord2DDouble startP = new Coord2DDouble(startPoint.getLon(), startPoint.getLat());
        Coord2DDouble endP = new Coord2DDouble(endPoint.getLon(), endPoint.getLat());
        return BizLayerUtil.calcDistanceBetweenPoints(startP, endP);
    }

    @Override
    public void selectSearchPoi(MapType mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus) {
        layersPoolManager.get(mapTypeId).getLayerSearch().setSelect(type, strID, bFocus);
    }


    @Override
    public void updateFavoriteMain(MapType mapTypeId, List<GmBizUserFavoritePoint> list) {
        layersPoolManager.get(mapTypeId).getLayerUser().updateFavoriteMain(list);
    }

    @Override
    public void clearFavoriteMain(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerUser().clearFavoriteMain();
    }

    @Override
    public int setDynamicLevelLock(MapType mapTypeId, boolean isLock, int type) {
        return 0;
    }

    @Override
    public void resetDynamicLevel(MapType mapTypeId, int type) {
    }

    @Override
    public boolean getDynamicLevelLock(MapType mapTypeId, int type) {
        return true;
    }

    @Override
    public float getDynamicLevelMapHeadDegree(MapType mapTypeId, int type) {
        return 0;
    }

    @Override
    public int openDynamicCenter(MapType mapTypeId, boolean changeCenter) {
        return 0;
    }

    /**
     * 搜索结果 父点+子点+中心点+出入口
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchResult(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchParentPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchParentPoi(searchResult);
        boolean searchChildPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchChildPoi(searchResult);
        boolean searchCentralPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchCentralPoi(searchResult);
        boolean searchExitEntrancePoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchExitEntrancePoi(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchResult searchParentPoi:" + searchParentPoi +
                " searchChildPoi:" + searchChildPoi +
                " searchCentralPoi:" + searchCentralPoi +
                " searchExitEntrancePoi:" + searchExitEntrancePoi);
        return true;
    }

    /**
     * 搜索中心点
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchCentralPoi(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchCentralPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchCentralPoi(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchCentralPoi searchCentralPoi:" + searchCentralPoi);
        return searchCentralPoi;
    }

    /**
     * 搜索起点、途经点、终点
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchBeginEnd(MapType mapTypeId, LayerItemSearchBeginViaEnd searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchBeginEndPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchBeginEndPoi(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchBeginEnd searchBeginEndPoi:" + searchBeginEndPoi);
        return searchBeginEndPoi;
    }

    /**
     * 沿途搜点 + 气泡
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchAlongRoute(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchAlongRoutePoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchAlongRoutePoi(searchResult);
        boolean searchAlongRoutePoiPop = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchAlongRoutePoiPop(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchAlongRoute searchAlongRoutePoi:" +
                searchAlongRoutePoi + " searchAlongRoutePoiPop:" + searchAlongRoutePoiPop);
        return true;
    }

    /**
     * 充电桩
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchChargeStation(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchChargeStation = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchChargeStation(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchChargeStation searchChargeStation:" + searchChargeStation);
        return searchChargeStation;
    }

    /**
     * 搜索 停车场+出入口
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchPark(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchParkPoi = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchParkPoi(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchPark searchParkPoi:" + searchParkPoi);
        return searchParkPoi;
    }

    /**
     * POI扎标
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchLabel(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchPoiLabel = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchPoiLabel(searchResult);
        Logger.d(TAG, "addLayerItemOfSearchLabel searchPoiLabel:" + searchPoiLabel);
        return searchPoiLabel;
    }

    /**
     * 搜索路线图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchLine(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return false;
    }

    /**
     * 搜索区域图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    @Override
    public boolean addLayerItemOfSearchPolygon(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return false;
    }

    /**
     * 清除所有搜索扎标
     *
     * @param mapTypeId
     */
    public void clearAllSearchLayerItems(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
    }

    /**
     * 清除搜索POI扎标
     *
     * @param mapTypeId
     */
    public void clearSearchPOILayerItems(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerSearch().clearSearchItem();
    }

    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口图层类型更新样式 */
    public boolean updateCrossStyle(MapType mapTypeId, @CrossType.CrossType1 int crossType) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().updateCrossStyle(crossType);
        Logger.d(TAG, "updateCrossStyle " + b);
        return b;
    }

    /* 根据放大路口类型进行显示隐藏控制 */
    public boolean setCrossVisible(MapType mapTypeId, @CrossType.CrossType1 int type, boolean bVisible) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setCrossVisible(type, bVisible);
        Logger.d(TAG, "setCrossVisible " + b);
        return b;
    }

    /* 设置栅格图图片数据 */
    public boolean setRasterImageData(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setRasterImageData(crossEntity);
        Logger.d(TAG, "setRasterImageData " + b);
        return b;
    }

    /* 根据放大路口类型填充数据 */
    public boolean updateCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().updateCross(crossEntity);
        Logger.d(TAG, "updateCross " + b);
        return b;
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().hideCross(type);
        Logger.d(TAG, "hideCross " + b);
        return b;
    }

    /* 设置导航车首上还是北上模式 */
    public boolean set3DCrossCarMode(MapType mapTypeId, boolean isCarUp) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().set3DCrossCarMode(isCarUp);
        Logger.d(TAG, "set3DCrossCarMode " + b);
        return b;
    }

    /* 设置3D飞线的路况信息 */
    public boolean setFlyTmc(MapType mapTypeId, byte[] buffer, ArrayList<RealCityTmcParam> param) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setFlyTmc(buffer, param);
        Logger.d(TAG, "setFlyTmc " + b);
        return b;
    }

    /* 更新3D精品大图引导信息 */
    public boolean updateNaviInfo(MapType mapTypeId, NaviInfo naviInfo) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().updateNaviInfo(naviInfo);
        Logger.d(TAG, "updateNaviInfo " + b);
        return b;
    }

    /* 设置路口栅格图信息
     *1、设置路口大图信息，使用自带近接/混淆矢量大图显隐策略，如果没有设置数据，用户可自定义策略并调用SetViewPostureEvent触发功能
     *2、本接口暂时只对混淆\近接路口生效，当设置路口大图信息，用户调用SetViewPostureEvent无效（内部策略自动调用
     */
    public boolean setCrossImageInfo(MapType mapTypeId, @CrossType.CrossType1 int type, boolean useCustom) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setCrossImageInfo(type, useCustom);
        Logger.d(TAG, "setCrossImageInfo " + b);
        return b;
    }

    /* 设置近接/混淆矢量大图的姿态事件, 目前只有type = CrossTypeVector才有实现才有实现 */
    public boolean setViewPostureEvent(MapType mapTypeId, CrossType type, VectorCrossViewPostureEvent postureEvent) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setViewPostureEvent(type, postureEvent);
        Logger.d(TAG, "setViewPostureEvent " + b);
        return b;
    }

    /* 设置放大路口显示区域 */
    public boolean setRoadCrossRect(MapType mapTypeId, @CrossType.CrossType1 int crossType, RectInt viewRect) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerCross().setRoadCrossRect(crossType, viewRect);
        Logger.d(TAG, "setRoadCrossRect " + b);
        return b;
    }

    /*========================================= 路口大图 =========================================*/


    @Override
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerUser().clearAllItems();
        }
        layersPoolManager.get(mapTypeId).getLayerUser().updateGpsTrack(userTrackDepth);
    }

    @Override
    public void cleanLayerItemOfUserTrackDepth(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerUser().cleanGpsTrack();
    }

    @Override
    public void addLayerItemOfUserReceive(MapType mapTypeId, LayerItemUserReceive receive, boolean clearOtherLayerItem) {
        layersPoolManager.get(mapTypeId).getLayerUser().updateSendToCar(receive);
    }

    @Override
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerUser().clearAllItems();
        }
        layersPoolManager.get(mapTypeId).getLayerUser().updateFavoriteMain(favorites);
    }

    @Override
    public void cleanLayerItemOfFavorite(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerUser().cleanFavoriteToMain();
    }

    @Override
    public void flyLineVisible(MapType mapTypeId, boolean visible) {
        layersPoolManager.get(mapTypeId).getLayerFlyLine().setVisible(visible,true);
    }

    @Override
    public void flyLineHideOnce(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerFlyLine().hideOnce();
    }
}
