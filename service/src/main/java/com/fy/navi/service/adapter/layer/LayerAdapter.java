package com.fy.navi.service.adapter.layer;

import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.refix.LayerItemCar;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.LayerItemCar;
import com.fy.navi.service.define.layer.refix.LayerItemSearchBeginViaEnd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserReceive;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.layer.refix.LayerType;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviLayerTexture;
import com.fy.navi.service.define.navi.NaviParkingEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class LayerAdapter {
    private static final String LAYER_PKG_NAME = LayerAdapter.class.getPackage().getName();
    private static final String LAYER_CLS_NAME = "LayerAdapterImpl";

    private ILayerApi mLayerApi;

    public static LayerAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final LayerAdapter ra = new LayerAdapter();
    }

    private LayerAdapter() {
        mLayerApi = (ILayerApi) AdapterConfig.getObject(LAYER_PKG_NAME, LAYER_CLS_NAME);
    }

    public boolean initLayerService(MapType mapTypeId) {
        return mLayerApi.initLayerService(mapTypeId);
    }

    public void setDefaultCarMode(MapType mapTypeId) {
        mLayerApi.setDefaultCarMode(mapTypeId);
    }

    public void setCarMode(MapType mapTypeId, CarModeType carMode) {
        mLayerApi.setCarMode(mapTypeId, new LayerItemCar(carMode));
    }

    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        return mLayerApi.setFollowMode(mapTypeId, bFollow);
    }

    public PreviewParams getPathResultBound(MapType mapTypeId, ArrayList<?> pathResult) {
        return mLayerApi.getPathResultBound(mapTypeId, pathResult);
    }

    public void drawRouteLine(MapType mapTypeId, RouteLineLayerParam routeLineLayer) {
        mLayerApi.drawRouteLine(mapTypeId, routeLineLayer);
    }

    public boolean switchSelectedPath(MapType mapTypeId, int index) {
        return mLayerApi.switchSelectedPath(mapTypeId, index);
    }

    public void updatePathArrow(MapType mapTypeId) {
        mLayerApi.updatePathArrow(mapTypeId);
    }

    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {
        mLayerApi.setPathArrowSegment(mapTypeId, segmentsIndexs);
    }

    public String getCurrentRouteTime(MapType mapTypeId) {
        return mLayerApi.getCurrentRouteTime(mapTypeId);
    }

    public void unInitLayerService() {
        mLayerApi.unInitLayerService();
    }


    public void setSelectedPathIndex(MapType mapTypeId, int routeIndex) {
        mLayerApi.setSelectedPathIndex(mapTypeId, routeIndex);
    }

    public void clearRouteLine(MapType mapTypeId) {
        mLayerApi.clearRouteLine(mapTypeId);
    }

    public void showRestArea(MapType mapTypeId, ArrayList<?> pathInfoList, int index) {
        mLayerApi.showRestArea(mapTypeId, pathInfoList, index);
    }

    public void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem) {
        mLayerApi.showWeatherView(mapTypeId, weatherLabelItem);
    }

    public void showRestrictionView(MapType mapTypeId, Object object) {
        showRestrictionView(mapTypeId, object, 0);
    }

    public void showRestrictionView(MapType mapTypeId, Object object, int position) {
        mLayerApi.showRestrictionView(mapTypeId, object, position);
    }

    public void registerLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer) {
        mLayerApi.registerLayerClickObserver(mapTypeId, layerId, observer);
    }

    public void unRegisterLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer) {
        mLayerApi.unRegisterLayerClickObserver(mapTypeId, layerId, observer);
    }

    public void openDynamicLevel(MapType mapTypeId, boolean isOpen) {
        mLayerApi.openDynamicLevel(mapTypeId, isOpen);
    }

    public void updateSearchParkPoi(MapType mapTypeId, ArrayList<NaviParkingEntity> parkList) {
        mLayerApi.updateSearchParkPoi(mapTypeId, parkList);
    }

    public void clearSearchParkPoi(MapType mapTypeId) {
        mLayerApi.clearSearchParkPoi(mapTypeId);
    }

    public void setParkFocus(MapType mapTypeId, String strID, boolean bFocus) {
        mLayerApi.setParkFocus(mapTypeId, strID, bFocus);
    }

    public void updateGuideCarStyle(MapType mapTypeId) {
        mLayerApi.updateGuideCarStyle(mapTypeId);
    }

    public void setVisibleCruiseSignalLight(MapType mapTypeId, boolean isVisible) {
        mLayerApi.setVisibleCruiseSignalLight(mapTypeId, isVisible);
    }

    public void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible) {
        mLayerApi.setVisibleGuideSignalLight(mapTypeId, isVisible);
    }

    public double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint) {
        return mLayerApi.calcStraightDistance(startPoint, endPoint);
    }

    public void setSearchSelect(MapType mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus) {
        mLayerApi.selectSearchPoi(mapTypeId, type, strID, bFocus);
    }

    public void updateFavoriteMain(MapType mapTypeId, List<GmBizUserFavoritePoint> list) {
        mLayerApi.updateFavoriteMain(mapTypeId, list);
    }

    public void clearFavoriteMain(MapType mapTypeId) {
        mLayerApi.clearFavoriteMain(mapTypeId);
    }

    public int setDynamicLevelLock(MapType mapTypeId, boolean isLock, int type) {
        return mLayerApi.setDynamicLevelLock(mapTypeId, isLock, type);
    }

    public void resetDynamicLevel(MapType mapTypeId, int type) {
        mLayerApi.resetDynamicLevel(mapTypeId, type);
    }

    public boolean getDynamicLevelLock(MapType mapTypeId, int type) {
        return mLayerApi.getDynamicLevelLock(mapTypeId, type);
    }

    public float getDynamicLevelMapHeadDegree(MapType mapTypeId, int type) {
        return mLayerApi.getDynamicLevelMapHeadDegree(mapTypeId, type);
    }

    public int openDynamicCenter(MapType mapTypeId, boolean changeCenter) {
        return mLayerApi.openDynamicCenter(mapTypeId, changeCenter);
    }

    /*========================================= 搜索图层接口定义=========================================*/

    /**
     * 搜索POI 父点+子点+中心点
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchResult(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchResult(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索中心点
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchCentralPoi(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchCentralPoi(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索起点、途经点、终点
     *
     * @param mapTypeId
     * @param searchBeginViaEnd
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchBeginEnd(MapType mapTypeId, LayerItemSearchBeginViaEnd searchBeginViaEnd, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchBeginEnd(mapTypeId, searchBeginViaEnd, clearOtherLayerItem);
    }

    /**
     * 沿途搜点 + 气泡
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchAlongRoute(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchAlongRoute(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索 充电桩
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchChargeStation(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchChargeStation(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索 停车场
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchPark(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchPark(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索 POI扎标
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchLabel(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchLabel(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索路线图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchLine(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchLine(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 搜索区域图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    public boolean addLayerItemOfSearchPolygon(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.addLayerItemOfSearchPolygon(mapTypeId, searchResult, clearOtherLayerItem);
    }


    /**
     * 清除所有搜索扎标
     *
     * @param mapTypeId
     */
    public void clearAllSearchLayerItems(MapType mapTypeId) {
        mLayerApi.clearAllSearchLayerItems(mapTypeId);
    }

    /**
     * 清除搜索POI扎标
     *
     * @param mapTypeId
     */
    public void clearSearchPOILayerItems(MapType mapTypeId) {
        mLayerApi.clearSearchPOILayerItems(mapTypeId);
    }

    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口图层类型更新样式 */
    public boolean updateCrossStyle(MapType mapTypeId, @CrossType.CrossType1 int crossType) {
        return mLayerApi.updateCrossStyle(mapTypeId, crossType);
    }

    /* 根据放大路口类型进行显示隐藏控制 */
    public boolean setCrossVisible(MapType mapTypeId, @CrossType.CrossType1 int type, boolean bVisible) {
        return mLayerApi.setCrossVisible(mapTypeId, type, bVisible);
    }

    /* 设置栅格图图片数据 */
    public boolean setRasterImageData(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerApi.setRasterImageData(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型填充数据 */
    public boolean updateCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerApi.updateCross(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        return mLayerApi.hideCross(mapTypeId, type);
    }

    /* 设置导航车首上还是北上模式 */
    public boolean set3DCrossCarMode(MapType mapTypeId, boolean isCarUp) {
        return mLayerApi.set3DCrossCarMode(mapTypeId, isCarUp);
    }

    /* 设置3D飞线的路况信息 */
    public boolean setFlyTmc(MapType mapTypeId, byte[] buffer, ArrayList<RealCityTmcParam> param) {
        return mLayerApi.setFlyTmc(mapTypeId, buffer, param);
    }

    /* 更新3D精品大图引导信息 */
    public boolean updateNaviInfo(MapType mapTypeId, NaviInfo naviInfo) {
        return mLayerApi.updateNaviInfo(mapTypeId, naviInfo);
    }

    /* 设置路口栅格图信息
     *1、设置路口大图信息，使用自带近接/混淆矢量大图显隐策略，如果没有设置数据，用户可自定义策略并调用SetViewPostureEvent触发功能
     *2、本接口暂时只对混淆\近接路口生效，当设置路口大图信息，用户调用SetViewPostureEvent无效（内部策略自动调用
     */
    public boolean setCrossImageInfo(MapType mapTypeId, @CrossType.CrossType1 int type, boolean useCustom) {
        return mLayerApi.setCrossImageInfo(mapTypeId, type, useCustom);
    }

    /* 设置近接/混淆矢量大图的姿态事件, 目前只有type = CrossTypeVector才有实现才有实现 */
    public boolean setViewPostureEvent(MapType mapTypeId, CrossType type, VectorCrossViewPostureEvent postureEvent) {
        return mLayerApi.setViewPostureEvent(mapTypeId, type, postureEvent);
    }

    /* 设置放大路口显示区域 */
    public boolean setRoadCrossRect(MapType mapTypeId, @CrossType.CrossType1 int crossType, RectInt viewRect) {
        return mLayerApi.setRoadCrossRect(mapTypeId, crossType, viewRect);
    }

    /*========================================= 路口大图 =========================================*/



    /*========================================= 用户图层接口定义=========================================*/

    /**
     * 用户历史轨迹点图层
     *
     * @param mapTypeId           mapTypeId
     * @param userTrackDepth      userTrackDepth
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {
        mLayerApi.addLayerItemOfUserTrackDepth(mapTypeId, userTrackDepth, clearOtherLayerItem);
    }

    /**
     * 用户历史轨迹点图层清除
     *
     * @param mapTypeId mapTypeId
     */
    public void cleanLayerItemOfUserTrackDepth(MapType mapTypeId) {
        mLayerApi.cleanLayerItemOfUserTrackDepth(mapTypeId);
    }

    /**
     * sendtocar
     *
     * @param mapTypeId           mapTypeId
     * @param receive             receive
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    public void addLayerItemOfUserReceive(MapType mapTypeId, LayerItemUserReceive receive, boolean clearOtherLayerItem) {
        mLayerApi.addLayerItemOfUserReceive(mapTypeId, receive, clearOtherLayerItem);
    }

    /**
     * sendtocar
     *
     * @param mapTypeId           mapTypeId
     * @param favorites           favorites
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem) {
        mLayerApi.addLayerItemOfFavorite(mapTypeId, favorites, clearOtherLayerItem);
    }

    /**
     * 收藏清除
     *
     * @param mapTypeId mapTypeId
     */
    public void cleanLayerItemOfFavorite(MapType mapTypeId) {
        mLayerApi.cleanLayerItemOfFavorite(mapTypeId);
    }

    public void flyLineVisible(MapType mapTypeId, boolean visible) {
        mLayerApi.flyLineVisible(mapTypeId, visible);
    }

    public void flyLineHideOnce(MapType mapTypeId) {
        mLayerApi.flyLineHideOnce(mapTypeId);
    }
}
