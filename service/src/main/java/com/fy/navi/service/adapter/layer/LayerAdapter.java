package com.fy.navi.service.adapter.layer;

import com.autonavi.gbl.guide.model.CrossType;
import com.fy.navi.service.AdapterConfig;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemSearchBeginViaEnd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserReceive;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.layer.refix.LayerSearchItemType;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapType;
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
        mLayerApi.setCarMode(mapTypeId, carMode);
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

    /*更新终点扎标样式*/
    public void updateEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        mLayerApi.updateEndPoint(mapTypeId, endPoint);
    }

    /**
     * 设置路线样式风格
     * @param isStartNavi 是否开始导航
     * @param isOffLine 是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        mLayerApi.setPathStyle(mapTypeId, isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index 隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        return mLayerApi.setPathVisible(mapTypeId, index, isVisible);
    }

     /*删除途经点*/
    public void removeViaPoint(MapType mapTypeId, String pid) {
        mLayerApi.removeViaPoint(mapTypeId, pid);
    }

    /**
     * 更新引导路线数据
     * @param pathInfoList 路线数据
     * @param selectIndex 选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        return mLayerApi.updatePathInfo(mapTypeId, pathInfoList, selectIndex);
    }

    /*设置行前拥堵气泡是否显示*/
    public boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow) {
        return mLayerApi.setRouteJamBubblesVisible(mapTypeId, isShow);
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

    public void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        mLayerApi.registerLayerClickObserver(mapTypeId, observer);
    }

    public void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        mLayerApi.unRegisterLayerClickObserver(mapTypeId, observer);
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

    /*========================================= 搜索图层接口定义=========================================*/

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        return mLayerApi.updateSearchMarker(mapTypeId, searchResult, clearOtherLayerItem);
    }

    /**
     * 删除扎标map数据
     * key -> BizSearchType
     */
    public boolean removeMapDataByKey(MapType mapTypeId, int key) {
        return mLayerApi.removeMapDataByKey(mapTypeId, key);
    }

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
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerSearchItemType searchItemType) {
        mLayerApi.clearSearchPOILayerItems(mapTypeId, searchItemType);
    }

    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 路口大图 =========================================*/

    /* 设置栅格图图片数据 */
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return mLayerApi.showCross(mapTypeId, crossEntity);
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        return mLayerApi.hideCross(mapTypeId, type);
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
     * sendtocar
     *
     * @param mapTypeId           mapTypeId
     * @param favorites           favorites
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem) {
        mLayerApi.addLayerItemOfFavorite(mapTypeId, favorites, clearOtherLayerItem);
    }

    public void openFlyLine(MapType mapTypeId, boolean visible) {
        mLayerApi.openFlyLine(mapTypeId, visible);
    }

    /*=========================================↓ 扎标图层 ↓=========================================*/

    /*显示终点区域弹出框图层*/
    public boolean updatePopSearchPointInfo(MapType mapTypeId, LayerItemLabelResult labelResult) {
        return mLayerApi.updatePopSearchPointInfo(mapTypeId, labelResult);
    }

    /*清除扎标*/
    public void clearLabelItem(MapType mapTypeId) {
        mLayerApi.clearLabelItem(mapTypeId);
    }

    /*=========================================↑ 扎标图层 ↑=========================================*/
}
