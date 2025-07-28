package com.sgm.navi.service.adapter.layer;

import android.graphics.Rect;

import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.CrossType;
import com.sgm.navi.service.AdapterConfig;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.LayerItemLabelResult;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.sgm.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.sgm.navi.service.define.layer.refix.LayerItemSearchResult;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteAlterChargeStationInfo;
import com.sgm.navi.service.define.route.RouteChargeStationParam;
import com.sgm.navi.service.define.search.PoiInfoEntity;

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

    private LayerItemSearchResult mLayerItemSearchResult;

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

    public boolean initLayerService() {
        return mLayerApi.initLayerService();
    }

    public boolean initLayer(MapType mapType) {
        return mLayerApi.initLayer(mapType);
    }

    public boolean unInitLayer(MapType mapType) {
        return mLayerApi.unInitLayer(mapType);
    }

    public void unInitLayerService() {
        mLayerApi.unInitLayerService();
    }

    public void setDefaultCarMode(MapType mapTypeId) {
        mLayerApi.setDefaultCarMode(mapTypeId);
    }

    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {
        mLayerApi.setCarPosition(mapTypeId, geoPoint);
    }

    public void setCarMode(MapType mapTypeId, CarModeType carMode) {
        mLayerApi.setCarMode(mapTypeId, carMode);
    }

    public CarModeType getCarModeType(MapType mapTypeId) {
        return mLayerApi.getCarModeType(mapTypeId);
    }

    /* 设置车标是否显示 */
    public void setCarLogoVisible(MapType mapTypeId, boolean visible) {
        mLayerApi.setCarLogoVisible(mapTypeId, visible);
    }

    /* 设置凯迪车型骨骼车标 */
    public void initCarLogoByFlavor(MapType mapTypeId, String flavor) {
        mLayerApi.initCarLogoByFlavor(mapTypeId, flavor);
    }

    /* 设置骨骼车标的基础缩放值 */
    public void setSkeletonBaseScale(MapType mapTypeId, float f) {
        mLayerApi.setSkeletonBaseScale(mapTypeId, f);
    }

    /* 设置3D车模缩放比例 */
    public void setModelScale(MapType mapTypeId, float f) {
        mLayerApi.setModelScale(mapTypeId, f);
    }

    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {
        mLayerApi.setPreviewMode(mapTypeId, bPreview);
    }

    public void setLockMapRollAngle(MapType mapTypeId, boolean isLock) {
        mLayerApi.setLockMapRollAngle(mapTypeId, isLock);
    }

    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        return mLayerApi.setFollowMode(mapTypeId, bFollow);
    }

    /* 路线全览 */
    public void showPreviewView(MapType mapTypeId) {
        mLayerApi.showPreviewView(mapTypeId);
    }

    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        mLayerApi.drawRouteLine(mapTypeId, routeResult);
    }

    /**
     * 只绘制当前路线
     */
    public void drawOnlyOneRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        mLayerApi.drawOnlyOneRouteLine(mapTypeId, routeResult);
    }

    /* 路线替换补能扎标 */
    public void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        mLayerApi.updateRouteReplaceChargePoints(mapTypeId, chargeStationInfos);
    }

    /*更新终点扎标数据*/
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        mLayerApi.updateRouteEndPoint(mapTypeId, endPoint);
    }

    /*自动添加的补能站数据*/
    public void updateRouteChargeStation(MapType mapTypeId, RouteChargeStationParam routeChargeStation) {
        mLayerApi.updateRouteChargeStation(mapTypeId, routeChargeStation);
    }

    /*更新途径点信息*/
    public void updateViaPointList(MapType mapTypeId, List<PoiInfoEntity> viaPointList) {
        mLayerApi.updateViaPointList(mapTypeId, viaPointList);
    }

    /*清除指定路线类型扎标*/
    public void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type) {
        mLayerApi.clearRouteItemByType(mapTypeId, type);
    }

    /* 更新Odd信息 */
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        mLayerApi.updateOddInfo(mapTypeId, oddInfoList, pathId);
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        mLayerApi.openDynamicLevel(mapTypeId, dynamicLevelMode);
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel(MapType mapTypeId) {
        mLayerApi.closeDynamicLevel(mapTypeId);
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {
        mLayerApi.setDynamicLevelLock(mapTypeId, dynamicLevelMode, isLock);
    }

    /* 设置自动比例尺是否主动调整地图中心 */
    public void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock) {
        mLayerApi.openDynamicCenter(mapTypeId, isDynaCenterLock);
    }

    /* 设置能量耗尽点扎标是否显示 只在全览态展示 */
    public void setRouteEnergyEmptyPointVisible(MapType mapTypeId, boolean isShow) {
        mLayerApi.setRouteEnergyEmptyPointVisible(mapTypeId, isShow);
    }

    /**
     * 设置路线样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        mLayerApi.setPathStyle(mapTypeId, isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index     隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        return mLayerApi.setPathVisible(mapTypeId, index, isVisible);
    }

    public boolean setPathVisible(MapType mapTypeId, long pathId, boolean isVisible) {
        return mLayerApi.setPathVisible(mapTypeId, pathId, isVisible);
    }

    /*删除途经点*/
    public void removeViaPoint(MapType mapTypeId, String pid) {
        mLayerApi.removeViaPoint(mapTypeId, pid);
    }

    /* 设置起点扎标是否显示 */
    public void setStartPointVisible(MapType mapTypeId, boolean visible) {
        mLayerApi.setStartPointVisible(mapTypeId, visible);
    }

    /* 途经点扎标设置是否选中 */
    public void setRoutePointSelect(MapType mapTypeId, LayerPointItemType type, boolean isSelect, int index) {
        mLayerApi.setRoutePointSelect(mapTypeId, type, isSelect, index);
    }

    /* 清除路线图层扎标focus */
    public void clearRoutePointFocus(MapType mapTypeId, LayerPointItemType type) {
        mLayerApi.clearRoutePointFocus(mapTypeId, type);
    }

    /* HUD样式初始化 */
    public void initGuideRouteHUDMode(MapType mapTypeId) {
        mLayerApi.initGuideRouteHUDMode(mapTypeId);
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList 路线数据
     * @param selectIndex  选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        return mLayerApi.updatePathInfo(mapTypeId, pathInfoList, selectIndex);
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

    public void clearRestrictPolyline(MapType mapTypeId) {
        mLayerApi.clearRestrictPolyline(mapTypeId);
    }

    public void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        mLayerApi.registerLayerClickObserver(mapTypeId, observer);
    }

    public void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        mLayerApi.unRegisterLayerClickObserver(mapTypeId, observer);
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

    public void clearFavoriteMain(MapType mapTypeId) {
        mLayerApi.clearFavoriteMain(mapTypeId);
    }


    /*========================================= 搜索图层接口定义=========================================*/

    public void updateMapLevel(MapType mapTypeId, float mapLevel) {
        mLayerApi.updateMapLevel(mapTypeId, mapLevel);
    }

    public void setSearchSelect(MapType mapTypeId, LayerPointItemType type, int index) {
        mLayerApi.selectSearchPoi(mapTypeId, type, index);
    }

    public void setSearchSelect(MapType mapTypeId, LayerPointItemType type, int index, boolean select) {
        mLayerApi.selectSearchPoi(mapTypeId, type, index, select);
    }

    public void setSearchSelect(MapType mapTypeId, LayerPointItemType type, int index, List<PoiInfoEntity> poiInfoEntities) {
        mLayerApi.selectSearchPoi(mapTypeId, type, index, poiInfoEntities);
    }

    /* 清除选中的扎标焦点 */
    public void clearFocus(MapType mapTypeId, LayerPointItemType type) {
        mLayerApi.clearFocus(mapTypeId, type);
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        mLayerItemSearchResult = searchResult;
        return mLayerApi.updateSearchMarker(mapTypeId, type, searchResult, clearOtherLayerItem);
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult result) {
        mLayerItemSearchResult = result;
        mLayerApi.updateSearchResult(mapTypeId, type, result);
    }

    /* 清除所有搜索扎标 */
    public void clearAllSearchLayerItems(MapType mapTypeId) {
        mLayerItemSearchResult = null;
        mLayerApi.clearAllSearchLayerItems(mapTypeId);
    }

    /* 清除搜索POI扎标 */
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType) {
        mLayerApi.clearSearchPOILayerItems(mapTypeId, searchItemType);
    }

    public LayerItemSearchResult getLastSearchResult() {
        if (null == mLayerItemSearchResult) {
            return new LayerItemSearchResult();
        }
        return mLayerItemSearchResult;
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

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(MapType mapTypeId, Rect rect) {
        mLayerApi.updateRoadCrossRect(mapTypeId, rect);
    }

    public Rect getRoadCrossRect(MapType mapTypeId) {
        return mLayerApi.getRoadCrossRect(mapTypeId);
    }

    public void setCrossImageInfo(MapType mapTypeId, CrossImageInfo info) {
        mLayerApi.setCrossImageInfo(mapTypeId, info);
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
     * @param mapTypeId mapTypeId
     * @param favorites favorites
     */
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {
        mLayerApi.addLayerItemOfFavorite(mapTypeId, favorites);
    }

    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {
        mLayerApi.removeFavoriteMain(mapTypeId, poiInfoEntity);
    }

    public void hideOrShowFavoriteMain(MapType mapTypeId, boolean isShow) {
        mLayerApi.hideOrShowFavoriteMain(mapTypeId, isShow);
    }

    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        mLayerApi.setFavoriteVisible(mapTypeId, visible);
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
