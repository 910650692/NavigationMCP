package com.sgm.navi.service.adapter.layer.bls;

import android.graphics.Rect;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.ILayerApi;
import com.sgm.navi.service.adapter.layer.bls.impl.LayersPoolManager;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
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
import com.sgm.navi.service.logicpaket.position.PositionPackage;

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
    public boolean initLayerService() {
        return layersPoolManager.initLayerService();
    }

    @Override
    public boolean initLayer(MapType mapType) {
        return layersPoolManager.initLayer(mapType);
    }

    @Override
    public boolean unInitLayer(MapType mapType) {
        return layersPoolManager.unInitLayer(mapType);
    }

    @Override
    public void unInitLayerService() {
        layersPoolManager.unInitLayerService();
    }

    @Override
    public void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        layersPoolManager.addLayerClickCallback(mapTypeId, observer);
    }

    @Override
    public void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        layersPoolManager.removeClickCallback(mapTypeId, observer);
    }


    @Override
    public void setDefaultCarMode(MapType mapTypeId) {
        setCarPosition(mapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                PositionPackage.getInstance().getLastCarLocation().getCourse()));
        setCarMode(mapTypeId, CarModeType.CAR_MODEL_BRAND);
        setFollowMode(mapTypeId, true);
    }

    @Override
    public void setCarMode(MapType mapTypeId, CarModeType carMode) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setCarMode(carMode);
    }

    @Override
    public CarModeType getCarModeType(MapType mapTypeId) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerCar().getCarModeType();
    }

    /* 设置车标是否显示 */
    public void setCarLogoVisible(MapType mapTypeId, boolean visible) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setCarLogoVisible(visible);
    }

    /* 设置凯迪车型骨骼车标 */
    public void initCarLogoByFlavor(MapType mapTypeId, String flavor) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().initCarLogoByFlavor(flavor);
    }

    /* 设置骨骼车标的基础缩放值 */
    public void setSkeletonBaseScale(MapType mapTypeId, float f) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setSkeletonBaseScale(f);
    }

    /* 设置3D车模缩放比例 */
    public void setModelScale(MapType mapTypeId, float f) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setModelScale(f);
    }

    @Override
    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setPreviewMode(bPreview);
    }

    @Override
    public void setLockMapRollAngle(MapType mapTypeId, boolean isLock) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setLockMapRollAngle(isLock);
    }

    @Override
    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setCarPosition(geoPoint);
    }

    @Override
    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().setFollowMode(bFollow);
        return 1;
    }

    /* 路线全览 */
    public void showPreviewView(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().showPreviewView();
    }

    /**
     * 绘制路线
     *
     * @param mapTypeId
     * @param routeResult
     */
    @Override
    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().drawRouteLine(mapTypeId, routeResult);
    }

    /* 途经点扎标设置是否选中 */
    public void setRouteViaPointSelectStatus(MapType mapTypeId, boolean isSelect, int index) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setRouteViaPointSelectStatus(isSelect, index);
    }

    /* 设置起点扎标是否显示 */
    public void setStartPointVisible(MapType mapTypeId, boolean visible) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setStartPointVisible(visible);
    }

    /* 路线替换补能扎标 */
    public void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateRouteReplaceChargePoints(chargeStationInfos);
    }

    /* 更新终点扎标数据 */
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateRouteEndPoint(endPoint);
    }

    /*自动添加的补能站数据*/
    public void updateRouteChargeStation(MapType mapTypeId, RouteChargeStationParam routeChargeStation) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateRouteChargeStation(routeChargeStation);
    }

    /*更新途径点信息*/
    public void updateViaPointList(MapType mapTypeId, List<PoiInfoEntity> viaPointList) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateViaPointList(viaPointList);
    }

    /* 更新Odd信息 */
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateOddInfo(oddInfoList, pathId);
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList 路线数据
     * @param selectIndex  选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updatePathInfo(pathInfoList, selectIndex);
    }

    /**
     * 删除途经点
     *
     * @param pid 途经点id
     */
    public void removeViaPoint(MapType mapTypeId, String pid) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().removeViaPoint(pid);
    }

    /*清除指定路线类型扎标*/
    public void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().clearRouteItemByType(type);
    }

    /**
     * 设置路线样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setPathStyle(isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index     隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setPathVisible(index, isVisible);
    }

    public boolean setPathVisible(MapType mapTypeId, long pathId, boolean isVisible) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setPathVisible(pathId, isVisible);
    }

    /**
     * 选择路线
     *
     * @param mapTypeId
     * @param routeIndex
     */
    @Override
    public void setSelectedPathIndex(MapType mapTypeId, int routeIndex) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setSelectedPathIndex(routeIndex);
    }

    /**
     * 清除路线
     *
     * @param mapTypeId
     */
    @Override
    public void clearRouteLine(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().clearPaths();
    }

    /**
     * 设置行前拥堵气泡是否显示
     */
    public boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setRouteJamBubblesVisible(isShow);
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
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().showRestArea(pathInfoList, index);
    }

    /**
     * 展示路线的天气
     *
     * @param mapTypeId
     * @param weatherLabelItem
     */
    @Override
    public void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().showWeatherView(weatherLabelItem);
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
        layersPoolManager.getLayersPool(mapTypeId).getLayerArea().showRestrictionView(object, position);
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
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().switchSelectedPath(index);
    }

    /**
     * 更新路线上的箭头
     *
     * @param mapTypeId
     */
    @Override
    public void updatePathArrow(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updatePathArrow();
    }

    /**
     * 设置转向箭头要显示导航段
     *
     * @param mapTypeId
     * @param segmentsIndexs
     */
    @Override
    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setPathArrowSegment(segmentsIndexs);
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().openDynamicLevel(dynamicLevelMode);
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().closeDynamicLevel();
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setDynamicLevelLock(dynamicLevelMode, isLock);
    }

    /* 设置自动比例尺是否主动调整地图中心 */
    public void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().openDynamicCenter(isDynaCenterLock);
    }

    /* 设置能量耗尽点扎标是否显示 只在全览态展示 */
    public void setRouteEnergyEmptyPointVisible(MapType mapTypeId, boolean isShow) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().setRouteEnergyEmptyPointVisible(isShow);
    }

    @Override
    public void updateGuideCarStyle(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerCar().updateGuideCarStyle();
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
    public void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().setSelect(type, index);
    }

    @Override
    public void clearFocus(MapType mapTypeId, LayerPointItemType type) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().clearFocus(type);
    }

    @Override
    public void clearFavoriteMain(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerUser().clearFavoriteMain();
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchMarker = layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().updateSearchMarker(type, searchResult);
        return searchMarker;
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult result) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().updateSearchResult(type, result);
    }

    /**
     * 清除所有搜索扎标
     *
     * @param mapTypeId
     */
    public void clearAllSearchLayerItems(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().clearAllItems();
    }

    /**
     * 清除搜索POI扎标
     *
     * @param mapTypeId
     */
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerSearch().clearSearchItemByType(searchItemType);
    }

    /*========================================= 路口大图 =========================================*/

    @Override
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().showCross(crossEntity.getCrossImageEntity());
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().hideCross(type);
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(MapType mapTypeId, Rect rect) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().updateRoadCrossRect(rect);
    }

    public Rect getRoadCrossRect(MapType mapTypeId) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerGuideRoute().getRoadCrossRect();
    }

    /*========================================= 路口大图 =========================================*/


    @Override
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.getLayersPool(mapTypeId).getLayerUser().clearAllItems();
        }
        layersPoolManager.getLayersPool(mapTypeId).getLayerUser().updateGpsTrack(userTrackDepth);
    }

    @Override
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerUser().updateFavoriteMain(favorites);
    }

    @Override
    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerUser().removeFavoriteMain(poiInfoEntity);
    }

    @Override
    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerUser().setFavoriteVisible(visible);
    }

    @Override
    public void openFlyLine(MapType mapTypeId, boolean visible) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerFlyLine().openFlyLine(visible);
    }

    /*=========================================↓ 扎标图层 ↓=========================================*/

    /*显示终点区域弹出框图层*/
    public boolean updatePopSearchPointInfo(MapType mapTypeId, LayerItemLabelResult labelResult) {
        return layersPoolManager.getLayersPool(mapTypeId).getLayerLabel().updatePopSearchPointInfo(labelResult);
    }

    /*清除扎标*/
    public void clearLabelItem(MapType mapTypeId) {
        layersPoolManager.getLayersPool(mapTypeId).getLayerLabel().clearLabelItem();
    }
    /*=========================================↑ 扎标图层 ↑=========================================*/
}
