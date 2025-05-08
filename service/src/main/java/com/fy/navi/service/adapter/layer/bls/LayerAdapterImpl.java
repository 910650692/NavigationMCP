package com.fy.navi.service.adapter.layer.bls;

import android.graphics.Rect;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.fy.navi.service.adapter.layer.ILayerApi;
import com.fy.navi.service.adapter.layer.bls.impl.LayersPoolManager;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
import com.fy.navi.service.define.layer.refix.LayerItemRouteOdd;
import com.fy.navi.service.define.layer.refix.LayerItemSearchResult;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.layer.refix.LayerItemUserTrackDepth;
import com.fy.navi.service.define.layer.refix.LayerPointItemType;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlterChargeStationInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import java.util.ArrayList;

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
    public void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        layersPoolManager.addLayerClickCallback(mapTypeId,observer);
    }

    @Override
    public void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer) {
        layersPoolManager.removeClickCallback(mapTypeId,observer);
    }

    @Override
    public void unInitLayerService() {
        layersPoolManager.unInitLayerService();
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
        layersPoolManager.get(mapTypeId).getLayerCar().setCarMode(carMode);
    }

    @Override
    public CarModeType getCarModeType(MapType mapTypeId) {
        return layersPoolManager.get(mapTypeId).getLayerCar().getCarModeType();
    }

    @Override
    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {
        layersPoolManager.get(mapTypeId).getLayerCar().setPreviewMode(bPreview);
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
    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().drawRouteLine(routeResult);
    }

    /* 路线替换补能扎标 */
    public void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().updateRouteReplaceChargePoints(chargeStationInfos);
    }

    /* 更新终点扎标数据 */
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().updateRouteEndPoint(endPoint);
    }

    /* 更新Odd信息 */
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().updateOddInfo(oddInfoList, pathId);
    }

    /**
     * 更新引导路线数据
     * @param pathInfoList 路线数据
     * @param selectIndex 选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().updatePathInfo(pathInfoList, selectIndex);
    }

    /**
     * 删除途经点
     * @param pid 途经点id
     */
    public void removeViaPoint(MapType mapTypeId, String pid) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().removeViaPoint(pid);
    }

    /**
     * 设置路线样式风格
     * @param isStartNavi 是否开始导航
     * @param isOffLine 是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().setPathStyle(isStartNavi, isOffLine, isMultipleMode);
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index 隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().setPathVisible(index, isVisible);
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
     * 设置行前拥堵气泡是否显示
     */
    public boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow) {
        return layersPoolManager.get(mapTypeId).getLayerGuideRoute().setRouteJamBubblesVisible(isShow);
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
     * ====此方法后续废弃====
     */
    @Override
    public void openDynamicLevel(MapType mapTypeId, boolean isOpen) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().openDynamicLevel(isOpen);
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().openDynamicLevel(dynamicLevelMode);
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().closeDynamicLevel();
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().setDynamicLevelLock(dynamicLevelMode, isLock);
    }

    /* 设置自动比例尺是否主动调整地图中心 */
    public void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().openDynamicCenter(isDynaCenterLock);
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
    public void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index) {
        layersPoolManager.get(mapTypeId).getLayerSearch().setSelect(type, index);
    }

    @Override
    public void clearFavoriteMain(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerUser().clearFavoriteMain();
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        if (clearOtherLayerItem) {
            layersPoolManager.get(mapTypeId).getLayerSearch().clearAllItems();
        }
        boolean searchMarker = layersPoolManager.get(mapTypeId).getLayerSearch().updateSearchMarker(type, searchResult);
        Logger.d(TAG, "updateSearchMarker " + searchMarker);
        return searchMarker;
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
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType) {
        layersPoolManager.get(mapTypeId).getLayerSearch().clearSearchItemByType(searchItemType);
    }

    /*========================================= 路口大图 =========================================*/

    @Override
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerGuideRoute().showCross(crossEntity.getCrossImageEntity());
        Logger.d(TAG, "hideCross " + b);
        Logger.i("crossImageDebug", "showCross " + b);
        return b;
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        boolean b = layersPoolManager.get(mapTypeId).getLayerGuideRoute().hideCross(type);
        Logger.d(TAG, "hideCross " + b);
        Logger.i("crossImageDebug", "hideCross " + b);
        return b;
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(MapType mapTypeId, Rect rect) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().updateRoadCrossRect(rect);
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
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {
        layersPoolManager.get(mapTypeId).getLayerUser().updateFavoriteMain(favorites);
    }

    @Override
    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {
        layersPoolManager.get(mapTypeId).getLayerUser().removeFavoriteMain(poiInfoEntity);
    }

    @Override
    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        layersPoolManager.get(mapTypeId).getLayerUser().setFavoriteVisible(visible);
    }

    @Override
    public void openFlyLine(MapType mapTypeId, boolean visible) {
        layersPoolManager.get(mapTypeId).getLayerFlyLine().openFlyLine(visible);
    }

    /*=========================================↓ 扎标图层 ↓=========================================*/

    /*显示终点区域弹出框图层*/
    public boolean updatePopSearchPointInfo(MapType mapTypeId, LayerItemLabelResult labelResult) {
        return layersPoolManager.get(mapTypeId).getLayerLabel().updatePopSearchPointInfo(labelResult);
    }

    /*清除扎标*/
    public void clearLabelItem(MapType mapTypeId) {
        layersPoolManager.get(mapTypeId).getLayerLabel().clearLabelItem();
    }

    @Override
    public void setPassGray(MapType mapTypeId, boolean isSetGray) {
        layersPoolManager.get(mapTypeId).getLayerGuideRoute().setPassGreyMode(isSetGray);
    }

    /*=========================================↑ 扎标图层 ↑=========================================*/
}
