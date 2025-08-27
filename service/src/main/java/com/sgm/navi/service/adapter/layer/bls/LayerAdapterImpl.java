package com.sgm.navi.service.adapter.layer.bls;

import android.graphics.Rect;

import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.layer.ILayerAdapterCallBack;
import com.sgm.navi.service.adapter.layer.ILayerApi;
import com.sgm.navi.service.adapter.layer.bls.impl.LayersPool;
import com.sgm.navi.service.adapter.layer.bls.impl.LayersPoolManager;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
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
import com.sgm.navi.service.define.route.RouteParam;
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
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setCarMode(carMode);
        }
    }

    @Override
    public CarModeType getCarModeType(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerCar().getCarModeType();
        }
        return CarModeType.CAR_MODE_DEFAULT;
    }

    /* 设置车标是否显示 */
    public void setCarLogoVisible(MapType mapTypeId, boolean visible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setCarLogoVisible(visible);
        }
    }

    /* 设置凯迪车型骨骼车标 */
    public void initCarLogoByFlavor(MapType mapTypeId, String flavor) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().initCarLogoByFlavor(flavor);
        }
    }

    /* 设置骨骼车标的基础缩放值 */
    public void setSkeletonBaseScale(MapType mapTypeId, float f) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setSkeletonBaseScale(f);
        }
    }

    /* 设置3D车模缩放比例 */
    public void setModelScale(MapType mapTypeId, float f) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setModelScale(f);
        }
    }

    @Override
    public void setPreviewMode(MapType mapTypeId, boolean bPreview) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setPreviewMode(bPreview);
        }
    }

    @Override
    public void setLockMapRollAngle(MapType mapTypeId, boolean isLock) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setLockMapRollAngle(isLock);
        }
    }

    @Override
    public void setCarPosition(MapType mapTypeId, GeoPoint geoPoint) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setCarPosition(geoPoint);
        }
    }

    @Override
    public int setFollowMode(MapType mapTypeId, boolean bFollow) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().setFollowMode(bFollow);
        }
        return 1;
    }

    /* 路线全览 */
    public void showPreviewView(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().showPreviewView();
        }
    }

    /**
     * 绘制路线
     *
     * @param mapTypeId
     * @param routeResult
     */
    @Override
    public void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().drawRouteLine(routeResult);
        }
    }

    /* 显示终点名称 */
    @Override
    public void showEndAreaPoint(MapType mapTypeId, RouteParam routeParam) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerArea().showEndAreaPoint(routeParam);
        }
    }

    /* 清除终点名称 */
    @Override
    public void clearEndAreaPoint(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerArea().clearEndAreaPoint();
        }
    }

    /**
     * 只绘制当前路线
     *
     * @param routeResult
     */
    public void drawOnlyOneRouteLine(MapType mapTypeId, RequestRouteResult routeResult) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().drawOnlyOneRouteLine(routeResult);
        }
    }

    /* 设置路线图层扎标是否选中 */
    public void setRoutePointSelect(MapType mapTypeId, LayerPointItemType type, boolean isSelect, int index) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setSelect(type, isSelect, index);
        }
    }

    /* 清除路线图层扎标focus */
    public void clearRoutePointFocus(MapType mapTypeId, LayerPointItemType type) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().clearFocus(type);
        }
    }

    /* 设置起点扎标是否显示 */
    public void setStartPointVisible(MapType mapTypeId, boolean visible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setStartPointVisible(visible);
        }
    }

    /* HUD样式初始化 */
    public void initGuideRouteHUDMode(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().initGuideRouteHUDMode();
        }
    }

    /* 路线替换补能扎标 */
    public void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateRouteReplaceChargePoints(chargeStationInfos);
        }
    }

    /* 更新终点扎标数据 */
    public void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateRouteEndPoint(endPoint);
        }
    }

    /*自动添加的补能站数据*/
    public void updateRouteChargeStation(MapType mapTypeId, RouteChargeStationParam routeChargeStation) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateRouteChargeStation(routeChargeStation);
        }
    }

    /*更新途径点信息*/
    public void updateViaPointList(MapType mapTypeId, List<PoiInfoEntity> viaPointList) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateViaPointList(viaPointList);
        }
    }

    /* 更新Odd信息 */
    public void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateOddInfo(oddInfoList, pathId);
        }
    }

    /**
     * 更新引导路线数据
     *
     * @param pathInfoList 路线数据
     * @param selectIndex  选中下标
     */
    public boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().updatePathInfo(pathInfoList, selectIndex);
        }
        return false;
    }

    /**
     * 删除途经点
     *
     * @param pid 途经点id
     */
    public void removeViaPoint(MapType mapTypeId, String pid) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().removeViaPoint(pid);
        }
    }

    /*清除指定路线类型扎标*/
    public void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().clearRouteItemByType(type);
        }
    }

    /**
     * 设置路线样式风格
     *
     * @param isStartNavi    是否开始导航
     * @param isOffLine      是否离线
     * @param isMultipleMode 是否多备选模式
     */
    public void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setPathStyle(isStartNavi, isOffLine, isMultipleMode);
        }
    }

    /**
     * 隐藏分歧备选路线
     *
     * @param index     隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    public boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().setPathVisible(index, isVisible);
        }
        return false;
    }

    public boolean setPathVisible(MapType mapTypeId, long pathId, boolean isVisible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().setPathVisible(pathId, isVisible);
        }
        return false;
    }

    /**
     * 选择路线
     *
     * @param mapTypeId
     * @param routeIndex
     */
    @Override
    public void setSelectedPathIndex(MapType mapTypeId, int routeIndex) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setSelectedPathIndex(routeIndex);
        }
    }

    /**
     * 清除路线
     *
     * @param mapTypeId
     */
    @Override
    public void clearRouteLine(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().clearPaths();
        }
    }

    /**
     * 设置行前拥堵气泡是否显示
     */
    public boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().setRouteJamBubblesVisible(isShow);
        }
        return false;
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
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().showRestArea(pathInfoList, index);
        }
    }

    /**
     * 展示路线的天气
     *
     * @param mapTypeId
     * @param weatherLabelItem
     */
    @Override
    public void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().showWeatherView(weatherLabelItem);
        }
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
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerArea().showRestrictionView(object, position);
        }
    }

    /**
     * 清除限行线
     * @param mapTypeId
     */
    @Override
    public void clearRestrictPolyline(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerArea().clearRestrictPolyline();
        }
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
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().switchSelectedPath(index);
        }
        return false;
    }

    /**
     * 更新路线上的箭头
     *
     * @param mapTypeId
     */
    @Override
    public void updatePathArrow(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updatePathArrow();
        }
    }

    /**
     * 设置转向箭头要显示导航段
     *
     * @param mapTypeId
     * @param segmentsIndexs
     */
    @Override
    public void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setPathArrowSegment(segmentsIndexs);
        }
    }

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    public void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().openDynamicLevel(dynamicLevelMode);
        }
    }

    /* 关闭动态比例尺 */
    public void closeDynamicLevel(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().closeDynamicLevel();
        }
    }

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    public void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setDynamicLevelLock(dynamicLevelMode, isLock);
        }
    }

    /* 设置自动比例尺是否主动调整地图中心 */
    public void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().openDynamicCenter(isDynaCenterLock);
        }
    }

    /* 设置能量耗尽点扎标是否显示 只在全览态展示 */
    public void setRouteEnergyEmptyPointVisible(MapType mapTypeId, boolean isShow) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setRouteEnergyEmptyPointVisible(isShow);
        }
    }

    @Override
    public void updateGuideCarStyle(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerCar().updateGuideCarStyle();
        }
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
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().setSelect(type, index);
        }
    }

    @Override
    public void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index, boolean select) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().setSelect(type, index, select);
        }
    }

    @Override
    public void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index, List<PoiInfoEntity> poiInfoEntities) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().setSelect(type, index, poiInfoEntities);
        }
    }

    @Override
    public void clearFocus(MapType mapTypeId, LayerPointItemType type) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().clearFocus(type);
        }
    }

    @Override
    public void clearFavoriteMain(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerUser().clearFavoriteMain();
        }
    }

    public void updateMapLevel(MapType mapTypeId, float mapLevel) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().updateMapLevel(mapLevel);
        }
    }

    /* 搜索图层扎标接口 */
    public boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (clearOtherLayerItem) {
            if (Logger.openLog) {
                Logger.d(TAG, "updateSearchMarker clearOtherLayerItem ", clearOtherLayerItem);
            }
            if (null != layersPool) {
                layersPool.getLayerSearch().clearAllItems();
            }
        }
        if (null != layersPool) {
            return layersPool.getLayerSearch().updateSearchMarker(type, searchResult);
        }
        return false;
    }

    /* 更新列表可视扎标数据 */
    public void updateSearchResult(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult result) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().updateSearchResult(type, result);
        }
    }

    /**
     * 清除所有搜索扎标
     *
     * @param mapTypeId
     */
    public void clearAllSearchLayerItems(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().clearAllItems();
        }
    }

    /**
     * 清除搜索POI扎标
     *
     * @param mapTypeId
     */
    public void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerSearch().clearSearchItemByType(searchItemType);
        }
    }

    /*========================================= 路口大图 =========================================*/

    @Override
    public boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().showCross(crossEntity.getCrossImageEntity());
        }
        return false;
    }

    /* 根据放大路口类型隐藏对应的路口大图 */
    public boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().hideCross(type);
        }
        return false;
    }

    /* 动态更新路口大图显示区域 */
    public void updateRoadCrossRect(MapType mapTypeId, Rect rect) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateRoadCrossRect(rect);
        }
    }

    public Rect getRoadCrossRect(MapType mapTypeId) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            return layersPool.getLayerGuideRoute().getRoadCrossRect();
        }
        return new Rect();
    }

    public void setCrossImageInfo(MapType mapTypeId, CrossImageInfo info) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().setCrossImageInfo(info);
        }
    }

    /*========================================= 路口大图 =========================================*/


    @Override
    public void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (clearOtherLayerItem) {
            if (Logger.openLog) {
                Logger.d(TAG, "addLayerItemOfUserTrackDepth clearOtherLayerItem ", clearOtherLayerItem);
            }
            if (null != layersPool) {
                layersPool.getLayerUser().clearAllItems();
            }
        }
        if (null != layersPool) {
            layersPool.getLayerUser().updateGpsTrack(userTrackDepth);
        }
    }

    @Override
    public void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerUser().updateFavoriteMain(favorites);
        }
    }

    @Override
    public void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerUser().removeFavoriteMain(poiInfoEntity);
        }
    }

    @Override
    public void hideOrShowFavoriteMain(MapType mapTypeId, boolean isShow) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerUser().hideOrShowFavoriteMain(isShow);
        }
    }

    @Override
    public void setFavoriteVisible(MapType mapTypeId, boolean visible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerUser().setFavoriteVisible(visible);
        }
    }

    @Override
    public void openFlyLine(MapType mapTypeId, boolean visible) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerFlyLine().openFlyLine(visible);
        }
    }

    /*=========================================↓ 扎标图层 ↓=========================================*/

    /* 更新终点停车场扎标是否显示 */
    public void updateRouteEndPointParkViewVisible(MapType mapTypeId, boolean isShow) {
        LayersPool layersPool = layersPoolManager.getLayersPool(mapTypeId);
        if (null != layersPool) {
            layersPool.getLayerGuideRoute().updateRouteEndPointParkViewVisible(isShow);
        }
    }

    /*=========================================↑ 扎标图层 ↑=========================================*/
}
