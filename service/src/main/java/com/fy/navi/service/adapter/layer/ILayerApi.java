package com.fy.navi.service.adapter.layer;

import android.graphics.Rect;

import com.autonavi.gbl.guide.model.CrossType;
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

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public interface ILayerApi {

    /* 初始化业务图层优先级配置及内聚功能配置 */
    boolean initLayerService(MapType mapTypeId);

    void removeLayerService(MapType mapType);

    void unInitLayerService();


    /*========================================= ROUTE LAYER START =========================================*/
    /* 全览参数句柄转换 */
    PreviewParams.RectDouble getPathResultBound(MapType mapTypeId, ArrayList<?> pathResult);

    /* 绘制路线 */
    void drawRouteLine(MapType mapTypeId, RequestRouteResult routeResult);

    /* 路线替换补能扎标 */
    void updateRouteReplaceChargePoints(MapType mapTypeId, ArrayList<RouteAlterChargeStationInfo> chargeStationInfos);

    /*更新终点扎标数据*/
    void updateRouteEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint);

    /* 更新Odd信息 */
    void updateOddInfo(MapType mapTypeId, ArrayList<LayerItemRouteOdd> oddInfoList, long pathId);

    /* 选择路线 */
    void setSelectedPathIndex(MapType mapTypeId, int routeIndex);

    /* 清除路线 */
    void clearRouteLine(MapType mapTypeId);

    /*清除指定路线类型扎标*/
    void clearRouteItemByType(MapType mapTypeId, LayerPointItemType type);

    /* 展示路线的服务区 */
    void showRestArea(MapType mapTypeId, ArrayList<?> pathInfoList, int index);

    /* 展示路线的天气 */
    void showWeatherView(MapType mapTypeId, ArrayList<?> weatherLabelItem);

    /* 展示限行区域 */
    void showRestrictionView(MapType mapTypeId, Object object, int position);

    /* 切换路线 */
    boolean switchSelectedPath(MapType mapTypeId, int index);

    /*更新路线上的箭头*/
    void updatePathArrow(MapType mapTypeId);

    /*设置转向箭头要显示导航段*/
    void setPathArrowSegment(MapType mapTypeId, ArrayList<Long> segmentsIndexs);

    /*设置行前拥堵气泡是否显示*/
    boolean setRouteJamBubblesVisible(MapType mapTypeId, boolean isShow);

    /**
     * 设置路线样式风格
     * @param isStartNavi 是否开始导航
     * @param isOffLine 是否离线
     * @param isMultipleMode 是否多备选模式
     */
    void setPathStyle(MapType mapTypeId, boolean isStartNavi, boolean isOffLine, boolean isMultipleMode);

    /**
     * 隐藏分歧备选路线
     *
     * @param index 隐藏路线下标 -> list下标 默认0开始
     * @param isVisible 路线是否显示 -> 隐藏需传入false
     */
    boolean setPathVisible(MapType mapTypeId, int index, boolean isVisible);

    /**
     * 更新引导路线数据
     * @param pathInfoList 路线数据
     * @param selectIndex 选中下标
     */
    boolean updatePathInfo(MapType mapTypeId, ArrayList<?> pathInfoList, int selectIndex);

     /* 删除途经点 */
    void removeViaPoint(MapType mapTypeId, String pid);

    /* 设置起点扎标是否显示 */
    void setStartPointVisible(MapType mapTypeId, boolean visible);

    /*========================================= ROUTE LAYER START =========================================*/

    /*是否打开自动比例尺*/
    void openDynamicLevel(MapType mapTypeId, boolean isOpen);

    /*图层点击注册监听*/
    void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer);

    /*移除图层注册监听*/
    void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer);

    /***更新车标***/
    void updateGuideCarStyle(MapType mapTypeId);

    /***开启巡航红绿灯***/
    void setVisibleCruiseSignalLight(MapType mapTypeId, boolean isVisible);

    /***开启导航红绿灯***/
    void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible);

    /***计算两点之前的直线距离***/
    double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint);

    void clearFavoriteMain(MapType mapTypeId);

    /* 是否打开动态比例尺功能，type区分巡航动态比例尺还是导航动态比例尺 */
    void openDynamicLevel(MapType mapTypeId, DynamicLevelMode dynamicLevelMode);

    /* 关闭动态比例尺 */
    void closeDynamicLevel(MapType mapTypeId);

    /* 设置动态比例尺是否锁住状态，type区分巡航动态比例尺还是导航动态比例尺 */
    void setDynamicLevelLock(MapType mapTypeId, DynamicLevelMode dynamicLevelMode, boolean isLock);

    /* 设置自动比例尺是否主动调整地图中心 */
    void openDynamicCenter(MapType mapTypeId, boolean isDynaCenterLock);

    /*========================================= ROUTE LAYER END =========================================*/





    /*========================================= 搜索图层接口定义=========================================*/

    /*搜索结果POI获取焦点*/
    void selectSearchPoi(MapType mapTypeId, LayerPointItemType type, int index);

    /*清除搜索结果POI获取焦点*/
    void clearFocus(MapType mapTypeId, LayerPointItemType type);

    /* 搜索图层扎标接口 */
    boolean updateSearchMarker(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /* 更新列表可视扎标数据 */
    void updateSearchResult(MapType mapTypeId, LayerPointItemType type, LayerItemSearchResult result);

    /*清除所有搜索扎标*/
    void clearAllSearchLayerItems(MapType mapTypeId);

    /*清除搜索POI扎标*/
    void clearSearchPOILayerItems(MapType mapTypeId, LayerPointItemType searchItemType);

    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 车标图层接口定义=========================================*/

    void setDefaultCarMode(MapType mapTypeId);

    /* 设置车标模式 */
    void setCarMode(MapType mapTypeId, CarModeType carMode);

    /* 设置凯迪车型骨骼车标 */
    void initCarLogoByFlavor(MapType mapTypeId, String flavor);

    /***
     * 获取当前屏幕的车标模式
     * @param mapTypeId
     * @return
     */
    CarModeType getCarModeType(MapType mapTypeId);

    /* 设置车标是否显示 */
    void setCarLogoVisible(MapType mapTypeId, boolean visible);

    /* 设置车标预览模式 */
    void setPreviewMode(MapType mapTypeId, boolean bPreview);

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    void setCarPosition(MapType mapTypeId, GeoPoint geoPoint);

    /* 设置设置跟随模式、自由模式 */
    int setFollowMode(MapType mapTypeId, boolean bFollow);

    /* 设置骨骼车标的基础缩放值 */
    void setSkeletonBaseScale(MapType mapTypeId, float f);

    /* 设置3D车模缩放比例 */
    void setModelScale(MapType mapTypeId, float f);

    /*========================================= 车标图层接口定义=========================================*/


    /*========================================= 用户图层接口定义=========================================*/

    /**
     * 用户历史轨迹点图层+用户历史轨迹线
     *
     * @param mapTypeId           mapTypeId
     * @param userTrackDepth      userTrackDepth
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    void addLayerItemOfUserTrackDepth(MapType mapTypeId, LayerItemUserTrackDepth userTrackDepth, boolean clearOtherLayerItem);

    /**
     * 查看单个或者多个收藏点
     *
     * @param mapTypeId           mapTypeId
     * @param favorites           favorites
     */
    void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites);


    /**
     * 删除单个收藏点
     *
     */
    void removeFavoriteMain(MapType mapTypeId, PoiInfoEntity poiInfoEntity);


    /**
     * 显示隐藏收藏点
     *
     * @param mapTypeId           mapTypeId
     * @param visible visible
     */
    void setFavoriteVisible(MapType mapTypeId, boolean visible);


    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口类型填充数据 */
    boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity);

    /* 根据放大路口类型隐藏对应的路口大图 */
    boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type);

    /* 动态更新路口大图显示区域 */
    void updateRoadCrossRect(MapType mapTypeId, Rect rect);

    Rect getRoadCrossRect(MapType mapTypeId);

    /*========================================= 路口大图 =========================================*/


    /*========================================= 移图选点 =========================================*/
    void openFlyLine(MapType mapTypeId, boolean visible);
    /*========================================= 移图选点 =========================================*/

    /*=========================================↓ 扎标图层 ↓=========================================*/

    /*显示终点区域弹出框图层*/
    boolean updatePopSearchPointInfo(MapType mapTypeId, LayerItemLabelResult labelResult);

    /*清除扎标*/
    void clearLabelItem(MapType mapTypeId);

    /*=========================================↑ 扎标图层 ↑=========================================*/
}
