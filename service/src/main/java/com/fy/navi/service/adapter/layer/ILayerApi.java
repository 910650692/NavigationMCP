package com.fy.navi.service.adapter.layer;

import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.map.layer.model.LayerTexture;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.autonavi.gbl.map.layer.observer.IPrepareLayerStyle;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemDynamicLevel;
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

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public interface ILayerApi {

    /* 初始化业务图层优先级配置及内聚功能配置 */
    boolean initLayerService(MapType mapTypeId);

    void unInitLayerService();


    /*========================================= ROUTE LAYER START =========================================*/
    /* 全览参数句柄转换 */
    PreviewParams getPathResultBound(MapType mapTypeId, ArrayList<?> pathResult);

    /* 绘制路线 */
    void drawRouteLine(MapType mapTypeId, RouteLineLayerParam routeLineLayer);

    /* 选择路线 */
    void setSelectedPathIndex(MapType mapTypeId, int routeIndex);

    /* 清除路线 */
    void clearRouteLine(MapType mapTypeId);

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

    /*获取预计到达时间*/
    String getCurrentRouteTime(MapType mapTypeId);

    /*========================================= ROUTE LAYER START =========================================*/

    /**
     * 是否打开自动比例尺
     *
     * @param mapTypeId 地图类型
     * @param isOpen    开关状态
     */
    void openDynamicLevel(MapType mapTypeId, boolean isOpen);

    /*图层点击注册监听*/
    void registerLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer);

    /*移除图层注册监听*/
    void unRegisterLayerClickObserver(MapType mapTypeId, LayerType layerId, ILayerAdapterCallBack observer);

    /***停车场扎标***/
    void updateSearchParkPoi(MapType mapTypeId, ArrayList<NaviParkingEntity> parkList);

    /***清除停车场扎标***/
    void clearSearchParkPoi(MapType mapTypeId);

    /***设置停车场扎标是否选中***/
    void setParkFocus(MapType mapTypeId, String strID, boolean bFocus);

    /***更新车标***/
    void updateGuideCarStyle(MapType mapTypeId);

    /***开启巡航红绿灯***/
    void setVisibleCruiseSignalLight(MapType mapTypeId, boolean isVisible);

    /***开启导航红绿灯***/
    void setVisibleGuideSignalLight(MapType mapTypeId, boolean isVisible);

    /***计算两点之前的直线距离***/
    double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint);

    /*搜索结果POI获取焦点*/
    void selectSearchPoi(MapType mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus);

    /*收藏夹查看主图模式*/
    void updateFavoriteMain(MapType mapTypeId, List<GmBizUserFavoritePoint> list);

    void clearFavoriteMain(MapType mapTypeId);

    /**
     * 设置动态比例尺是否锁住
     * <p>
     * 对动态比例尺加锁或者解锁
     *
     * @param mapTypeId
     * @param isLock
     * @param type
     * @return
     */
    int setDynamicLevelLock(MapType mapTypeId, boolean isLock, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 设置自动比例尺是否主动调整地图中心
     *
     * 设置自动比例尺是否主动调整地图中心，该功能的前提条件：OpenDynamicLevel为true
     * @return 0 代表成功
     */
    int openDynamicCenter(MapType mapTypeId, boolean changeCenter);

    /**
     * 重置缓存的状态, 算路成功后调用此方法
     * <p>
     * 重置动态比例尺状态
     * 默认值type=DynamicLevelGuide
     */
    void resetDynamicLevel(MapType mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 是否处于锁住的状态
     * @param  mapTypeId
     * @param type
     * @return true 代表已锁住
     */
    boolean getDynamicLevelLock(MapType mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 获取动态比例尺的地图仰角的值
     * @param type
     * @return
     */
    float getDynamicLevelMapHeadDegree(MapType mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);



    /*========================================= 搜索图层接口定义=========================================*/

    /**
     * 父点+子点+中心点+出入口
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchResult(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索中心点
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchCentralPoi(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索起点、途经点、终点
     *
     * @param mapTypeId
     * @param searchBeginViaEnd
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchBeginEnd(MapType mapTypeId, LayerItemSearchBeginViaEnd searchBeginViaEnd, boolean clearOtherLayerItem);

    /**
     * 沿途搜点 + 气泡
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchAlongRoute(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索充电桩
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchChargeStation(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索停车场
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchPark(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索POI扎标
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchLabel(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索路线图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchLine(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 搜索区域图层
     *
     * @param mapTypeId
     * @param searchResult
     * @param clearOtherLayerItem
     * @return
     */
    boolean addLayerItemOfSearchPolygon(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 清除所有搜索扎标
     *
     * @param mapTypeId
     */
    void clearAllSearchLayerItems(MapType mapTypeId);

    /**
     * 清除搜索POI扎标
     *
     * @param mapTypeId
     */
    void clearSearchPOILayerItems(MapType mapTypeId);

    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 车标图层接口定义=========================================*/

    void setDefaultCarMode(MapType mapTypeId);

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    void setCarMode(MapType mapTypeId, LayerItemCar carMode);

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    void setCarPosition(MapType mapTypeId, GeoPoint geoPoint);

    /* 设置设置跟随模式、自由模式 */
    int setFollowMode(MapType mapTypeId, boolean bFollow);


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
     * 删除用户历史轨迹点图层+用户历史轨迹线
     *
     * @param mapTypeId mapTypeId
     */
    void cleanLayerItemOfUserTrackDepth(MapType mapTypeId);


    /**
     * Send2Car图层
     *
     * @param mapTypeId           mapTypeId
     * @param receive             receive
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    void addLayerItemOfUserReceive(MapType mapTypeId, LayerItemUserReceive receive, boolean clearOtherLayerItem);


    /**
     * 查看单个或者多个收藏点
     *
     * @param mapTypeId           mapTypeId
     * @param favorites           favorites
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem);


    /**
     * 删除收藏点
     *
     * @param mapTypeId mapTypeId
     */
    void cleanLayerItemOfFavorite(MapType mapTypeId);

    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口图层类型更新样式 */
    boolean updateCrossStyle(MapType mapTypeId, @CrossType.CrossType1 int crossType);

    /* 根据放大路口类型进行显示隐藏控制 */
    boolean setCrossVisible(MapType mapTypeId, @CrossType.CrossType1 int type, boolean bVisible);

    /* 设置栅格图图片数据 */
    boolean setRasterImageData(MapType mapTypeId, LayerItemCrossEntity crossEntity);

    /* 根据放大路口类型填充数据 */
    boolean updateCross(MapType mapTypeId, LayerItemCrossEntity crossEntity);

    /* 根据放大路口类型隐藏对应的路口大图 */
    boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type);

    /* 设置导航车首上还是北上模式 */
    boolean set3DCrossCarMode(MapType mapTypeId, boolean isCarUp);

    /* 设置3D飞线的路况信息 */
    boolean setFlyTmc(MapType mapTypeId, byte[] buffer, ArrayList<RealCityTmcParam> param);

    /* 更新3D精品大图引导信息 */
    boolean updateNaviInfo(MapType mapTypeId, NaviInfo naviInfo);

    /* 设置路口栅格图信息
     *1、设置路口大图信息，使用自带近接/混淆矢量大图显隐策略，如果没有设置数据，用户可自定义策略并调用SetViewPostureEvent触发功能
     *2、本接口暂时只对混淆\近接路口生效，当设置路口大图信息，用户调用SetViewPostureEvent无效（内部策略自动调用
     */
    boolean setCrossImageInfo(MapType mapTypeId, @CrossType.CrossType1 int type, boolean useCustom);

    /* 设置近接/混淆矢量大图的姿态事件, 目前只有type = CrossTypeVector才有实现才有实现 */
    boolean setViewPostureEvent(MapType mapTypeId, CrossType type, VectorCrossViewPostureEvent postureEvent);

    /* 设置放大路口显示区域 */
    boolean setRoadCrossRect(MapType mapTypeId, @CrossType.CrossType1 int crossType, RectInt viewRect);

    /*========================================= 路口大图 =========================================*/


    void flyLineVisible(MapType mapTypeId,boolean visible);
    void flyLineHideOnce(MapType mapTypeId);

}
