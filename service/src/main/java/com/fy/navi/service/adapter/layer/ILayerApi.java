package com.fy.navi.service.adapter.layer;

import com.autonavi.gbl.common.model.RectInt;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.map.layer.model.CarMode;
import com.autonavi.gbl.map.layer.model.RealCityTmcParam;
import com.autonavi.gbl.map.layer.model.VectorCrossViewPostureEvent;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.LayerItemLabelResult;
import com.fy.navi.service.define.layer.refix.LayerItemRouteEndPoint;
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
public interface ILayerApi {

    /* 初始化业务图层优先级配置及内聚功能配置 */
    boolean initLayerService(MapType mapTypeId);

    void unInitLayerService();


    /*========================================= ROUTE LAYER START =========================================*/
    /* 全览参数句柄转换 */
    PreviewParams getPathResultBound(MapType mapTypeId, ArrayList<?> pathResult);

    /* 绘制路线 */
    void drawRouteLine(MapType mapTypeId, RouteLineLayerParam routeLineLayer);

    /*更新终点扎标样式*/
    void updateEndPoint(MapType mapTypeId, LayerItemRouteEndPoint endPoint);

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

    /*========================================= ROUTE LAYER START =========================================*/

    /*是否打开自动比例尺*/
    void openDynamicLevel(MapType mapTypeId, boolean isOpen);

    /*图层点击注册监听*/
    void registerLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer);

    /*移除图层注册监听*/
    void unRegisterLayerClickObserver(MapType mapTypeId, ILayerAdapterCallBack observer);

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

    /*========================================= ROUTE LAYER END =========================================*/





    /*========================================= 搜索图层接口定义=========================================*/

    /* 搜索图层扎标接口 */
    boolean updateSearchMarker(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /**
     * 删除扎标map数据
     * key -> BizSearchType
     */
    boolean removeMapDataByKey(MapType mapTypeId, int key);

    /*父点+子点+中心点+出入口*/
    boolean addLayerItemOfSearchResult(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /*搜索中心点*/
    boolean addLayerItemOfSearchCentralPoi(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /*搜索POI扎标*/
    boolean addLayerItemOfSearchLabel(MapType mapTypeId, LayerItemSearchResult searchResult, boolean clearOtherLayerItem);

    /*清除所有搜索扎标*/
    void clearAllSearchLayerItems(MapType mapTypeId);

    /*清除搜索POI扎标*/
    void clearSearchPOILayerItems(MapType mapTypeId, LayerSearchItemType searchItemType);

    /*========================================= 搜索图层接口定义=========================================*/


    /*========================================= 车标图层接口定义=========================================*/

    void setDefaultCarMode(MapType mapTypeId);

    /* 设置车标模式 */
    void setCarMode(MapType mapTypeId, CarModeType carMode);

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
     * 查看单个或者多个收藏点
     *
     * @param mapTypeId           mapTypeId
     * @param favorites           favorites
     * @param clearOtherLayerItem clearOtherLayerItem
     */
    void addLayerItemOfFavorite(MapType mapTypeId, LayerItemUserFavorite favorites, boolean clearOtherLayerItem);

    /*========================================= 用户图层接口定义=========================================*/

    /*========================================= 路口大图 =========================================*/

    /* 根据放大路口类型填充数据 */
    boolean showCross(MapType mapTypeId, LayerItemCrossEntity crossEntity);

    /* 根据放大路口类型隐藏对应的路口大图 */
    boolean hideCross(MapType mapTypeId, @CrossType.CrossType1 int type);

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
