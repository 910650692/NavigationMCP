package com.fy.navi.service.adapter.layer;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.layer.CarModeType;
import com.fy.navi.service.define.layer.GemDynamicLevel;
import com.fy.navi.service.define.layer.GemLayerClickBusinessType;
import com.fy.navi.service.define.layer.LayerType;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.SearchResultLayer;
import com.fy.navi.service.define.layer.bls.CarLocation;
import com.fy.navi.service.define.map.GmBizUserFavoritePoint;
import com.fy.navi.service.define.map.MapTypeId;
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
    void initLayerService();

    /* 初始化业务图层的内部图层样式纹理，如果使用自定义图层样式，则无需调用该接口 */
    void initInnerStyle();

    /* 初始化业务图层的内部图层样式纹理，如果使用默认图层样式，则无需调用该接口 */
    void setCustomLayerStyle();

    void unInitLayerService();

    /*------------------ MAP LAYER START ------------------------------------*/

    void setDefaultCarMode(MapTypeId mapTypeId);

    /* 控制车标图层显隐 */
    void setCarModeVisible(MapTypeId mapTypeId, boolean isVisible);

    /* 控制车标图层是否可点击 */
    void setCarModeClickable(MapTypeId mapTypeId, boolean bClickable);

    /* 设置车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    void setCarMode(MapTypeId mapTypeId, @CarModeType.CarModelTypeId int carMode);

    int getCarMode(MapTypeId mapTypeId);

    /* 获取当前车标模式，2D车标/3D车标/骨骼车标/车速车标 */
    int getCurrentCarModeType(MapTypeId mapTypeId);

    /* 设置车头朝上模式 */
    void setCarUpMode(MapTypeId mapTypeId, boolean bCarUp);

    /* 设置车标缩放系数和比例尺对应关系 */
    boolean setCarScaleByMapLevel(MapTypeId mapTypeId, float[] vScales);

    /* 设置车标位置信息。通常用于单次设置车标位置，频次低 */
    void setCarPosition(MapTypeId mapTypeId, CarLocation carLocation);

    /* 更新车标位置信息，用于定位引擎下发定位消息驱动使用，频次高 */
    void updateCarPosition(MapTypeId mapTypeId, CarLocation carLocation);

    /* 设置设置跟随模式、自由模式 */
    int setFollowMode(MapTypeId mapTypeId, boolean bFollow);

    /*------------------------------------------- MAP LAYER END ------------------------------------*/

    /*************************************** SEARCH LAYER START ***************************************/

    /* 搜索结果扎点，包含父子节点 */
    void addSearchPointMarker(MapTypeId mapTypeId, SearchResultLayer searchResultLayer);

    /* 搜索结果label扎点 */
    boolean addSearchLabelMarker(MapTypeId mapTypeId, SearchResultLayer.ChildPoint childPoint);

    /* 清除所有搜索相关图层 */
    void clearSearchAllItemLayer(MapTypeId mapTypeId);

    /*************************************** SEARCH LAYER END ***************************************/

    /*========================================= ROUTE LAYER START =========================================*/
    /* 全览参数句柄转换 */
    PreviewParams getPathResultBound(MapTypeId mapTypeId, ArrayList<?> pathResult);

    /* 绘制路线 */
    void drawRouteLine(RouteLineLayerParam routeLineLayer);

    /* 选择路线 */
    void setSelectedPathIndex(MapTypeId mapTypeId, int routeIndex);

    /* 清除路线 */
    void clearRouteLine(MapTypeId mapTypeId);

    /* 展示路线的服务区 */
    void showRestArea(MapTypeId mapTypeId, ArrayList<?> pathInfoList, int index);

    /* 展示路线的天气 */
    void showWeatherView(MapTypeId mapTypeId, ArrayList<?> weatherLabelItem);

    /* 展示限行区域 */
    void showRestrictionView(MapTypeId mapTypeId, Object object, int position);

    /* 切换路线 */
    boolean switchSelectedPath(MapTypeId mapTypeId, int index);

    /*更新路线上的箭头*/
    void updatePathArrow(MapTypeId mapTypeId);

    /*设置转向箭头要显示导航段*/
    void setPathArrowSegment(MapTypeId mapTypeId, ArrayList<Long> segmentsIndexs);

    String getCurrentRouteTime(MapTypeId mapTypeId);

    /*========================================= ROUTE LAYER START =========================================*/

    /*========================================= RODE CROSS LAYER START =========================================*/
    /* 显示路口大图 */
    boolean showCross(MapTypeId mapTypeId, CrossImageEntity crossInfo);

    /* 隐藏路口大图 */
    void hideCross(MapTypeId mapTypeId, int type);

    /* 隐藏路口大图 */
    void setVisible(MapTypeId mapTypeId, int type, boolean bVisible);

    /* 设置路口大图信息 */
    void setCrossImageInfo(MapTypeId mapTypeId, CrossImageEntity crossInfo);

    /* 根据放大路口类型填充数据 */
    boolean updateCross(MapTypeId mapTypeId, byte[] buff, int crossType);

    /* 设置栅格图图片数据 */
    boolean setRasterImageData(MapTypeId mapTypeId, NaviLayerTexture arrowImage, NaviLayerTexture roadImage);

    /*========================================= RODE CROSS LAYER END =========================================*/

    /*清除搜索扎标*/
    void clearSearchMarks(MapTypeId mapTypeId);

    /**
     * 是否打开自动比例尺
     *
     * @param mapTypeId 地图类型
     * @param isOpen    开关状态
     */
    void openDynamicLevel(MapTypeId mapTypeId, boolean isOpen);

    /*图层点击注册监听*/
    void registerLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer);

    /*移除图层注册监听*/
    void unRegisterLayerClickObserver(MapTypeId mapTypeId, @LayerType.LayerId int layerId, ILayerAdapterCallBack observer);

    /***停车场扎标***/
    void updateSearchParkPoi(MapTypeId mapTypeId, ArrayList<NaviParkingEntity> parkList);

    /***清除停车场扎标***/
    void clearSearchParkPoi(MapTypeId mapTypeId);

    /***设置停车场扎标是否选中***/
    void setParkFocus(MapTypeId mapTypeId, String strID, boolean bFocus);

    /***更新车标***/
    void updateGuideCarStyle(MapTypeId mapTypeId);

    /***开启巡航红绿灯***/
    void setVisibleCruiseSignalLight(MapTypeId mapTypeId, boolean isVisible);

    /***开启导航红绿灯***/
    void setVisibleGuideSignalLight(MapTypeId mapTypeId, boolean isVisible);

    /***计算两点之前的直线距离***/
    double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint);

    /*搜索结果POI获取焦点*/
    void selectSearchPoi(MapTypeId mapTypeId, GemLayerClickBusinessType type, String strID, boolean bFocus);

    /*收藏夹查看主图模式*/
    void updateFavoriteMain(MapTypeId mapTypeId, List<GmBizUserFavoritePoint> list);

    void clearFavoriteMain(MapTypeId mapTypeId);

    /**
     *设置动态比例尺是否锁住
     *
     * 对动态比例尺加锁或者解锁
     * @param mapTypeId
     * @param isLock
     * @param type
     * @return
     */
    int setDynamicLevelLock(MapTypeId mapTypeId, boolean isLock, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 设置自动比例尺是否主动调整地图中心
     *
     * 设置自动比例尺是否主动调整地图中心，该功能的前提条件：OpenDynamicLevel为true
     * @return 0 代表成功
     */
    int openDynamicCenter(MapTypeId mapTypeId, boolean changeCenter);

    /**
     * 重置缓存的状态, 算路成功后调用此方法
     *
     * 重置动态比例尺状态
     * 默认值type=DynamicLevelGuide
     */
    void resetDynamicLevel(MapTypeId mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 是否处于锁住的状态
     * @param  mapTypeId
     * @param type
     * @return true 代表已锁住
     */
    boolean getDynamicLevelLock(MapTypeId mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);

    /***
     * 获取动态比例尺的地图仰角的值
     * @param type
     * @return
     */
    float getDynamicLevelMapHeadDegree (MapTypeId mapTypeId, @GemDynamicLevel.GemDynamicLevelType int type);
}
