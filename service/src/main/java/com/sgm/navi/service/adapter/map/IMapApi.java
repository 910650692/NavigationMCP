package com.sgm.navi.service.adapter.map;

import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.PreviewParams;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapStateStyle;
import com.sgm.navi.service.define.map.MapViewParams;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.PointDataInfo;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.mfc.MfcController;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public interface IMapApi {

    void initMapService();

    void unInitMapService();

    /**
     * 初始化 MapView
     *
     * @param mapTypeId
     * @return
     */
    boolean createMapView(MapType mapTypeId);

    /**
     * 绑定一个map
     *
     * @param mapView
     */
    void bindMapView(IBaseScreenMapView mapView);

    /**
     * 解绑一个map
     *
     * @param mapView
     */
    void unBindMapView(IBaseScreenMapView mapView);

    /**
     * 销毁MapService 应用退出时执行
     */
    void destroyMapView(MapType mapTypeId);

    /**
     * 注册回调
     *
     * @param mapTypeId
     * @param callback
     */
    void registerCallback(MapType mapTypeId, IMapAdapterCallback callback);

    /**
     * 移除回调
     *
     * @param mapTypeId
     * @param callback
     */
    void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback callback);


    /**
     * get 当前比例尺
     *
     * @return current level
     */
    float getCurrentZoomLevel(MapType mapTypeId);

    /* 判断当前mapview是否存在 */
    boolean isMapViewExist(MapType mapTypeId);

    /*获取缩放等级*/
    int getCurrentScale(MapType mapTypeId);

    /**
     * set 当前比例尺
     *
     * @param level this level
     */
    void setZoomLevel(MapType mapTypeId, float level);

    /*** 设置地图中线点在屏幕中的位置 **/
    void setMapCenterInScreen(MapType mapTypeId, int x, int y);

    /**
     * 设置Hud地图中线点在屏幕中的位置
     */
    void setHudMapCenterInScreen(MapType mapTypeId, int x, int y);

    /*** 设置地图中线点 **/
    void setMapCenter(MapType mapTypeId, GeoPoint coord3DDoubleBean);

    /*** 获取地图中线点 **/
    GeoPoint getMapCenter(MapType mapTypeId);

    boolean setTrafficStates(MapType mapTypeId, boolean isOpen);

    // 地图POI分类控制显隐
    void setCustomLabelTypeVisible(MapType mapTypeId, ArrayList<Integer> typeList, boolean visible);

    void setMapViewTextSize(MapType mapTypeId, float f);


    MapMode getCurrentMapMode(MapType mapTypeId);

    /*** 设置底图视角模式 **/
    boolean setMapMode(MapType mapTypeId, MapMode mapMode);

    void setMapStateStyle(MapType mapTypeId, MapStateStyle mapStateStyle);

    void goToCarPosition(MapType mapTypeId, boolean bAnimation, boolean changeLevel);

    /* mfc移动地图*/
    void mfcMoveMap(MapType mapTypeId, MfcController mfcController, int moveDistance);

    GeoPoint mapToLonLat(MapType mapTypeId, double mapX, double mapY);

    PointDataInfo lonLatToScreen(MapType mapTypeId, double lon, double lat, double z);

    MapViewParams getMapSurfaceParam(MapType mapTypeId);

    /* 全览底图*/
    void showPreview(MapType mapTypeId, PreviewParams previewParams);

    /*退出全览*/
    void exitPreview(MapType mapTypeId);

    /*更新UI样式，暂时只适配了黑夜、白天*/
    void updateUiStyle(MapType mapTypeId, ThemeType uiMode);

    String getMapBound(MapType mapTypeId);

    /*设置是否开启3D建筑*/
    void set3DBuilding(MapType mapTypeId, boolean isOpen);

    void setMapLabelClickable(MapType mapTypeId, boolean enable);

    /**
     * 刷帧
     */
    void resetTickCount(MapType mapTypeId, int tickCount);

    /**
     * 计算两点距离
     *
     * @param startPoint
     * @param endPoint
     * @return
     */
    double calcStraightDistance(GeoPoint startPoint, GeoPoint endPoint);
}
