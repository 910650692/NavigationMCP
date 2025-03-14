package com.fy.navi.service.adapter.map;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.PreviewParams;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.ArrayList;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public interface IMapApi {
    boolean init(MapTypeId mapTypeId);

    void unBindMapView(IBaseScreenMapView mapView);

    void registerCallback(MapTypeId mapTypeId, IMapAdapterCallback callback);

    void unRegisterCallback(MapTypeId mapTypeId, IMapAdapterCallback callback);

    /**
     * 销毁MapService 应用退出时执行
     */
    void unitMapService();

    /**
     * get 当前比例尺
     *
     * @return current level
     */
    float getCurrentZoomLevel(MapTypeId mapTypeId);

    /*获取缩放等级*/
    int getCurrentScale(MapTypeId mapTypeId);

    /**
     * set 当前比例尺
     *
     * @param level this level
     */
    void setZoomLevel(MapTypeId mapTypeId, float level);

    /*** 设置地图中线点在屏幕中的位置 **/
    void setMapCenterInScreen(MapTypeId mapTypeId, int x, int y);

    /*** 设置地图中线点 **/
    void setMapCenter(MapTypeId mapTypeId, GeoPoint coord3DDoubleBean);

    boolean setTrafficStates(MapTypeId mapTypeId, boolean isOpen);

    // 地图POI分类控制显隐
    void setCustomLabelTypeVisible(MapTypeId mapTypeId, ArrayList<Integer> typeList, boolean visible);

    void setMapViewTextSize(MapTypeId mapTypeId, float f);

    void initMapView(IBaseScreenMapView mapView);

    void unInitMapView(IBaseScreenMapView mapSurfaceView);

    MapMode getCurrentMapMode(MapTypeId mapTypeId);

    /*** 设置底图视角模式 **/
    void setMapMode(MapTypeId mapTypeId, MapMode mapMode);

    void setMapStateStyle(MapTypeId mapTypeId, MapStateStyle mapStateStyle);

    void goToCarPosition(MapTypeId mapTypeId, boolean bAnimation, boolean changeLevel);

    GeoPoint mapToLonLat(MapTypeId mapTypeId, double mapX, double mapY);

    MapSurfaceViewSizeParams getMapSurfaceParam(MapTypeId mapTypeId);

    /* 全览底图*/
    void showPreview(MapTypeId mapTypeId, PreviewParams previewParams);

    /*退出全览*/
    void exitPreview(MapTypeId mapTypeId);

    /*更新UI样式，暂时只适配了黑夜、白天*/
    void updateUiStyle(MapTypeId mapTypeId, int uiMode);

    String getMapBound(MapTypeId mapTypeId);

    /*设置是否开启3D建筑*/
    void set3DBuilding(MapTypeId mapTypeId, boolean isOpen);

    /***
     * 设置俯仰角
     * @param mapTypeId
     * @param pitch 角度 [0°,  85°]
     * @param isAnimation 是否需要动画
     * @param isSync 是否同步
     */
    void setPitchAngle(MapTypeId mapTypeId, float pitch, boolean isAnimation, boolean isSync);

    /**
     * 添加是否进入全览的回调
     * @param callback isEnterPreviewCallback
     */
    void addIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback);

    /**
     * @return 是否进入全览的状态
     */
    boolean getIsEnterPreview(MapTypeId mapTypeId);

    void removeIsEnterPreviewCallback(MapTypeId mapTypeId, IsEnterPreviewCallback callback);
}
