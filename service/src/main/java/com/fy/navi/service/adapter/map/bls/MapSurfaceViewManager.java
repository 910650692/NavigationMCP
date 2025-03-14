package com.fy.navi.service.adapter.map.bls;

import android.content.Context;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.map.MapService;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.define.map.MapStateStyle;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;

import java.util.Hashtable;

/**
 * 创建MapSurfaceView工厂
 */
public class MapSurfaceViewManager {

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private Hashtable<MapTypeId, MapSurfaceViewImp> mapSurfaceCache = new Hashtable<>();

    public MapSurfaceViewManager() {

    }

    public boolean init(MapService mapService, MapTypeId mapTypeId) {
        Logger.d(TAG, "MapSurfaceViewManager init :" + mapTypeId.toString());
        createMapSurfaceView(AppContext.mContext, mapService, mapTypeId, new MapSurfaceViewSizeParams());
        return true;
    }

    public void createMapSurfaceView(Context context, MapService mapService, MapTypeId mapTypeId, MapSurfaceViewSizeParams mapSurfaceViewSizeParams) {
        Logger.d(TAG, "createMapSurfaceView start :" + mapTypeId.toString());
        MapSurfaceViewImp mapSurfaceView = new MapSurfaceViewImp(context).createMapSurfaceViewImp(mapService, mapTypeId, mapSurfaceViewSizeParams);
        mapSurfaceView.setGestureConfigure();
        mapSurfaceView.setMapStyle(MapStateStyle.MAP_DEFAULT);
        mapSurfaceView.setOperatorPosture();
        mapSurfaceView.setOperatorGestureParam();
        mapSurfaceView.initSkyBox();
        mapSurfaceView.setOperatorBusinessParam(mapSurfaceView.getMapSurfaceViewSizeParams().screenWidth, mapSurfaceView.getMapSurfaceViewSizeParams().screenHeight);
        mapSurfaceCache.put(mapTypeId, mapSurfaceView);
        Logger.d(TAG, "createMapSurfaceView end :" + mapTypeId.toString());
    }

    public void unInitMapView(MapTypeId mapTypeId) {
        MapSurfaceViewImp mapSurfaceViewImp = mapSurfaceCache.remove(mapTypeId);
        if (mapSurfaceViewImp != null) {
            mapSurfaceViewImp.removeAllCallback();
        }
    }

    public MapSurfaceViewImp getMapSurfaceView(MapTypeId mapTypeId) {
        return ConvertUtils.isNullRequire(mapSurfaceCache.get(mapTypeId), "获取对应的MapSurfaceViewImp失败 : " + mapTypeId.toString());
    }

    public void registerCallback(MapTypeId mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "registerCallback");
        getMapSurfaceView(mapTypeId).registerCallback(callback);
    }

    public void unRegisterCallback(MapTypeId mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "unRegisterCallback");
        getMapSurfaceView(mapTypeId).unRegisterCallback(callback);
    }

    public void clearAllMapSurfaceView() {
        mapSurfaceCache.clear();
    }

}
