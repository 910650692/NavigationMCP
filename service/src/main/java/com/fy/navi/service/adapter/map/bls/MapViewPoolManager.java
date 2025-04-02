package com.fy.navi.service.adapter.map.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.define.map.MapViewParams;
import com.fy.navi.service.define.map.MapType;

import java.util.Hashtable;

/**
 * 创建MapSurfaceView工厂
 */
public class MapViewPoolManager {

    private static final String TAG = MapDefaultFinalTag.MAP_SERVICE_TAG;

    private Hashtable<MapType, MapViewImpl> mapViewPools = new Hashtable<>();

    public static MapViewPoolManager getInstance() {
        return MapViewPoolManager.Holder.INSTANCE;
    }

    private static final class Holder {
        private static final MapViewPoolManager INSTANCE = new MapViewPoolManager();
    }

    private MapViewPoolManager() {

    }

    public boolean init(MapType mapTypeId) {
        Logger.d(TAG, "MapSurfaceViewManager init :" + mapTypeId.toString());
        if (!mapViewPools.containsKey(mapTypeId)) {
            createMapViewImpl(mapTypeId, new MapViewParams());
        }
        return true;
    }

    private void createMapViewImpl(MapType mapTypeId, MapViewParams mapViewParams) {
        MapViewImpl mapViewImpl = new MapViewImpl(AppContext.getInstance().getMContext());
        mapViewImpl.initMapView(mapTypeId, mapViewParams);
        mapViewPools.put(mapTypeId, mapViewImpl);
    }

    public MapViewImpl get(MapType mapTypeId, MapViewParams mapViewParams) {
        if (!mapViewPools.containsKey(mapTypeId)) {
            createMapViewImpl(mapTypeId, mapViewParams);
        }
        return ConvertUtils.isNullRequire(mapViewPools.get(mapTypeId), "获取对应的MapSurfaceViewImp失败 : " + mapTypeId.toString());
    }

    public MapViewImpl get(MapType mapTypeId) {
        return get(mapTypeId, new MapViewParams());
    }

    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "registerCallback");
        get(mapTypeId).registerCallback(callback);
    }

    public void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "unRegisterCallback");
        get(mapTypeId).unRegisterCallback(callback);
    }
}
