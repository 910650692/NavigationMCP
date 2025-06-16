package com.fy.navi.service.adapter.map.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
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

    public boolean createMapView(MapType mapTypeId) {
        Logger.d(TAG, "MapSurfaceViewManager init :" , mapTypeId.toString());
        if (!mapViewPools.containsKey(mapTypeId)) {
            createMapViewImpl(mapTypeId, new MapViewParams());
        }
        return true;
    }

    private void createMapViewImpl(MapType mapTypeId, MapViewParams mapViewParams) {
        Logger.d(TAG, "MapSurfaceViewManager create :" , mapTypeId.toString());
        MapViewImpl mapViewImpl = new MapViewImpl(AppCache.getInstance().getMContext());
        mapViewImpl.initMapView(mapTypeId, mapViewParams);
        mapViewPools.put(mapTypeId, mapViewImpl);
    }

    public MapViewImpl get(MapType mapTypeId, MapViewParams mapViewParams) {
        Logger.d(TAG, "MapSurfaceViewManager get :" , mapTypeId.toString());
        if (!mapViewPools.containsKey(mapTypeId)) {
            createMapViewImpl(mapTypeId, mapViewParams);
        }
        return ConvertUtils.isNullRequire(mapViewPools.get(mapTypeId), "获取对应的MapSurfaceViewImp失败 : " + mapTypeId.toString());
    }

    public MapViewImpl get(MapType mapTypeId) {
        return get(mapTypeId, new MapViewParams());
    }

    public boolean isMapViewExist(MapType mapTypeId) {
        return mapViewPools.containsKey(mapTypeId);
    }

    public void registerCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "registerCallback");
        get(mapTypeId).registerCallback(callback);
    }

    public void unRegisterCallback(MapType mapTypeId, IMapAdapterCallback callback) {
        Logger.d(TAG, "unRegisterCallback");
        get(mapTypeId).unRegisterCallback(callback);
    }

    public void destroyMapView(MapType mapTypeId) {
        if (ConvertUtils.isEmpty(mapViewPools)) return;
        MapViewImpl mapView = ConvertUtils.pop(mapViewPools, mapTypeId);
        if (null != mapView) mapView.destroyMapView();
    }

    public void removeAllCallback() {
        if (ConvertUtils.isEmpty(mapViewPools)) return;
        for (MapViewImpl mapView : mapViewPools.values()) {
            mapView.destroyMapView();
        }
        ConvertUtils.clear(mapViewPools);
        mapViewPools = null;
    }
}
