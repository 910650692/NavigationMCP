package com.sgm.navi.service.logicpaket.hud;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.IMapAdapterCallback;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.map.HUDMapView;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.HashMap;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/6/2
 */
public class HudPackage implements StartService.ISdkInitCallback, IMapAdapterCallback, IRouteResultObserver, IGuidanceObserver {
    private static final String TAG = MapDefaultFinalTag.HUD_SERVICE_TAG;
    private IBaseScreenMapView mMapSurfaceView = null;
    private HashMap<String, IHudCallback> hudCallbackMap = new HashMap<>();

    private static final class Helper {
        private static final HudPackage hudPackage = new HudPackage();
    }

    private HudPackage() {
        StartService.getInstance().registerSdkCallback(TAG, this);
        MapAdapter.getInstance().registerCallback(MapType.HUD_MAP, this);
        NaviPackage.getInstance().registerObserver(TAG, this);
        RoutePackage.getInstance().registerRouteObserver(TAG, this);
    }

    public void registerHudCallback(String key, IHudCallback hudCallback) {
        ConvertUtils.push(hudCallbackMap, key, hudCallback);
        Logger.d(TAG, "registerHudCallback hudCallbackMap： " + hudCallbackMap.size());
    }

    public void initHudService() {
        if (null != mMapSurfaceView) return;
        Logger.d(TAG, "init Hud Service");
        initHudService(null);
    }

    public void initHudService(IBaseScreenMapView mapSurfaceView) {
        Logger.d(TAG, "init Hud Service");
        if (null == mapSurfaceView) {
            mapSurfaceView = new HUDMapView(AppCache.getInstance().getMContext());
        }
        mMapSurfaceView = mapSurfaceView;
        boolean sdkStatus = StartService.getInstance().checkSdkIsNeedInit();
        Logger.i(TAG, "校验Sdk是否需要初始化sdkStatus：" + sdkStatus);
        if (sdkStatus) StartService.getInstance().startInitSdk();
        Logger.d(TAG, "HUDMapView地图创建中......");
    }

    public void unInitHudService() {
        if (ConvertUtils.isEmpty(mMapSurfaceView)) return;
        RoutePackage.getInstance().unRegisterRouteObserver(TAG);
        NaviPackage.getInstance().unregisterObserver(TAG);
        LayerPackage.getInstance().unInitLayer(MapType.HUD_MAP);
        MapAdapter.getInstance().unregisterCallback(MapType.HUD_MAP, this);
        MapAdapter.getInstance().unBindMapView(mMapSurfaceView);
        MapAdapter.getInstance().destroyMapView(MapType.HUD_MAP);
        Logger.d(TAG, "HUDMapView地图已销毁");
        ConvertUtils.clear(hudCallbackMap);
        mMapSurfaceView = null;
    }

    public static HudPackage getInstance() {
        return Helper.hudPackage;
    }


    @Override
    public void onSdkInitSuccess() {
        MapPackage.getInstance().createMapView(MapType.HUD_MAP);
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "引擎初始化失败重试重......");
    }


    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        if (mapTypeId == MapType.HUD_MAP){
            initLayer();
        }
    }

    private void initLayer() {
        MapPackage.getInstance().bindMapView(mMapSurfaceView);
        LayerAdapter.getInstance().initLayer(MapType.HUD_MAP);
        LayerAdapter.getInstance().setCarPosition(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                PositionPackage.getInstance().getLastCarLocation().getCourse()));
        MapPackage.getInstance().goToCarPosition(MapType.HUD_MAP);
        MapPackage.getInstance().setMapCenter(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
        LayerAdapter.getInstance().setCarMode(MapType.HUD_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        LayerAdapter.getInstance().setFollowMode(MapType.HUD_MAP, true);
        MapPackage.getInstance().switchMapMode(MapType.HUD_MAP, MapMode.UP_2D, false);
        MapPackage.getInstance().setZoomLevel(MapType.HUD_MAP, 15);
        LayerAdapter.getInstance().setDynamicLevelLock(MapType.HUD_MAP, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
    }

    @Override
    public void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
        Logger.d(TAG, "HUDMap截图数据长度：" + bytes.length, "hudCallbackMap size : " + hudCallbackMap.size());
        for (IHudCallback hudCallback : hudCallbackMap.values()) {
            hudCallback.onEGLScreenshot(mapTypeId, bytes);
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
//        RoutePackage.getInstance().showRouteLine(MapType.HUD_MAP);
    }

    @Override
    public void onNaviStop() {
        //TODO
        RoutePackage.getInstance().clearRouteLine(MapType.HUD_MAP);
    }
}
