package com.fy.navi.service.logicpaket.hud;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.IMapAdapterCallback;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.map.HUDMapView;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.Hashtable;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/6/2
 */
public class HudPackage {
    private static final String TAG = MapDefaultFinalTag.HUD_SERVICE_TAG;
    private static IBaseScreenMapView mMapSurfaceView = null;
    private static Hashtable<String, IHudCallback> hudCallbackMap = null;

    private HudPackage() {
        hudCallbackMap = new Hashtable<>();
        StartService.getInstance().registerSdkCallback(TAG, sdkInitCallback);
    }

    public void initHudService() {
        if (null != mMapSurfaceView) return;
        Logger.d(TAG, "init Hud Service");
        initHudService(null);
    }

    public void registerHudCallback(String key, IHudCallback hudCallback) {
        ConvertUtils.push(hudCallbackMap, key, hudCallback);
        Logger.d(TAG, "registerHudCallback hudCallbackMap： " + hudCallbackMap.size());
    }

    public void initHudService(IBaseScreenMapView mapSurfaceView) {
        Logger.d(TAG, "init Hud Service");
        if (null == mapSurfaceView)
            mapSurfaceView = new HUDMapView(AppCache.getInstance().getMContext());
        mMapSurfaceView = mapSurfaceView;
        boolean sdkStatus = StartService.getInstance().checkSdkIsNeedInit();
        Logger.i(TAG, "校验Sdk是否需要初始化sdkStatus：" + sdkStatus);
        if (sdkStatus) StartService.getInstance().startInitSdk();
        Logger.d(TAG, "HUDMapView地图创建中......");
    }

    private static void initMapView(){
        Logger.d(TAG, "引擎初始化完成 创建HudMapView地图中......");
        MapAdapter.getInstance().bindMapView(mMapSurfaceView);
        Logger.d(TAG, "HUDMap地图创建完成");
        MapAdapter.getInstance().registerCallback(MapType.HUD_MAP, mapPackageCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, routeResultObserver);
        NaviPackage.getInstance().registerObserver(TAG, guidanceObserver);
    }

    private static void initLayer(){
        LayerAdapter.getInstance().initLayerService(MapType.HUD_MAP);
//        LayerAdapter.getInstance().setPassGray(MapType.HUD_MAP, true);
        LayerPackage.getInstance().setCarPosition(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                PositionPackage.getInstance().getLastCarLocation().getCourse()));
        MapPackage.getInstance().goToCarPosition(MapType.HUD_MAP);
        MapPackage.getInstance().setMapCenter(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
        LayerPackage.getInstance().setCarMode(MapType.HUD_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        LayerPackage.getInstance().setFollowMode(MapType.HUD_MAP, true);
        MapPackage.getInstance().switchMapMode(MapType.HUD_MAP, MapMode.UP_2D, false);
        MapPackage.getInstance().setZoomLevel(MapType.HUD_MAP, 15);
        LayerPackage.getInstance().setDynamicLevelLock(MapType.HUD_MAP, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
    }

    public void unInitHudService() {
        if (ConvertUtils.isEmpty(mMapSurfaceView)) return;
        LayerPackage.getInstance().removeLayerService(MapType.HUD_MAP);
        MapAdapter.getInstance().unregisterCallback(MapType.HUD_MAP, mapPackageCallback);
        MapAdapter.getInstance().unBindMapView(mMapSurfaceView);
        RoutePackage.getInstance().unRegisterRouteObserver(TAG);
        NaviPackage.getInstance().unregisterObserver(TAG);
        MapAdapter.getInstance().destroyMapView(MapType.HUD_MAP);
        Logger.d(TAG, "HUDMapView地图已销毁");
        ConvertUtils.clear(hudCallbackMap);
        mMapSurfaceView = null;
    }

    public static HudPackage getInstance() {
        return Helper.hudPackage;
    }

    private static final class Helper {
        private static final HudPackage hudPackage = new HudPackage();
    }

    private static final StartService.ISdkInitCallback sdkInitCallback = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            if(ConvertUtils.isEmpty(mMapSurfaceView)) return;
            initMapView();
            initLayer();
        }

        @Override
        public void onSdkInitFail(int initSdkResult, String msg) {
            Logger.d(TAG, "引擎初始化失败重试重......");
        }
    };

    private static final IMapAdapterCallback mapPackageCallback = new IMapAdapterCallback() {
        @Override
        public void onMapLoadSuccess(MapType mapTypeId) {
            Logger.d(TAG, "HUDMap地图加载完成");
        }

        @Override
        public void onEGLScreenshot(MapType mapTypeId, byte[] bytes) {
            Logger.d(TAG, "HUDMap截图数据长度：" + bytes.length, "hudCallbackMap size : " + hudCallbackMap.size());
            for (IHudCallback hudCallback : hudCallbackMap.values()) {
                hudCallback.onEGLScreenshot(mapTypeId, bytes);
            }
        }
    };

    private static final IRouteResultObserver routeResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
            Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
            RoutePackage.getInstance().showRouteLine(MapType.HUD_MAP);
        }
    };

    private static final IGuidanceObserver guidanceObserver = new IGuidanceObserver() {

        @Override
        public void onNaviStart() {
            Logger.d(TAG, "onNaviStart");
        }

        @Override
        public void onNaviStop() {
            Logger.d(TAG, "onNaviStop");
            if(ConvertUtils.isEmpty(mMapSurfaceView)) return;
            RoutePackage.getInstance().clearRouteLine(MapType.HUD_MAP);
        }
    };
}
