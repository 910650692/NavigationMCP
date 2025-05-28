package com.fy.navi.hmi.hud;
import com.android.utils.log.Logger;
import com.fy.navi.service.StartService;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IEglScreenshotCallBack;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;


public class HudMapManager implements IMapPackageCallback,
        IRouteResultObserver, INaviStatusCallback, ICruiseObserver, SettingPackage.SettingChangeCallback,IGuidanceObserver, IPositionPackageCallback , StartService.ISdkInitCallback {
    private static final String TAG = "HudMapManager";
    private boolean mLoadMapSuccess = true;
    private static final HudMapManager instance = new HudMapManager();
    private HudMapManager() {
        Logger.i("HudMapManager", "HUD地图加载中...");
        StartService.getInstance().registerSdkCallback(this);
    }

    public static HudMapManager getInstance() {
        return instance;
    }

    public void init(){
        MapPackage.getInstance().initCallback(MapType.HUD_MAP);
        MapPackage.getInstance().registerEGLScreenshotCallBack(TAG, mIEglScreenshotCallBack);
        MapPackage.getInstance().registerCallback(MapType.HUD_MAP, this);
        //MapPackage.getInstance().initMapView(hudMapView);
        Logger.d("HudMapManager", "HUD地图已加载");
        RoutePackage.getInstance().registerRouteObserver(TAG, this);
        NaviPackage.getInstance().registerObserver(TAG, this);
        CruisePackage.getInstance().registerObserver(TAG, this);
        PositionPackage.getInstance().registerCallBack(this);
        //设置走过的路线是否为灰色
        LayerPackage.getInstance().setPassGray(MapType.HUD_MAP, true);

        LayerPackage.getInstance().setCarPosition(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                PositionPackage.getInstance().getLastCarLocation().getCourse()));
        MapPackage.getInstance().goToCarPosition(MapType.HUD_MAP);
        MapPackage.getInstance().setHudMapCenterInScreen(MapType.HUD_MAP, 328/2,172/2);
        MapPackage.getInstance().setMapCenter(MapType.HUD_MAP, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                PositionPackage.getInstance().getLastCarLocation().getLatitude()));
        LayerPackage.getInstance().setCarMode(MapType.HUD_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        LayerPackage.getInstance().setFollowMode(MapType.HUD_MAP, true);
        LayerPackage.getInstance().setDynamicLevelLock(MapType.HUD_MAP, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
        MapPackage.getInstance().switchMapMode(MapType.HUD_MAP, MapMode.UP_2D);
        MapPackage.getInstance().setZoomLevel(MapType.HUD_MAP, 16);
        SettingPackage.getInstance().setSettingChangeCallback(MapType.HUD_MAP.name(), this);
    }

    private final IEglScreenshotCallBack mIEglScreenshotCallBack = new IEglScreenshotCallBack() {
        @Override
        public void onEGLScreenshot(MapType mapType, byte[] bytes) {
        }
    };

    @Override
    public void onSdkInitSuccess() {
        StartService.ISdkInitCallback.super.onSdkInitSuccess();
    }

    public void destroyMap() {
        Logger.d("HudMapManager", "HUD地图已销毁");
        if (MapPackage.getInstance() != null) {
            MapPackage.getInstance().unRegisterCallback(MapType.HUD_MAP, this);
            RoutePackage.getInstance().unRegisterRouteObserver(TAG);
            CruisePackage.getInstance().unregisterObserver(TAG);
        }
    }

    @Override
    public void onLocationInfo(LocInfoBean locationInfo) {
        Logger.d(TAG, "onLocationInfo: " + locationInfo.toString());
        IPositionPackageCallback.super.onLocationInfo(locationInfo);
    }
    @Override
    public void onMapLoadSuccess(final MapType mMapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess:" + mMapTypeId.name(), "mLoadMapSuccess:" + mLoadMapSuccess);
        if (mMapTypeId == MapType.HUD_MAP && mLoadMapSuccess) {
            Logger.d(TAG, "onMapLoadSuccess:");
            mLoadMapSuccess = false;
            //回车位保存中心
            LayerPackage.getInstance().setCarPosition(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            MapPackage.getInstance().goToCarPosition(mMapTypeId);
            MapPackage.getInstance().setHudMapCenterInScreen(MapType.HUD_MAP, 328/2,172/2);
            MapPackage.getInstance().setMapCenter(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude()));
            LayerPackage.getInstance().setCarMode(mMapTypeId, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            LayerPackage.getInstance().setFollowMode(mMapTypeId, true);
            LayerPackage.getInstance().setDynamicLevelLock(mMapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
            MapPackage.getInstance().switchMapMode(MapType.HUD_MAP, MapMode.UP_2D);
            MapPackage.getInstance().setZoomLevel(mMapTypeId, 16);
            SettingPackage.getInstance().setSettingChangeCallback(mMapTypeId.name(), this);
        }

    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
        RoutePackage.getInstance().showRouteLine(MapType.HUD_MAP);
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        Logger.d(TAG, "onNaviStop");
        RoutePackage.getInstance().clearRouteLine(MapType.HUD_MAP);
    }

    /**
     * 设置监听回调
     * @param key   设置项的key值
     * @param value 设置项对应的value值
     */
    @Override
    public void onSettingChanged(String key, String value) {
        SettingPackage.SettingChangeCallback.super.onSettingChanged(key, value);
        Logger.d(TAG, "onSettingChanged:" + key + "-:-" + value);
        //设置车标模式
        LayerPackage.getInstance().setCarMode(MapType.HUD_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
    }

    //地图日夜切换
    @Override
    public void onUiModeChanged(ThemeType uiMode) {
        IMapPackageCallback.super.onUiModeChanged(uiMode);
        MapAdapter.getInstance().updateUiStyle(MapType.HUD_MAP, uiMode);
    }

    @Override
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        IGuidanceObserver.super.onCrossImageInfo(isShowImage, naviImageInfo);
        Logger.d(TAG, "onCrossImageInfo isShowImage:" + isShowImage + " naviImageInfo:" + naviImageInfo);
    }
}


