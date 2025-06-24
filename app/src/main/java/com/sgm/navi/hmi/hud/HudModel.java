package com.sgm.navi.hmi.hud;
import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;

import com.sgm.navi.NaviService;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navistatus.INaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseModel;

public class HudModel extends BaseModel<BaseHudViewModel> implements IMapPackageCallback,
IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, SettingPackage.SettingChangeCallback
, StartService.ISdkInitCallback {
    private static final String TAG = "HudModel";
    public HudModel() {
        StartService.getInstance().registerSdkCallback(TAG, this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "start navi Service");
        Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        MapPackage.getInstance().unRegisterCallback(getMapId(), this);
        RoutePackage.getInstance().unRegisterRouteObserver(mViewModel.mScreenId);
        CruisePackage.getInstance().unregisterObserver(mViewModel.mScreenId);
        NavistatusAdapter.getInstance().unRegisterCallback(this);
        StartService.getInstance().unregisterSdkCallback(this);
        NaviPackage.getInstance().unregisterObserver(mViewModel.mScreenId);
    }

    private MapType getMapId() {
        return MapType.valueOf(mViewModel.mScreenId);
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.d(TAG, "Sdk init success");
        MapPackage.getInstance().createMapView(getMapId());
        Logger.d(TAG, "HUD底图创建完成");
        MapPackage.getInstance().registerCallback(getMapId(), this);
        RoutePackage.getInstance().registerRouteObserver(mViewModel.mScreenId, this);
        NaviPackage.getInstance().registerObserver(mViewModel.mScreenId, this);
        CruisePackage.getInstance().registerObserver(mViewModel.mScreenId, this);
        NavistatusAdapter.getInstance().registerCallback(this);
        mViewModel.loadMapView();
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "Sdk init fail");
    }


    @Override
    public void onMapLoadSuccess(final MapType mMapTypeId) {
        if (mMapTypeId == MapType.HUD_MAP) {
            Logger.d(TAG, "HUD底图加载完成", mMapTypeId.name());
            //设置地图中心点
            MapPackage.getInstance().setMapCenter(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude()));
            //回车位保存中心
            LayerPackage.getInstance().setCarPosition(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            MapPackage.getInstance().goToCarPosition(mMapTypeId);
            // 根据主屏的车标模式设置车标模式     mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP)获取主图的车标样式
            LayerPackage.getInstance().setCarMode(mMapTypeId, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));

            MapPackage.getInstance().switchMapMode(MapType.HUD_MAP, MapMode.UP_2D,false);
            MapPackage.getInstance().setZoomLevel(mMapTypeId, 17);
            LayerPackage.getInstance().setFollowMode(mMapTypeId, true);
            MapAdapter.getInstance().updateUiStyle(MapType.HUD_MAP, ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext()) ? ThemeType.NIGHT : ThemeType.DAY);
            LayerAdapter.getInstance().setSkeletonBaseScale(MapType.HUD_MAP,100);
            //设置走过的路线是否为灰色
            //LayerPackage.getInstance().setPassGray(getMapId(), true);
            //设置地图文字大小
            boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
            if (mapViewTextSize) {
                MapPackage.getInstance().setMapViewTextSize(MapType.HUD_MAP, 1f);
            } else {
                MapPackage.getInstance().setMapViewTextSize(MapType.HUD_MAP, 1.8f);
            }

            if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)
                    || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.LIGHT_NAVING)){
                Logger.d(TAG, "导航中显示导航路线 HUDActivity");
                RoutePackage.getInstance().showRouteLine(getMapId());
            }
        }
    }


    @Override
    public void onNaviStart() {
        Logger.d(TAG, "onNaviStart");
        RoutePackage.getInstance().showRouteLine(getMapId());
    }

    @Override
    public void onNaviStop() {
        Logger.d(TAG, "onNaviStop");
        RoutePackage.getInstance().clearRouteLine(getMapId());
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        Logger.d(TAG, "onNaviStatusChange:" , naviStatus);
    }

    /**
     * 设置监听回调
     * @param key   设置项的key值
     * @param value 设置项对应的value值
     */
    @Override
    public void onSettingChanged(String key, String value) {

    }
}
