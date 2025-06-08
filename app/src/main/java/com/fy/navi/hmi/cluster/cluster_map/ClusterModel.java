package com.fy.navi.hmi.cluster.cluster_map;

import android.content.Intent;
import android.text.TextUtils;

import androidx.core.app.ActivityCompat;

import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.cluster.ClusterViewModel;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.StartService;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

public class ClusterModel extends BaseModel<ClusterViewModel> implements IMapPackageCallback,
        IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, StartService.ISdkInitCallback {
    private static final String TAG = "ClusterModel";

    public ClusterModel() {
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
        Logger.d(TAG, "仪表底图创建完成");
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
        if (mMapTypeId == MapType.CLUSTER_MAP) {
            Logger.d(TAG, "仪表底图加载完成", mMapTypeId.name());
            MapPackage.getInstance().goToCarPosition(mMapTypeId);
            MapPackage.getInstance().switchMapMode(MapType.CLUSTER_MAP, MapMode.UP_3D, false);
            MapPackage.getInstance().setZoomLevel(mMapTypeId, 17);
            LayerPackage.getInstance().setCarPosition(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            LayerPackage.getInstance().setCarMode(mMapTypeId, CarModeType.CAR_MODE_DEFAULT);
            LayerPackage.getInstance().setFollowMode(mMapTypeId, true);
            boolean nightModeEnabled = ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext());
            MapAdapter.getInstance().updateUiStyle(MapType.CLUSTER_MAP, ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext()) ? ThemeType.NIGHT : ThemeType.DAY);
            //设置地图文字大小
            boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
            if (mapViewTextSize) {
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
            } else {
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
            }
        }
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
        RoutePackage.getInstance().showRouteLine(getMapId());
    }

    @Override
    public void onNaviStart() {
        Logger.d(TAG, "onNaviStart");
        mViewModel.updateNaviStatus(true);
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (mViewModel == null) return;
        if (!checkNaviInfoPanelLegal(naviETAInfo)) return;
        mViewModel.updateEta(naviETAInfo);
        if (!TextUtils.isEmpty(naviETAInfo.getCurRouteName())) {
            mViewModel.updateRouteName(naviETAInfo.getCurRouteName());
        }
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        Logger.d(TAG, "onNaviStatusChange:" + naviStatus);
        switch (naviStatus) {
            case NaviStatus.NaviStatusType.CRUISE -> //巡航状态
                //主图模式与巡航模式下，默认⽐例尺为50ｍ（TBD根据实车联调）；
                    MapPackage.getInstance().setZoomLevel(MapType.CLUSTER_MAP, 50);
            case NaviStatus.NaviStatusType.NAVING -> //导航状态
                //导航态下，仪表切换为地图模式后，中控地图导航模式切换为“路线全览模式。
                    NaviPackage.getInstance().onMeterAction();
            default -> {
//                LayerPackage.getInstance().openDynamicLevel(MapType.CLUSTER_MAP, false);
                //主图模式与巡航模式下，默认⽐例尺为50ｍ（TBD根据实车联调）；
                MapPackage.getInstance().setZoomLevel(MapType.CLUSTER_MAP, 50);
            }
        }
    }

    @Override
    public void onNaviStop() {
        Logger.d(TAG, "onNaviStop");
        RoutePackage.getInstance().clearRouteLine(getMapId());
        mViewModel.updateNaviStatus(false);
    }

    @Override
    public void onUiModeChanged(ThemeType uiMode) {
        Logger.d(TAG, "onMapLoadSuccess:nightModeEnabled: onUiModeChanged:" + uiMode);
        MapPackage.getInstance().updateUiStyle(MapType.CLUSTER_MAP, uiMode);
    }

    /**
     * 检查导航信息是否合法
     *
     * @param naviinfo 导航信息对象
     * @return boolean 是否合法
     */
    public static boolean checkNaviInfoPanelLegal(final NaviEtaInfo naviinfo) {
        if (naviinfo == null) {
            Logger.d(TAG, "Navi info is null", "ETA info invalid or illegal");
            return false;
        }
        if (naviinfo.NaviInfoData == null || naviinfo.NaviInfoData.isEmpty()) {
            Logger.d(TAG, "Navi info data is null or empty");
            return false;
        }
        if (naviinfo.NaviInfoFlag >= naviinfo.NaviInfoData.size()) {
            Logger.d(TAG, "Navi info flag out of bounds");
            return false;
        }
        if (naviinfo.NaviInfoData.get(naviinfo.NaviInfoFlag) == null) {
            Logger.d(TAG, "Navi info data at flag index is null");
            return false;
        }
        return true;
    }
}
