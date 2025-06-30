package com.sgm.navi.hmi.cluster.cluster_map;

import android.content.Intent;
import android.text.TextUtils;

import androidx.core.app.ActivityCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.NaviService;
import com.sgm.navi.hmi.cluster.ClusterViewModel;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navistatus.INaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseModel;

import com.sgm.navi.hmi.BuildConfig;

import java.util.ArrayList;

public class ClusterModel extends BaseModel<ClusterViewModel> implements IMapPackageCallback,
        IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, StartService.ISdkInitCallback ,SettingPackage.SettingChangeCallback{
    private static final String TAG = "ClusterModel";
    private static float MAP_ZOOM_LEVEL_DEFAULT = 17F;
    private boolean isInItMapView= false;

    public ClusterModel() {

    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "onCreate");
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
        if (!isInItMapView){
            initClusterMapAndObservers("ClusterModel onSdkInitSuccess");
        }
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "Sdk init fail");
    }

    @Override
    public void onMapLoadSuccess(final MapType mMapTypeId) {
        if (mMapTypeId == MapType.CLUSTER_MAP) {
            Logger.d(TAG, "仪表底图加载完成", mMapTypeId.name());
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
            Logger.d(TAG, "车标样式仪表获取主图的",LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            LayerPackage.getInstance().initCarLogoByFlavor(mMapTypeId,  BuildConfig.FLAVOR);
            LayerPackage.getInstance().setFollowMode(mMapTypeId, true);
            MapPackage.getInstance().switchMapMode(MapType.CLUSTER_MAP, MapMode.UP_3D,false);
            MapPackage.getInstance().setZoomLevel(mMapTypeId, MAP_ZOOM_LEVEL_DEFAULT);
            MapAdapter.getInstance().updateUiStyle(MapType.CLUSTER_MAP, ThemeUtils.INSTANCE.isNightModeEnabled(AppCache.getInstance().getMContext()) ? ThemeType.NIGHT : ThemeType.DAY);
            //设置走过的路线是否为灰色
            //LayerPackage.getInstance().setPassGray(getMapId(), true);
            //设置地图文字大小
            boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
            if (mapViewTextSize) {
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
            } else {
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
            }

            if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)
                    || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.LIGHT_NAVING)){
                Logger.d(TAG, "导航中显示导航路线");
                RoutePackage.getInstance().showRouteLine(getMapId());
            }
        }
    }



    @Override
    public void onNaviStart() {
        Logger.d(TAG, "onNaviStart");
        mViewModel.updateNaviStatus(true);
        RoutePackage.getInstance().showRouteLine(getMapId());
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
            //case NaviStatus.NaviStatusType.NAVING -> //导航状态
                //导航态下，仪表切换为地图模式后，中控地图导航模式切换为“路线全览模式。
                //NaviPackage.getInstance().onMeterAction();
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

    /**
     * 设置监听回调
     * @param key   设置项的key值
     * @param value 设置项对应的value值
     */
    @Override
    public void onSettingChanged(String key, String value) {
        Logger.d(TAG, "onSettingChanged:" + key + "-:-" + value);
        //设置地图大小
        boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
        if (mapViewTextSize){
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
        }else {
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
        }
        //设置车标模式
        LayerPackage.getInstance().setCarMode(MapType.CLUSTER_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        Logger.d(TAG, "车标样式仪表获取主图的onSettingChanged",LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
        LayerPackage.getInstance().initCarLogoByFlavor(MapType.CLUSTER_MAP,  BuildConfig.FLAVOR);
    }



    /**
     * 初始化 Cluster 地图及相关观察者回调
     */
    public void initClusterMapAndObservers(String from) {
        Logger.d(TAG, "sdk 成功",from);
        boolean mapViewInitResult = MapPackage.getInstance().createMapView(MapType.CLUSTER_MAP);
        Logger.d(TAG, "mapViewInitResult: ==" , mapViewInitResult);
        if (!mapViewInitResult) return;
        mViewModel.loadMapView();
        isInItMapView = true;
        MapPackage.getInstance().registerCallback(getMapId(), this);
        RoutePackage.getInstance().registerRouteObserver(mViewModel.mScreenId, this);
        NaviPackage.getInstance().registerObserver(mViewModel.mScreenId, this);
        CruisePackage.getInstance().registerObserver(mViewModel.mScreenId, this);
        NavistatusAdapter.getInstance().registerCallback(this);
        // 监听设置包变化
        SettingPackage.getInstance().setSettingChangeCallback(getMapId().name(), this);
    }

    public void registerClusterMap() {
        Logger.d(TAG, "registerClusterMap");
        Logger.d(TAG, "StartService.getInstance().getSdkActivation()==",StartService.getInstance().getSdkActivation());
        if (StartService.getInstance().getSdkActivation() == 0){
            initClusterMapAndObservers("registerClusterMap");
        }else {
            Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
            StartService.getInstance().registerSdkCallback(TAG, this);
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        //偏航以后仪表没有重新算路
        String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Logger.d(TAG, "onRouteDrawLine", "currentNaviStatus = ", currentNaviStatus);
        if (!NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus)
                && !NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus)) {
            return;
        }
        RoutePackage.getInstance().showRouteLine(getMapId());
    }
}
