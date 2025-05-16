package com.fy.navi.hmi.cluster.cluster_map;

import android.text.TextUtils;

import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
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
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.ui.base.BaseModel;

public class ClusterModel extends BaseModel<BaseClusterViewModel> implements IMapPackageCallback,
IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, SettingPackage.SettingChangeCallback {
    private static final String TAG = "ClusterModel";
    private boolean mLoadMapSuccess = true;  //只加载一次
    public ClusterModel() {}
    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        if (MapPackage.getInstance() != null) {
            MapPackage.getInstance().unRegisterCallback(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
            MapPackage.getInstance().unBindMapView(mViewModel.getMapView());
            RoutePackage.getInstance().unRegisterRouteObserver(getMapId().name());
            CruisePackage.getInstance().unregisterObserver(getMapId().name());
            NavistatusAdapter.getInstance().unRegisterCallback(this);
        }
    }
    /***
     * 把高德底图绑定到自己的视图上
     */
    public void loadMapView() {
        Logger.d(TAG, "loadMapView");
        MapPackage.getInstance().registerCallback(getMapId(), this);
        RoutePackage.getInstance().registerRouteObserver(getMapId().name(), this);
        NaviPackage.getInstance().registerObserver(getMapId().name(), this);
        CruisePackage.getInstance().registerObserver(getMapId().name(), this);
        NavistatusAdapter.getInstance().registerCallback(this);
        //设置走过的路线是否为灰色
        LayerPackage.getInstance().setPassGray(getMapId(), true);
        MapPackage.getInstance().initMapView(mViewModel.getMapView());
    }
    @Override
    public void onMapLoadSuccess(final MapType mMapTypeId) {
        Logger.d(TAG, "onMapLoadSuccess:" + mMapTypeId.name(), "mLoadMapSuccess:"+mLoadMapSuccess);
        if (mMapTypeId == MapType.CLUSTER_MAP && mLoadMapSuccess) {
            Logger.d(TAG, "onMapLoadSuccess:");
            mLoadMapSuccess = false;
            //设置地图中心点
//            MapPackage.getInstance().setMapCenter(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
//                    PositionPackage.getInstance().getLastCarLocation().getLatitude()));
            //回车位保存中心
            LayerPackage.getInstance().setCarPosition(mMapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            MapPackage.getInstance().goToCarPosition(mMapTypeId);
            // 根据主屏的车标模式设置车标模式     mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP)获取主图的车标样式
            LayerPackage.getInstance().setCarMode(mMapTypeId, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            Logger.d(TAG, "onMapLoadSuccess:LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP):"+LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            LayerPackage.getInstance().setFollowMode(mMapTypeId, true);
            //设置地图模式 为3D模式  并不保存地图模式(不影响主图地图模式)
            MapPackage.getInstance().switchMapMode(MapType.CLUSTER_MAP, MapMode.UP_3D);
            //仪表地图永为3D模式  比例尺保持17的样子
            MapPackage.getInstance().setZoomLevel(mMapTypeId, 17);
            //监听设置包变化
            SettingPackage.getInstance().setSettingChangeCallback(mMapTypeId.name(), this);
            //设置地图文字大小
            boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
            if (mapViewTextSize){
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
            }else {
                MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
            }
        }
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        IGuidanceObserver.super.onNaviInfo(naviETAInfo);
        if (mViewModel != null){
            mViewModel.updateEta(naviETAInfo);
            if (!TextUtils.isEmpty(naviETAInfo.getCurRouteName())) {
                mViewModel.updateRouteName(naviETAInfo.getCurRouteName());
            }
        }
    }

    @Override
    public void onNaviStatusChange(final String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
        Logger.d(TAG, "onNaviStatusChange:" + naviStatus);
        if (naviStatus.equals(NaviStatus.NaviStatusType.CRUISE)){//巡航状态
            //主图模式与巡航模式下，默认⽐例尺为50ｍ（TBD根据实车联调）；
            LayerPackage.getInstance().openDynamicLevel(MapType.CLUSTER_MAP, false);
            MapPackage.getInstance().setZoomLevel(MapType.CLUSTER_MAP, 50);
        }else if (naviStatus.equals(NaviStatus.NaviStatusType.NAVING)){//导航状态
            if (Boolean.TRUE.equals(MyFsaService.getIsClusterMapOpen().getValue())){
                //导航态下，仪表切换为地图模式后，中控地图导航模式切换为“路线全览模式。
                OpenApiHelper.enterPreview(MapType.MAIN_SCREEN_MAIN_MAP);
                //toast提示（MsgType: 3s Timeout+AnyKey):
                ToastUtils.Companion.getInstance().showCustomToastView("仪表导航已开启，地图默认显示全程路线",  3000);
            }
            LayerPackage.getInstance().openDynamicLevel(MapType.CLUSTER_MAP, true);
        }else if (naviStatus.equals(NaviStatus.NaviStatusType.NO_STATUS)){//不知
            LayerPackage.getInstance().openDynamicLevel(MapType.CLUSTER_MAP, false);
            //主图模式与巡航模式下，默认⽐例尺为50ｍ（TBD根据实车联调）；
            MapPackage.getInstance().setZoomLevel(MapType.CLUSTER_MAP, 50);
        }else{
            LayerPackage.getInstance().openDynamicLevel(MapType.CLUSTER_MAP, false);
            //主图模式与巡航模式下，默认⽐例尺为50ｍ（TBD根据实车联调）；
            MapPackage.getInstance().setZoomLevel(MapType.CLUSTER_MAP, 50);
        }
        mViewModel.updateNaviStatus(naviStatus);
    }

    @Override
    public void onRouteDrawLine(final RouteLineLayerParam routeLineLayerParam) {
        Logger.i(TAG, "onRouteDrawLine:" + routeLineLayerParam.getMMapTypeId());
        RoutePackage.getInstance().showRouteLine(getMapId());
    }

    private MapType getMapId() {
        return mViewModel.getMapView().provideMapTypeId();
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        Logger.d(TAG, "onNaviStop");
        RoutePackage.getInstance().clearRouteLine(getMapId());
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
        //设置地图大小
        boolean mapViewTextSize = SettingPackage.getInstance().getMapViewTextSize();
        if (mapViewTextSize){
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1f);
        }else {
            MapPackage.getInstance().setMapViewTextSize(MapType.CLUSTER_MAP, 1.8f);
        }
        //设置车标模式
        LayerPackage.getInstance().setCarMode(MapType.CLUSTER_MAP, LayerPackage.getInstance().getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
    }

    //地图日夜切换
    @Override
    public void onUiModeChanged(ThemeType uiMode) {
        IMapPackageCallback.super.onUiModeChanged(uiMode);
        MapAdapter.getInstance().updateUiStyle(MapType.CLUSTER_MAP, uiMode);
    }
}
