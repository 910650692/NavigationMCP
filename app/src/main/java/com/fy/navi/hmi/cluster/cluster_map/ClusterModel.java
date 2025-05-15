package com.fy.navi.hmi.cluster.cluster_map;

import android.graphics.Rect;
import android.view.MotionEvent;

import com.android.utils.log.Logger;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.map.MapAdapter;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.layer.refix.CarModeType;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapTypeManager;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.setting.SettingManager;
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

import java.util.ArrayList;

/**
 * @Description
 * @Author yaWei
 * @date 2025/2/18
 */
public class ClusterModel extends BaseModel<BaseClusterViewModel> implements IMapPackageCallback,
IRouteResultObserver, INaviStatusCallback, ISceneCallback, IGuidanceObserver, ICruiseObserver, SettingPackage.SettingChangeCallback {
    private static final String TAG = "ClusterModel";
    private boolean mLoadMapSuccess = true;  //只加载一次
    public ClusterModel() {

    }
    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "onDestroy");
        if (MapPackage.getInstance() != null) {
            MapPackage.getInstance().unRegisterCallback(MapTypeManager.getInstance().getMapTypeIdByName(mViewModel.mScreenId), this);
            MapPackage.getInstance().unBindMapView(mViewModel.getMapView());
            RoutePackage.getInstance().unRegisterRouteObserver(getMapId().name());
            CruisePackage.getInstance().unregisterObserver(getMapId().name());
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
    public void onNaviStatusChange(final String naviStatus) {
        IMapPackageCallback.super.onNaviStatusChange(naviStatus);
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
