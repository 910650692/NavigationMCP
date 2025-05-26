package com.fy.navi.hmi.splitscreen;

import android.graphics.Rect;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.ui.base.BaseModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/21
 * Description: [在这里描述文件功能]
 */
public class OneThirdScreenModel extends BaseModel<BaseOneThirdScreenViewModel> implements IMapPackageCallback, IGuidanceObserver {
    private static final String TAG = "OneThirdScreenModel";
    private MapPackage mMapPackage;
    private NaviPackage mNaviPackage;
    private PositionPackage mPositionPackage;
    private LayerPackage mLayerPackage;
    private RoutePackage mRoutePackage;
    private boolean isMapLoadSuccess = false;
    private final MapType MAP_TYPE = MapType.LAUNCHER_DESK_MAP;
    private final String CALLBACK_KEY = "OneThirdScreenModel";
    private final NavistatusAdapter mNaviStatusAdapter;
    private final ImmersiveStatusScene mImmersiveStatusScene;
    private CrossImageEntity mCrossImageEntity;
    public OneThirdScreenModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mPositionPackage = PositionPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mMapPackage.registerCallback(MAP_TYPE, this);
        mNaviPackage.registerObserver(CALLBACK_KEY, this);
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mImmersiveStatusScene = ImmersiveStatusScene.getInstance();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mNaviPackage.unregisterObserver(CALLBACK_KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
        isMapLoadSuccess = false;
        Logger.i(TAG, "onDestroy");
    }

    public void loadMapView() {
        Logger.i(TAG, "loadMapView:" + isMapLoadSuccess);
        mMapPackage.initMapView(mViewModel.getMapView());
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        IMapPackageCallback.super.onMapLoadSuccess(mapTypeId);
        Logger.i(TAG, "onMapLoadSuccess:" + mapTypeId.name(), "isMapLoadSuccess:" + isMapLoadSuccess);
        if (mapTypeId == MAP_TYPE && !isMapLoadSuccess) {
            isMapLoadSuccess = true;
            mMapPackage.setMapCenter(getMapType(), new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                    mPositionPackage.getLastCarLocation().getLatitude()));
            mMapPackage.goToCarPosition(getMapType());
            mMapPackage.setMapCenterInScreen(getMapType(), mViewModel.getLogoPosition()[0], mViewModel.getLogoPosition()[1]);
            mLayerPackage.setDefaultCarMode(getMapType());
        }
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.i(TAG, "onUpdateTMCLightBar naviTmcInfo = " + naviTmcInfo.toString());
        mNaviPackage.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo, false);
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        IGuidanceObserver.super.onNaviInfo(naviETAInfo);
        mViewModel.onNaviInfo(naviETAInfo);
    }

    @Override
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        IGuidanceObserver.super.onCrossImageInfo(isShowImage, naviImageInfo);
        mViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    @Override
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        IGuidanceObserver.super.onLaneInfo(isShowLane, laneInfoEntity);
        mViewModel.onLaneInfo(isShowLane, laneInfoEntity);
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        mViewModel.onNaviStop();
    }

    @Override
    public void onMapInitSuccess(MapType mapTypeId, boolean success) {
        IMapPackageCallback.super.onMapInitSuccess(mapTypeId, success);
    }

    private MapType getMapType() {
        return mViewModel.getMapView().provideMapTypeId();
    }

    /**
     * @return 是否正在导航中
     */
    public boolean isOnNavigating() {
        return ConvertUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
    }

    /**
     * @return 是否处于沉浸态
     */
    public boolean isOnImmersive() {
        return ImmersiveStatusScene.getInstance().getCurrentImersiveStatus(MAP_TYPE) == ImersiveStatus.IMERSIVE;
    }

    public void stopNavi() {
        mNaviPackage.stopNavigation();
    }

    public void showPreview() {
        OpenApiHelper.enterPreview(MAP_TYPE);
    }

    public void muteOrUnMute() {
        mNaviPackage.setMute(!mNaviPackage.isMute());
    }

    public Boolean isMute() {
        return mNaviPackage.isMute();
    }

    public void naviContinue() {
        mImmersiveStatusScene.setImmersiveStatus(MAP_TYPE, ImersiveStatus.IMERSIVE);
        OpenApiHelper.exitPreview(MAP_TYPE);
    }

    public void setRoadCrossRect(Rect rect) {
        mNaviPackage.setRoadCrossRect(MAP_TYPE, rect);
    }
}