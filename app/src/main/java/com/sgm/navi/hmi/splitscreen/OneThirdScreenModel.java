package com.sgm.navi.hmi.splitscreen;

import android.graphics.Rect;
import android.graphics.drawable.BitmapDrawable;
import android.view.MotionEvent;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.map.MapType;
import com.android.utils.theme.ThemeType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/21
 * Description: [在这里描述文件功能]
 */
public class OneThirdScreenModel extends BaseModel<BaseOneThirdScreenViewModel> implements IMapPackageCallback, IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback {
    private static final String TAG = "OneThirdScreenModel";
    private MapPackage mMapPackage;
    private MapAdapter mapAdapter;
    private NaviPackage mNaviPackage;
    private PositionPackage mPositionPackage;
    private LayerPackage mLayerPackage;
    private RoutePackage mRoutePackage;
    private CalibrationPackage mCalibrationPackage;
    private SettingPackage mSettingPackage;
    private boolean isMapLoadSuccess = false;
    private final MapType MAP_TYPE = MapType.LAUNCHER_DESK_MAP;
    private final String CALLBACK_KEY = "OneThirdScreenModel";
    private final NavistatusAdapter mNaviStatusAdapter;
    private final ImmersiveStatusScene mImmersiveStatusScene;
    private boolean mPreviewIsOnShowing = false; // 全览状态，true代表正在全览
    private CrossImageEntity lastCrossImgEntity;

    private boolean mCrossImgOnShowing = false;
    private NextManeuverEntity mNextManeuverEntity;

    public OneThirdScreenModel() {
        mapAdapter = MapAdapter.getInstance();
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mPositionPackage = PositionPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mNaviPackage.registerObserver(CALLBACK_KEY, this);
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mImmersiveStatusScene = ImmersiveStatusScene.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mImmersiveStatusScene.registerCallback(CALLBACK_KEY, this);
        mSettingPackage = SettingPackage.getInstance();
        clearCacheInfo();
        mNextManeuverEntity = new NextManeuverEntity();
        lastCrossImgEntity = mNaviPackage.getLastCrossEntity();
        mCrossImgOnShowing = mNaviPackage.isMCrossImgIsOnShowing();
    }

    private void clearCacheInfo() {
        if (!isOnNavigating()) {
            mRoutePackage.removeAllRouteInfo(MAP_TYPE);
            mLayerPackage.setVisibleGuideSignalLight(MAP_TYPE, false);
            mRoutePackage.clearRouteLine(MAP_TYPE);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mImmersiveStatusScene.unRegisterCallback(CALLBACK_KEY);
        mNaviPackage.unregisterObserver(CALLBACK_KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
        isMapLoadSuccess = false;
        Logger.i(TAG, "onDestroy");
    }

    public void loadMapView() {
        Logger.i(TAG, "loadMapView:" + isMapLoadSuccess);
        if(mMapPackage == null || mViewModel == null) {
            Logger.e(TAG, "loadMapView: MapPackage or ViewModel is null");
            return;
        }
        mMapPackage.bindMapView(mViewModel.getMapView());
        mMapPackage.registerCallback(MAP_TYPE, this);
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        IMapPackageCallback.super.onMapLoadSuccess(mapTypeId);
        Logger.i(TAG, "onMapLoadSuccess:" + mapTypeId.name(), "isMapLoadSuccess:" + isMapLoadSuccess);
        if (mapTypeId == MAP_TYPE && !isMapLoadSuccess) {
            isMapLoadSuccess = true;
            gotoCarPosition();
            // TODO 这里暂时无法和主图保持一致，根据主图设置这里无法显示车标
            mLayerPackage.setDefaultCarMode(getMapType());
        }
    }

    @Override
    public void onManeuverInfo(final NaviManeuverInfo info) {
        mViewModel.onManeuverInfo(info);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        mNaviPackage.setTmcData(naviTmcInfo);
        mViewModel.onUpdateTMCLightBar(naviTmcInfo, false);
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        IGuidanceObserver.super.onNaviInfo(naviETAInfo);
        mViewModel.onNaviInfo(naviETAInfo);
    }

    @Override
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        IGuidanceObserver.super.onLaneInfo(isShowLane, laneInfoEntity);
        mViewModel.onLaneInfo(isShowLane, laneInfoEntity);
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        Logger.i(TAG, "onNaviStop");
        clearCacheInfo();
        mViewModel.onNaviStop();
        openOrCloseImmersive(true);
        gotoCarPosition();
    }

    // 回自车位
    private void gotoCarPosition() {
        mMapPackage.setMapCenter(getMapType(), new GeoPoint(mPositionPackage.getLastCarLocation().getLongitude(),
                mPositionPackage.getLastCarLocation().getLatitude()));
        mMapPackage.goToCarPosition(getMapType());
        mMapPackage.setMapCenterInScreen(getMapType(), mViewModel.getLogoPosition()[0], mViewModel.getLogoPosition()[1]);
    }

    @Override
    public void onMapClickPoi(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onMapClickPoi:" + mapTypeId.name());
        if (mapTypeId == MAP_TYPE) {
            mViewModel.startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onReversePoiClick(final MapType mapTypeId, final PoiInfoEntity poiInfo) {
        Logger.d(TAG, "onReversePoiClick:" + mapTypeId.name());
        if (mapTypeId == MAP_TYPE) {
            mViewModel.startMapActivity(INaviConstant.OpenIntentPage.POI_DETAIL_PAGE, poiInfo);
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        IMapPackageCallback.super.onMapTouchEvent(mapTypeId, touchEvent);
        Logger.i(TAG, "onMapTouchEvent:" + mapTypeId.name(), "isOnNavigating:" + isOnNavigating(), "isOnImmersive:" + isOnImmersive());
        openOrCloseImmersive(false);
    }

    @Override
    public void updateNextIcon(int resource, BitmapDrawable drawable) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextIconResource(resource);
            mNextManeuverEntity.setNextIconDrawable(drawable);
        }
    }

    @Override
    public void updateNextStatus(boolean isVisible, boolean isOffLine) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextManeuverVisible(isVisible);
            mNextManeuverEntity.setNextManeuverOffLine(isOffLine);
        }
    }

    @Override
    public NextManeuverEntity getNextManeuverEntity() {
        return mNextManeuverEntity;
    }

    @Override
    public void updateNextText(String text) {
        if (null != mNextManeuverEntity) {
            mNextManeuverEntity.setNextText(text);
        }
    }

    private MapType getMapType() {
        if (ConvertUtils.isNull(mViewModel)) {
            return MapType.MAIN_SCREEN_MAIN_MAP;
        }
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
        return mImmersiveStatusScene.getCurrentImersiveStatus(MAP_TYPE) == ImersiveStatus.IMERSIVE;
    }

    public void stopNavi() {
        mNaviPackage.stopNavigation(true);
    }

    public void showPreview() {
        Logger.i(TAG, "showPreview");
        mPreviewIsOnShowing = true;
        openOrCloseImmersive(false);
        mNaviPackage.setPreviewStatus(true);
        mLayerPackage.setFollowMode(MAP_TYPE, false);
        mRoutePackage.showPreview(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    public void closePreview() {
        mPreviewIsOnShowing = false;
        openOrCloseImmersive(true);
        mNaviPackage.setPreviewStatus(false);
        mLayerPackage.setFollowMode(MAP_TYPE, true);
        mMapPackage.exitPreview(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    public void muteOrUnMute() {
        mNaviPackage.setMute(!mNaviPackage.isMute());
    }

    public Boolean isMute() {
        return mNaviPackage.isMute();
    }

    public int getPowerType() {
        return mCalibrationPackage.powerType();
    }

    @Override
    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        IGuidanceObserver.super.onCrossImageInfo(isShowImage, naviImageInfo);
        mCrossImgOnShowing = isShowImage;
        showOrHideCross(naviImageInfo);
    }

    @Override
    public void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus lastImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange", "mapTypeId:" + mapTypeId.name(), "lastImersiveStatus:" + lastImersiveStatus.name());
        if (mapTypeId == MAP_TYPE) {
            mViewModel.onImmersiveStatusChange(lastImersiveStatus);
        }
    }

    public void openOrCloseImmersive(boolean isOpenImmersive) {
        Logger.i(TAG, "openOrCloseImmersive:" + isOpenImmersive, "mPreviewIsOnShowing:" + mPreviewIsOnShowing);
        final ImersiveStatus status = isOpenImmersive ? ImersiveStatus.IMERSIVE : ImersiveStatus.TOUCH;
        if (mImmersiveStatusScene.getCurrentImersiveStatus(MAP_TYPE) != status) {
            mImmersiveStatusScene.setImmersiveStatus(MAP_TYPE, status);
        }
        afterImmersiveChanged();
    }

    /***
     * 如果切换到沉浸态且处于全览，那么要退出全览
     */
    private void afterImmersiveChanged() {
        Logger.i(TAG, "currentImmersive:" + isOnImmersive(), "mPreviewIsOnShowing:" + mPreviewIsOnShowing);
        // 如果进入沉浸态且此刻正在全览，那么主动关闭全览
        if (isOnImmersive() && mPreviewIsOnShowing) {
            closePreview();
        }
        // 触摸态关闭锁住比例尺
        if (!isOnImmersive() && isOnNavigating()) {
            mLayerPackage.setDynamicLevelLock(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
            mLayerPackage.setFollowMode(MAP_TYPE, false);
        }
        // 导航中沉浸态开启自动比例尺
        if (isOnImmersive() && isOnNavigating()) {
            mLayerPackage.setDynamicLevelLock(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, false);
            mLayerPackage.setFollowMode(MAP_TYPE, true);
        }
    }

    public Rect getPreviewRect() {
        return mViewModel.getPreviewRect();
    }

    public NaviEtaInfo getCurrentNaviEtaInfo() {
        return mNaviPackage.getCurrentNaviEtaInfo();
    }

    public void setCrossRect(Rect rect) {
        mNaviPackage.setRoadCrossRect(MAP_TYPE, rect);
    }

    public boolean getCrossIsShowing() {
        return mCrossImgOnShowing;
    }

    public void showOrHideCross(CrossImageEntity currentEntity) {
        Logger.i(TAG, "showOrHideCross", "mCrossImgOnShowing:" + mCrossImgOnShowing);
        mViewModel.onCrossImageInfo(mCrossImgOnShowing);
        if (mCrossImgOnShowing) {
            LayerItemCrossEntity entity = new LayerItemCrossEntity();
            entity.setCrossImageEntity(currentEntity);
            final boolean isSuccess = mLayerPackage.showCross(MAP_TYPE, entity);
            mViewModel.showNextManeuver(isSuccess, mNextManeuverEntity);
        } else {
            if (!ConvertUtils.isNull(lastCrossImgEntity)) {
                mLayerPackage.hideCross(MAP_TYPE, lastCrossImgEntity.getType());
            }
        }
        lastCrossImgEntity = currentEntity;
    }

    public void onConfigurationChanged(ThemeType type) {
        mapAdapter.updateUiStyle(MAP_TYPE, type);
    }

    public CrossImageEntity getLastCrossEntity() {
        return lastCrossImgEntity;
    }
}