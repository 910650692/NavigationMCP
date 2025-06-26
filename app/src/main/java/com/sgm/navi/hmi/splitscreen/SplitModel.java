package com.sgm.navi.hmi.splitscreen;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import android.graphics.Rect;
import android.graphics.drawable.BitmapDrawable;
import android.view.MotionEvent;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/12
 * Description: [在这里描述文件功能]
 */
public class SplitModel extends BaseModel<BaseSplitViewModel> implements IMapPackageCallback, IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack, ISceneCallback{
    private static final String TAG = "SplitModel";
    private MapPackage mMapPackage;
    private MapAdapter mapAdapter;
    private NaviPackage mNaviPackage;
    private LayerPackage mLayerPackage;
    private RoutePackage mRoutePackage;
    private CalibrationPackage mCalibrationPackage;
    private final MapType MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;
    private final String CALLBACK_KEY = "SplitModel";
    private final NavistatusAdapter mNaviStatusAdapter;
    private boolean mPreviewIsOnShowing = false; // 全览状态，true代表正在全览
    private CrossImageEntity lastCrossImgEntity;

    private boolean mCrossImgOnShowing = false;
    private NextManeuverEntity mNextManeuverEntity;

    public SplitModel() {
        mapAdapter = MapAdapter.getInstance();
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mNaviPackage.registerObserver(CALLBACK_KEY, this);
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        ImmersiveStatusScene.getInstance().registerCallback(CALLBACK_KEY, this);
        mNextManeuverEntity = new NextManeuverEntity();
        lastCrossImgEntity = mNaviPackage.getLastCrossEntity();
//        mCrossImgOnShowing = mNaviPackage.isMCrossImgIsOnShowing();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        ImmersiveStatusScene.getInstance().unRegisterCallback(CALLBACK_KEY);
        mNaviPackage.unregisterObserver(CALLBACK_KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
        Logger.i(TAG, "onDestroy");
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
        mViewModel.onNaviStop();
        openOrCloseImmersive(true);
        mLayerPackage.clearRouteLine(MAP_TYPE);
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        Logger.d(TAG, MAP_TOUCH, "onMapTouchEvent mapTypeId:" , mapTypeId, " touchEvent:" , touchEvent);
        IMapPackageCallback.super.onMapTouchEvent(mapTypeId, touchEvent);
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

    /**
     * @return 是否正在导航中
     */
    public boolean isOnNavigating() {
        return ConvertUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
    }

    public void stopNavi() {
        mNaviPackage.stopNavigation();
    }

    public void showPreview() {
        Logger.i(TAG, "showPreview");
        mPreviewIsOnShowing = true;
        openOrCloseImmersive(false);
        mNaviPackage.setPreviewStatus(true);
        mLayerPackage.setFollowMode(MAP_TYPE, false);
        mLayerPackage.setPreviewMode(MAP_TYPE, true);
        mLayerPackage.setDynamicLevelLock(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, true);
        mRoutePackage.showPreview(MAP_TYPE);
    }

    public void closePreview() {
        mPreviewIsOnShowing = false;
        openOrCloseImmersive(true);
        mNaviPackage.setPreviewStatus(false);
        mLayerPackage.setFollowMode(MAP_TYPE, true);
        mLayerPackage.setPreviewMode(MAP_TYPE, false);
        mLayerPackage.setDynamicLevelLock(MAP_TYPE, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE, false);
        mMapPackage.exitPreview(MAP_TYPE);
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
        Logger.i(TAG, "onImmersiveStatusChange", "mapTypeId:" , mapTypeId.name(), "lastImersiveStatus:" , lastImersiveStatus.name());
        if (mapTypeId == MAP_TYPE) {
            mViewModel.onImmersiveStatusChange(lastImersiveStatus);
        }
    }

    public void openOrCloseImmersive(boolean isOpenImmersive) {
        Logger.d(TAG, MAP_TOUCH, "openOrCloseImmersive:" , isOpenImmersive, " mPreviewIsOnShowing:" , mPreviewIsOnShowing);
        final ImersiveStatus status = isOpenImmersive ? ImersiveStatus.IMERSIVE : ImersiveStatus.TOUCH;
        if (ImmersiveStatusScene.getInstance().getCurrentImersiveStatus(MAP_TYPE) != status) {
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MAP_TYPE, status);
        }
        afterImmersiveChanged();
    }

    /***
     * 如果切换到沉浸态且处于全览，那么要退出全览
     */
    private void afterImmersiveChanged() {
        Logger.i(TAG, "currentImmersive:" , isOnImmersive(), "mPreviewIsOnShowing:" , mPreviewIsOnShowing);
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
        Logger.i(TAG, "showOrHideCross", "mCrossImgOnShowing:" , mCrossImgOnShowing);
        if (ConvertUtils.isNull(mViewModel)) return;
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

    private boolean isOnImmersive() {
        return ImmersiveStatusScene.getInstance().getCurrentImersiveStatus(MAP_TYPE) == ImersiveStatus.IMERSIVE;
    }
}
