package com.sgm.navi.hmi.splitscreen;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.launcher.IDeskBackgroundChangeListener;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.adapter.navistatus.INaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseModel;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/12
 * Description: [在这里描述文件功能]
 */
public class SplitModel extends BaseModel<BaseSplitViewModel> implements IMapPackageCallback, IGuidanceObserver, ImmersiveStatusScene.IImmersiveStatusCallBack,
        ISceneCallback, INaviStatusCallback, IDeskBackgroundChangeListener {
    private static final String TAG = "SplitModel";
    private MapPackage mMapPackage;
    private NaviPackage mNaviPackage;
    private LayerPackage mLayerPackage;
    private RoutePackage mRoutePackage;
    private CalibrationPackage mCalibrationPackage;
    private final MapType MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;
    private final String CALLBACK_KEY = "SplitModel";
    private final NavistatusAdapter mNaviStatusAdapter;
    private boolean mPreviewIsOnShowing = false; // 全览状态，true代表正在全览
    private ImersiveStatus mImmersiveStatus;
    private String mNaviStatus;
    public SplitModel() {
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        // 隐藏路口大图
        if (!ConvertUtils.isNull(mNaviPackage.getLastCrossEntity())) {
            mLayerPackage.hideCross(MapType.MAIN_SCREEN_MAIN_MAP, mNaviPackage.getLastCrossEntity().getType());
        }
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mNaviPackage.registerObserver(CALLBACK_KEY, this);
        ImmersiveStatusScene.getInstance().registerCallback(CALLBACK_KEY, this);
        mNaviStatusAdapter.registerCallback(this);
        FloatViewManager.getInstance().addDeskBackgroundChangeListener(this);
        mNaviStatus = mNaviStatusAdapter.getCurrentNaviStatus();
        mImmersiveStatus = ImmersiveStatusScene.getInstance().getCurrentImersiveStatus(MAP_TYPE);
        lockMapSomeActions(true);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        lockMapSomeActions(false);
        ImmersiveStatusScene.getInstance().unRegisterCallback(CALLBACK_KEY);
        mNaviPackage.unregisterObserver(CALLBACK_KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
        mNaviStatusAdapter.unRegisterCallback(this);
        FloatViewManager.getInstance().removeDeskBackgroundChangeListener(this);
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

    /**
     * @return 是否正在导航中
     */
    public boolean isOnNavigating() {
        return ConvertUtils.equals(mNaviStatus, NaviStatus.NaviStatusType.NAVING);
    }

    public void stopNavi() {
        mNaviPackage.stopNavigation();
    }

    public void showPreview() {
        Logger.i(TAG, "showPreview");
        OpenApiHelper.enterPreview(MAP_TYPE);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MAP_TYPE, ImersiveStatus.TOUCH);
        mViewModel.startPreviewSchedule();
    }

    public void closePreview() {
        OpenApiHelper.exitPreview(MAP_TYPE);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MAP_TYPE, ImersiveStatus.IMERSIVE);
        mViewModel.stopPreviewSchedule();
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
    public void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus lastImersiveStatus) {
        if (mImmersiveStatus != lastImersiveStatus) {
            mImmersiveStatus = lastImersiveStatus;
            // 更新UI状态仅当触摸态发生变化的时候
            mViewModel.updateUiStateAfterImmersiveChanged(lastImersiveStatus);
        } else {
            Logger.w(TAG, "onImmersiveStatusChange-状态未改变，无需处理！");
        }
    }

    public void openOrCloseImmersive(boolean isOpenImmersive) {
        Logger.d(TAG, MAP_TOUCH, "openOrCloseImmersive:" , isOpenImmersive, " mPreviewIsOnShowing:" , mPreviewIsOnShowing);
        final ImersiveStatus status = isOpenImmersive ? ImersiveStatus.IMERSIVE : ImersiveStatus.TOUCH;
        if (ImmersiveStatusScene.getInstance().getCurrentImersiveStatus(MAP_TYPE) != status) {
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MAP_TYPE, status);
        }
    }

    public NaviEtaInfo getCurrentNaviEtaInfo() {
        return mNaviPackage.getCurrentNaviEtaInfo();
    }

    public boolean isOnImmersive() {
        return mImmersiveStatus == ImersiveStatus.IMERSIVE;
    }

    public boolean isOnTouch() {
        return mImmersiveStatus == ImersiveStatus.TOUCH;
    }

    /***
     * 显示或关闭全览
     */
    public void showOrClosePreview() {
        if (!isOnNavigating()) {
            Logger.d(TAG, "非导航态无需显示或者关闭全览！");
            return;
        }
        if (mPreviewIsOnShowing) {
            closePreview();
        } else {
            showPreview();
        }
    }

    /***
     * 导航状态改变
     * @param naviStatus 导航状态
     */
    @Override
    public void onNaviStatusChange(String naviStatus) {
        this.mNaviStatus = naviStatus;
        mViewModel.updateUiStateAfterNaviStatusChanged(naviStatus);
    }

    @Override
    public void onDeskBackgroundChange(FloatViewManager.DesktopMode desktopMode) {
        mViewModel.updateUiStateAfterDeskBackgroundChanged(desktopMode);
    }

    /**
     * 1/3屏状态  不支持缩放 不支持点击  支持移图
     */
    public void lockMapSomeActions(boolean isSplit) {
        Logger.d("ForTest", "lockMapSomeActions: " + isSplit);
        mMapPackage.isSplitScreen(MapType.MAIN_SCREEN_MAIN_MAP, isSplit);
        mMapPackage.setMapLabelClickable(MapType.MAIN_SCREEN_MAIN_MAP, !isSplit);
        mMapPackage.setLockMapPinchZoom(MapType.MAIN_SCREEN_MAIN_MAP, isSplit);
    }

    public void exitPreviewIfNeeded() {
        if (mNaviPackage.getPreviewStatus()) {
            closePreview();
        }
    }
}
