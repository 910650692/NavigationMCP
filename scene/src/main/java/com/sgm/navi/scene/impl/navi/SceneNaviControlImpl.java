package com.sgm.navi.scene.impl.navi;

import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;

import android.annotation.SuppressLint;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.api.navi.ISceneNaviControl;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.ui.navi.SceneNaviControlView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlImpl extends BaseSceneModel<SceneNaviControlView> implements
        ISceneNaviControl, SignalCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CONTROL_IMPL;
    private final NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
    private SignalPackage mSignalPackage;
    private ImmersiveStatusScene mImmersiveStatusScene;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    public ObservableField<Boolean> mGroupMoreSetupVisible;
    private boolean mIsMute;
    private int mVehicleType;

    public SceneNaviControlImpl(final SceneNaviControlView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSignalPackage = SignalPackage.getInstance();
        mGroupMoreSetupVisible = new ObservableField<>(true);
        mImmersiveStatusScene = ImmersiveStatusScene.getInstance();
    }

    @SuppressLint("WrongConstant")
    @Override
    protected void onCreate() {
        super.onCreate();
        showMain();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
        init();
        if (null != mSignalPackage) {
            mSignalPackage.registerObserver(this.getClass().getSimpleName(), this);
        }
    }

    public void refreshView() {
        if (mScreenView != null) {
            mScreenView.updateOverview(mNaviPackage.getPreviewStatus() ?
                    NaviConstant.OverviewType.OVERVIEW_FIXED :
                    NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        }
    }

    /**
     * 初始化设置参数
     */
    private void init() {
        // 初始化静音状态
        int muteStatus = SettingPackage.getInstance().getConfigKeyMute();
        Logger.i(TAG, "init muteStatus:", muteStatus);
        mNaviPackage.setCurrentNaviVolume(getNaviVolume());
        mIsMute = muteStatus == 1;
        mNaviPackage.setMuteByHmi(mIsMute);
        updateVariationVoice();
        // 初始化地图比例尺
        MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        switch (currentMapMode) {
            case NORTH_2D:
            case UP_2D:
                mMapPackage.setZoomLevel(mMapTypeId, 14);
                break;
            case UP_3D:
                mMapPackage.setZoomLevel(mMapTypeId, 17);
                break;
            default:
                break;
        }
    }

    @Override
    public void closeNavi() {
        Logger.i(TAG, "closeNavi");
        if (mCallBack != null) {
            mCallBack.closeNavi();
        }
        mNaviPackage.onNaviClose(true);
        mNaviPackage.stopNavigation();
        cancelTimer();
    }

    @Override
    public void naviContinue() {
        Logger.i(TAG, "naviContinue", "showPreview status : ", mNaviPackage.getFixedOverViewStatus());
    }

    @Override
    public void moreSetup() {
        Logger.i(TAG, "moreSetup");
        initTimer();
        setImmersiveStatus(ImersiveStatus.TOUCH);
        if(mCallBack != null){
            mCallBack.skipNaviControlMoreScene();
        }
    }

    @Override
    public void backControl(){}

    /**
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        Logger.i(TAG, "naviPreviewSwitch type:", type);
        if (type == 0) {
            exitPreview();
            if (mNaviPackage.getFixedOverViewStatus()) {
                mNaviPackage.setFixedOverViewStatus(false);
            }
        } else {
            enterPreview();
        }
    }

    @Override
    public void switchOverview() {
        initTimer();
        Logger.i(TAG, "switchOverview isFixedOverview：",
                mNaviPackage.getFixedOverViewStatus(),
                ",isPreViewShowing：", mNaviPackage.getPreviewStatus());
        // 固定全览状态下退出全览
        if (mNaviPackage.getFixedOverViewStatus()) {
            //退出固定全览
            mNaviPackage.setFixedOverViewStatus(false);
            setImmersiveStatus(ImersiveStatus.IMERSIVE);
            exitPreview();
        } else { // 非固定全览下退出全览
            setImmersiveStatus(ImersiveStatus.TOUCH);
            if (mNaviPackage.getPreviewStatus()) {
                isShowMoreSetup(true);
                exitPreview();
            } else {
                //触发全览
                clickToShowOverview();
            }
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_FULLVIEW)
    public void clickToShowOverview() {
        isShowMoreSetup(false);
        enterPreview();
        // 显示固定全览
        mScreenView.updateVariation(NaviConstant.VariationType.VARIATION_SELECT);
    }

    @Override
    public void onVariation() {
        initTimer();
        Logger.i(TAG, "onVariation isPreViewShowing：", mNaviPackage.getPreviewStatus());
        if (mNaviPackage.getPreviewStatus() && !mNaviPackage.getFixedOverViewStatus()) {//固定全览
            onFixedOverView();
        } else {
            setImmersiveStatus(ImersiveStatus.TOUCH);
            changeMuteStatus();
            updateSystemNaviVolume(mIsMute);
        }
    }

    /**
     * 改变静音状态
     */
    private void changeMuteStatus() {
        mIsMute = mNaviPackage.isMute();
        mNaviPackage.setMuteByHmi(!mIsMute);
        updateVariationVoice();
    }

    public void onFixedOverView() {
        isShowMoreSetup(true);
        mScreenView.changeOverViewControlLength(true);
        updateVariationVoice();
        mNaviPackage.setFixedOverViewStatus(true);
        mImmersiveStatusScene.setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
    }

    /**
     * @param isShow 是否显示设置按钮
     */
    private void isShowMoreSetup(final boolean isShow) {
        // 显示或隐藏设置按钮
        mGroupMoreSetupVisible.set(isShow);
        mScreenView.changeOverViewControlLength(isShow);
    }

    /**
     * 退出全览
     */
    private void exitPreview() {
        Logger.i(TAG, "exitPreview");
        mNaviPackage.setClusterFixOverViewStatus(false);
        // 更新ui显示为“看全览”
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        OpenApiHelper.exitPreview(mMapTypeId);
        if (mCallBack != null) {
            mCallBack.cancelClusterOverViewTimer();
        }
    }

    /**
     * 进入全览
     */
    private void enterPreview() {
        if (mCallBack != null) {
            mCallBack.cancelClusterOverViewTimer();
        }
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_SELECT);
        OpenApiHelper.enterPreview(mMapTypeId);
    }

    @Override
    public void refreshRoute() {}

    @Override
    public void naviBroadcast() {}

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_VOICE_SELECT)
    private void sendBroadcastModeTts(int broadcastMode) {
        String tts = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE -> BuryConstant.BroadcastMode.CONCISE;
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM ->
                    BuryConstant.BroadcastMode.MINIMALISM;
            default -> BuryConstant.BroadcastMode.DETAIL;
        };
        BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, tts)
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    @Override
    public void routePreference() {}

    @Override
    public void carHead() {}

    @Override
    public void naviSetting() {}

    @Override
    public void alongSearch(final int index) {}

    @Override
    public ObservableField<Boolean> getGroupMoreSetupField() {
        return mGroupMoreSetupVisible;
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        cancelTimer();
        if (null != mSignalPackage) {
            mSignalPackage.unregisterObserver(this.getClass().getSimpleName());
        }
        mImmersiveStatusScene = null;
    }

    /**
     * @param currentImersiveStatus ImmersiveStatus
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange currentImersiveStatus：",
                currentImersiveStatus);
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
        } else {
            showMain();
        }
    }

    /**
     * 开始倒计时
     */
    public void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(this::showMain);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 显示主页面
     */
    @Override
    public void showMain() {
        try {
            NaviSceneManager.getInstance().notifySceneReset(true);
            isShowMoreSetup(true);
            mScreenView.updateOverview(mNaviPackage.getPreviewStatus() ?
                    NaviConstant.OverviewType.OVERVIEW_FIXED :
                    NaviConstant.OverviewType.OVERVIEW_DEFAULT);
            updateVariationVoice();
            Logger.i(TAG, " initControlState isFixedOverview：",
                    mNaviPackage.getFixedOverViewStatus(), ",isPreViewShowing：",
                    mNaviPackage.getPreviewStatus());
        } catch (Exception e) {
            Logger.e(TAG, "showMain error:", e.getMessage());
        }
    }

    /**
     * 取消倒计时
     */
    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * 更新静音状态
     */
    private void updateVariationVoice() {
        mIsMute = mNaviPackage.isMute();
        Logger.d(TAG, "updateVariationVoice mIsMute：", mIsMute);
        mScreenView.updateVariation((mIsMute ? NaviConstant.VariationType.VARIATION_MUTE :
                NaviConstant.VariationType.VARIATION_BROADCAST));
    }

    public void notifySceneStateChange(final boolean isVisible) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviControlImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                        INaviSceneEvent.SceneStateChangeType.SceneShowState :
                        INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                mScreenView.getSceneId());
    }

    public int getCarType() {
        mVehicleType = OpenApiHelper.powerType();
        return mVehicleType;
    }

    private void setImmersiveStatus(ImersiveStatus immersiveStatus) {
        // 固定全览状态下操作控制栏不会显示继续导航按钮,已和UE确认
        if (mNaviPackage.getFixedOverViewStatus() || mNaviPackage.getClusterFixOverViewStatus()) {
            return;
        }
        if (null != mImmersiveStatusScene) {
            Logger.d(TAG, MAP_TOUCH, "immersiveStatus:" , immersiveStatus);
            mImmersiveStatusScene.setImmersiveStatus(mMapTypeId, immersiveStatus);
        }
    }

    /**
     * 获取导航音量值
     * @return 返回导航音量值
     */
    private int getNaviVolume() {
        if (null != mSignalPackage) {
            return mSignalPackage.getNaviVolume();
        } else {
            return NumberUtils.NUM_ERROR;
        }
    }

    /**
     * @param volume 设置导航音量值
     */
    private void setNaviVolume(int volume) {
        if (null != mSignalPackage) {
            mSignalPackage.setNaviVolume(volume);
        }
    }

    private void updateSystemNaviVolume(boolean isMute) {
        int currentSystemVolume = getNaviVolume();
        int lastSystemVolume = mNaviPackage.getCurrentNaviVolume();
        Logger.i(TAG, "updateSystemNaviVolume isMute:", isMute, " currentSystemVolume:",
                currentSystemVolume, " lastSystemNaviVolume:", lastSystemVolume);
        // 当前静音并且系统不是静音状态，进行静音操作
        if (isMute && currentSystemVolume > NumberUtils.NUM_0) {
            setNaviVolume(NumberUtils.NUM_0);
            // 不是静音，但是目前系统音量是静音状态，主动给音量设值
        } else if (!isMute && currentSystemVolume <= NumberUtils.NUM_0) {
            // 如果最后一次音量设置是有效值，就设置给系统，不然默认设置为31，即是一半的音量
            if (lastSystemVolume > NumberUtils.NUM_0) {
                setNaviVolume(lastSystemVolume);
            } else {
                setNaviVolume(NumberUtils.NUM_31);
            }
        }
    }

    @Override
    public void onNaviVolumeChanged(int volume) {
        mIsMute = mNaviPackage.isMute();
        Logger.i(TAG, "onNaviVolumeChanged volume:", volume, " mIsMute:", mIsMute,
                " mLastSystemNaviVolume:", mNaviPackage.getCurrentNaviVolume());
        // 导航音量为0时，导航音量静音
        if (volume == NumberUtils.NUM_0) {
            if (!mIsMute) {
                changeMuteStatus();
                return;
            }
            // 主动放大音量破除静音状态
        } else if (volume > NumberUtils.NUM_0) {
            if (!mIsMute) {
                mNaviPackage.setCurrentNaviVolume(volume);
            } else {
                changeMuteStatus();
                return;
            }
        }
        updateVariationVoice();
    }
}
