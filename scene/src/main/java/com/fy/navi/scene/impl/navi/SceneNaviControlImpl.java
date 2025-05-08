package com.fy.navi.scene.impl.navi;

import android.annotation.SuppressLint;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.ISceneNaviControl;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneNaviControlView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlImpl extends BaseSceneModel<SceneNaviControlView> implements
        ISceneNaviControl {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
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
    }

    /**
     * 初始化设置参数
     */
    private void init() {
        // 初始化静音状态、每次导航恢复默认播报状态
        mIsMute = false;
        mNaviPackage.setMute(false);
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
        mRoutePackage.clearRouteLine(mMapTypeId);
        mNaviPackage.stopNavigation();
        cancelTimer();
        mNaviPackage.setPreviewStatus(false);
    }

    @Override
    public void naviContinue() {
        Logger.i(TAG, "naviContinue", "showPreview status : " + mNaviPackage.getFixedOverViewStatus());
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
        Logger.i(TAG, "naviPreviewSwitch type:" + type);
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
        Logger.i(TAG, "switchOverview isFixedOverview：" + mNaviPackage.getFixedOverViewStatus() +
                ",isPreViewShowing：" + mNaviPackage.getPreviewStatus());
        // 固定全览状态下退出全览
        if (mNaviPackage.getFixedOverViewStatus()) {
            //退出固定全览
            mNaviPackage.setFixedOverViewStatus(false);
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
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_SELECT);
        // 显示固定全览
        mScreenView.updateVariation(NaviConstant.VariationType.VARIATION_SELECT);
    }

    @Override
    public void onVariation() {
        initTimer();
        Logger.i(TAG, "onVariation isPreViewShowing：" + mNaviPackage.getPreviewStatus());
        if (mNaviPackage.getPreviewStatus() && !mNaviPackage.getFixedOverViewStatus()) {//固定全览
            isShowMoreSetup(true);
            mScreenView.changeOverViewControlLength(true);
            updateVariationVoice();
            mNaviPackage.setFixedOverViewStatus(true);
            mImmersiveStatusScene.setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        } else {
            setImmersiveStatus(ImersiveStatus.TOUCH);
            mIsMute = mNaviPackage.isMute();
            mNaviPackage.setMute(!mIsMute);
            updateVariationVoice();
        }
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
        // 更新ui显示为“看全览”
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        OpenApiHelper.exitPreview(mMapTypeId);
    }

    /**
     * 进入全览
     */
    private void enterPreview() {
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
        mNaviPackage.setPreviewStatus(false);
        mNaviPackage.setFixedOverViewStatus(false);
        mImmersiveStatusScene = null;
        cancelTimer();
    }

    /**
     * @param currentImersiveStatus ImmersiveStatus
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange currentImersiveStatus：" +
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
        NaviSceneManager.getInstance().notifySceneReset(true);
        isShowMoreSetup(true);
        mScreenView.updateOverview(mNaviPackage.getFixedOverViewStatus() ?
                NaviConstant.OverviewType.OVERVIEW_FIXED :
                NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        Logger.i(TAG, " initControlState isFixedOverview：" +
                mNaviPackage.getFixedOverViewStatus() + ",isPreViewShowing：" +
                mNaviPackage.getPreviewStatus());
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
        Logger.d(TAG, "updateVariationVoice mIsMute：" + mIsMute);
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
        if (mNaviPackage.getFixedOverViewStatus()) {
            return;
        }
        if (null != mImmersiveStatusScene) {
            mImmersiveStatusScene.setImmersiveStatus(mMapTypeId, immersiveStatus);
        }
    }
}
