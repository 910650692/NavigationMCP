package com.fy.navi.scene.impl.navi;

import android.annotation.SuppressLint;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.ISceneNaviControl;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneNaviControlView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.route.RoutePriorityType;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.speech.SpeechPackage;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlImpl extends BaseSceneModel<SceneNaviControlView> implements
        ISceneNaviControl {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private LayerPackage mLayerPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
    private SearchPackage mSearchPackage;
    private SettingPackage mSettingPackage;
    private ImmersiveStatusScene mImmersiveStatusScene;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    public ObservableField<Boolean> mControlVisible;
    public ObservableField<Boolean> mGroupOneVisible;
    public ObservableField<Boolean> mGroupTwoVisible;
    public ObservableField<Boolean> mGroupMoreSetupVisible;

    private long mLastClickTime;
    private boolean mIsNeedShowChargeTipLater;
    // true:退出全览 false:看全览
//    private boolean mIsPreViewShowing = false;
//    private boolean mIsFixedOverview = false;//是否是固定全览状态
    private boolean mIsMute;

    private int mVehicleType;

    public SceneNaviControlImpl(final SceneNaviControlView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mControlVisible = new ObservableField<>(true);
        mGroupOneVisible = new ObservableField<>(true);
        mGroupTwoVisible = new ObservableField<>(false);
        mGroupMoreSetupVisible = new ObservableField<>(true);
        mImmersiveStatusScene = ImmersiveStatusScene.getInstance();
        mLastClickTime = System.currentTimeMillis();
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
        SettingPackage.getInstance().setConfigKeyMute(0);
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
        Logger.i(TAG, "naviContinue", "showPreview status : " +
                mNaviPackage.getFixedOverViewStatus());
        // 隐藏控制页面
//        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
//        if (!mIsFixedOverview) {
//            // 更新全览状态
//            mLayerPackage.setFollowMode(mMapTypeId, true);
//            mMapPackage.setZoomLevel(mMapTypeId, 17);
//            mIsPreViewShowing = false;
//            mMapPackage.goToCarPosition(mMapTypeId, false, false);
//            mNaviPackage.setPreviewStatus(false);
//        }
//        cancelTimer();
//        if (mCallBack != null) {
//            mCallBack.updateSceneVisible(NaviSceneId.NAVI_SCENE_PREFERENCE, false);
//        }
    }

    @Override
    public void moreSetup() {
        Logger.i(TAG, "moreSetup");
        initTimer();
        setImmersiveStatus(ImersiveStatus.TOUCH);
        // 更多页面展开后关闭EV消息卡片
        mIsNeedShowChargeTipLater = mCallBack.isNeedCloseNaviChargeTipLater();
        if (mIsNeedShowChargeTipLater) {
            NaviSceneManager.getInstance().notifySceneStateChange(INaviSceneEvent.
                    SceneStateChangeType.SceneCloseState, NaviSceneId.NAVI_CHARGE_TIP);
        }
        mGroupOneVisible.set(false);
        mGroupTwoVisible.set(true);
    }

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
            SettingPackage.getInstance().setConfigKeyMute(!mIsMute ? 1 : 0);
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
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_REFRESHMAP)
    public void refreshRoute() {
        Logger.i(TAG, "refreshRoute");
        long currentTime = System.currentTimeMillis();
        boolean isCanRefreshRoute = currentTime - mLastClickTime > NumberUtils.NUM_2000;
        if (!isCanRefreshRoute) {
            ToastUtils.Companion.getInstance().showCustomToastView(
                    ResourceUtils.Companion.getInstance().getString(R.string.current_newest_path));
            return;
        }
        mLastClickTime = currentTime;
        initTimer();
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mMapTypeId);
        param.setMRouteWay(RouteWayID.ROUTE_WAY_REFRESH);
        param.setMRoutePriorityType(RoutePriorityType.ROUTE_TYPE_MANUAL_REFRESH);
        // 算路那边在线刷新失败后会自动调用离线刷新，所以这边就调用一个接口就好
        mRoutePackage.requestRoute(param);
    }

    @Override
    public void naviBroadcast() {
        Logger.i(TAG, "naviBroadcast");
        setImmersiveStatus(ImersiveStatus.TOUCH);
        initTimer();
        switchBroadcastMode();
    }

    /*** 切换播报模式***/
    public void switchBroadcastMode() {
        int broadcastMode = mSettingPackage.getConfigKeyBroadcastMode();
        broadcastMode = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_DETAIL ->
                    NaviConstant.BroadcastType.BROADCAST_CONCISE;
            case NaviConstant.BroadcastType.BROADCAST_CONCISE ->
                    NaviConstant.BroadcastType.BROADCAST_MINIMALISM;
            default -> NaviConstant.BroadcastType.BROADCAST_DETAIL;
        };
        mSettingPackage.setConfigKeyBroadcastMode(broadcastMode);
        broadcastModeSwitchTts(broadcastMode);
        mScreenView.updateBroadcast(broadcastMode);
        Logger.i(TAG, "updateBroadcast：" + broadcastMode);

        //For Bury Point
        sendBroadcastModeTts(broadcastMode);
    }

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

    private void broadcastModeSwitchTts(int broadcastMode) {
        String tts = switch (broadcastMode) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE ->
                    String.format(ResourceUtils.Companion.getInstance().
                                    getString(R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance().
                                    getString(R.string.navi_broadcast_concise));
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM ->
                    String.format(ResourceUtils.Companion.getInstance().
                                    getString(R.string.navi_broadcast_switch),
                            ResourceUtils.Companion.getInstance().
                                    getString(R.string.navi_broadcast_minimalism));
            default -> String.format(ResourceUtils.Companion.getInstance().
                            getString(R.string.navi_broadcast_switch),
                    ResourceUtils.Companion.getInstance().
                            getString(R.string.navi_broadcast_detail));
        };
        SpeechPackage.getInstance().synthesize(tts);
    }

    @Override
    public void routePreference() {
        Logger.i(TAG, "routePreference");
        setImmersiveStatus(ImersiveStatus.TOUCH);
        if (mCallBack != null) {
            cancelTimer();
            mCallBack.skipNaviPreferenceScene();
        }
    }

    @Override
    public void carHead() {
        Logger.i(TAG, "carHead");
        setImmersiveStatus(ImersiveStatus.TOUCH);
        initTimer();
        MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        boolean result = mMapPackage.switchMapMode(mMapTypeId);
        MapMode switchedMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        //如果切换前后模式一样，没有切换成功发toast提示
        if (!result || currentMapMode == switchedMapMode) {
            String failModeText = mScreenView.updateCarModel(switchedMapMode);
            ToastUtils.Companion.getInstance().showCustomToastView(String.
                    format(ResourceUtils.Companion.getInstance().
                            getString(R.string.navi_map_mode_switch_fail), failModeText));
            return;
        }
        mScreenView.updateCarModel(switchedMapMode);
    }

    @Override
    public void naviSetting() {
        Logger.i(TAG, "naviSetting");
        if (mCallBack != null) {
            cancelTimer();
            mCallBack.skipSettingFragment();
        }
    }

    @Override
    public void alongSearch(final int index) {
        Logger.i(TAG, "alongSearch index:" + index + " mVehicleType:" + mVehicleType);
        setImmersiveStatus(ImersiveStatus.TOUCH);
        switch (index) {
            case 0:
                if (mVehicleType == 1) {//电车
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_charge), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_station), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 1:
                if (mVehicleType == 1 || mVehicleType == 0) {//电车-油车
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_lavatory), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_charge), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 2:
                if (mVehicleType == 1 || mVehicleType == 0) {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_parking), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_lavatory), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 3:
                if (mVehicleType == 1 || mVehicleType == 0) {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.navi_along_service), OpenApiHelper.ALONG_WAY);
                } else {
                    mCallBack.goSearchView(ResourceUtils.Companion.getInstance().
                            getString(R.string.st_quick_search_parking), OpenApiHelper.ALONG_WAY);
                }
                break;
            case 4:
                mCallBack.goAlongWayList();
                break;
            default:
                break;
        }
    }

    @Override
    public ObservableField<Boolean> getControlField() {
        return mControlVisible;
    }

    @Override
    public ObservableField<Boolean> getGroupOneField() {
        return mGroupOneVisible;
    }

    @Override
    public ObservableField<Boolean> getGroupTwoField() {
        return mGroupTwoVisible;
    }

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
        // 更多页面收起后判断是否需要再显示消息卡片
        if (mIsNeedShowChargeTipLater) {
            NaviSceneManager.getInstance().notifySceneStateChange(INaviSceneEvent.
                    SceneStateChangeType.SceneShowState, NaviSceneId.NAVI_CHARGE_TIP);
        }
        mControlVisible.set(true);
        mGroupOneVisible.set(true);
        mGroupTwoVisible.set(false);
        isShowMoreSetup(true);
        mScreenView.updateOverview(mNaviPackage.getFixedOverViewStatus() ?
                NaviConstant.OverviewType.OVERVIEW_FIXED :
                NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        final int broadcastMode = mSettingPackage.getConfigKeyBroadcastMode();
        mScreenView.updateBroadcast(broadcastMode);
        final MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        mScreenView.updateCarModel(currentMapMode);
        Logger.i(TAG, " initControlState isFixedOverview：" +
                mNaviPackage.getFixedOverViewStatus() + ",isPreViewShowing：" +
                mNaviPackage.getPreviewStatus() +
                ",broadcastMode：" + broadcastMode + ",MapMode：" + currentMapMode);
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
