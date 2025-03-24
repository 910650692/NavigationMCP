package com.fy.navi.scene.impl.navi;

import android.annotation.SuppressLint;
import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.api.navi.ISceneNaviControl;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviControlView;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteRequestParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlImpl extends BaseSceneModel<SceneNaviControlView> implements
        ISceneNaviControl, NetWorkUtils.NetworkObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private LayerPackage mLayerPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
    private SearchPackage mSearchPackage;
    private SettingPackage mSettingPackage;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;
    public ObservableField<Boolean> mControlVisible;
    public ObservableField<Boolean> mGroupOneVisible;
    public ObservableField<Boolean> mGroupTwoVisible;
    public ObservableField<Boolean> mGroupMoreSetupVisible;
    private ISceneCallback mISceneCallback;
    // true:退出全览 false:看全览
    private boolean mIsPreViewShowing = false;
    private boolean mIsFixedOverview = false;//是否是固定全览状态
    private boolean mIsMute;

    public SceneNaviControlImpl(final SceneNaviControlView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mControlVisible = new ObservableField<>(false);
        mGroupOneVisible = new ObservableField<>(true);
        mGroupTwoVisible = new ObservableField<>(false);
        mGroupMoreSetupVisible = new ObservableField<>(true);
        mControlVisible.set(false);
    }

    @SuppressLint("WrongConstant")
    @Override
    protected void onCreate() {
        super.onCreate();
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(this);
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
        Logger.i(TAG, "naviContinue", "showPreview status : " + mIsFixedOverview);
        // 隐藏控制页面
        mControlVisible.set(false);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        if (!mIsFixedOverview) {
            // 更新全览状态
            mLayerPackage.setFollowMode(mMapTypeId, true);
            mMapPackage.setZoomLevel(mMapTypeId, 17);
            mIsPreViewShowing = false;
            mMapPackage.goToCarPosition(mMapTypeId, false, false);
            mNaviPackage.setPreviewStatus(false);
        }
        cancelTimer();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_PREFERENCE, false);
        }
    }

    @Override
    public void moreSetup() {
        Logger.i(TAG, "moreSetup");
        initTimer();
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
            if (mIsFixedOverview) {
                mIsFixedOverview = false;
            }
        } else {
            enterPreview();
        }
    }

    @Override
    public void switchOverview() {
        initTimer();
        Logger.i(TAG, "switchOverview isFixedOverview：" + mIsFixedOverview +
                ",isPreViewShowing：" + mIsPreViewShowing);
        // 固定全览状态下退出全览
        if (mIsFixedOverview) {
            //退出固定全览
            mIsFixedOverview = false;
            exitPreview();
        } else { // 非固定全览下退出全览
            if (mIsPreViewShowing) {
                isShowMoreSetup(true);
                exitPreview();
            } else {
                //触发全览
                isShowMoreSetup(false);
                mIsPreViewShowing = true;
                enterPreview();
                mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_SELECT);
                // 显示固定全览
                mScreenView.updateVariation(NaviConstant.VariationType.VARIATION_SELECT);
            }
        }
    }

    @Override
    public void onVariation() {
        initTimer();
        Logger.i(TAG, "onVariation isPreViewShowing：" + mIsPreViewShowing);
        if (mIsPreViewShowing && !mIsFixedOverview) {//固定全览
            isShowMoreSetup(true);
            mScreenView.changeOverViewControlLength(true);
            updateVariationVoice();
            mIsFixedOverview = true;
            cancelTimer();//固定全览时退出全览倒计时
            ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        } else {
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
        mNaviPackage.setPreviewStatus(false);
        // 退出全览
        mMapPackage.exitPreview(mMapTypeId);
        // 更新ui显示为“看全览”
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        // 回到当前位置
        mMapPackage.goToCarPosition(mMapTypeId, false, false);
        // 更新全览状态flag
        mIsPreViewShowing = false;
        mLayerPackage.setFollowMode(mMapTypeId, true);
        mLayerPackage.openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, SettingPackage.getInstance().
                getAutoScale());
        updateVariationVoice();
    }

    /**
     * 进入全览
     */
    private void enterPreview() {
        mNaviPackage.setPreviewStatus(true);
        mRoutePackage.naviShowPreview(mMapTypeId);
        mLayerPackage.openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, false);
        mLayerPackage.setFollowMode(mMapTypeId, false);
    }


    @Override
    public void refreshRoute() {
        Logger.i(TAG, "refreshRoute");
        initTimer();
        final RouteRequestParam param = new RouteRequestParam();
        param.setMMapTypeId(mMapTypeId);
        param.setMRouteWay(RouteWayID.ROUTE_WAY_CHANGE_PREFERENCE);
        // 算路那边在线刷新失败后会自动调用离线刷新，所以这边就调用一个接口就好
        mRoutePackage.requestRoute(param);
    }

    @Override
    public void naviBroadcast() {
        Logger.i(TAG, "naviBroadcast");
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
        mScreenView.updateBroadcast(broadcastMode);
        Logger.i(TAG, "updateBroadcast：" + broadcastMode);
    }

    @Override
    public void routePreference() {
        Logger.i(TAG, "routePreference");
        if (mISceneCallback != null) {
            cancelTimer();
            mISceneCallback.skipNaviPreferenceScene();
        }
    }

    @Override
    public void carHead() {
        Logger.i(TAG, "carHead");
        initTimer();
        mMapPackage.switchMapMode(mMapTypeId);
        mScreenView.updateCarModel(mMapPackage.getCurrentMapMode(mMapTypeId));
    }

    @Override
    public void naviSetting() {
        Logger.i(TAG, "naviSetting");
        if (mISceneCallback != null) {
            cancelTimer();
            mISceneCallback.skipSettingFragment();
        }
    }

    @Override
    public void alongSearch(final String key) {
        Logger.d(TAG, "alongSearch key：" + key);
        if (ConvertUtils.isEmpty(key)) {
            return;
        }
        if (key.contains(mScreenView.getContext().getString(R.string.navi_along_search))) {
            if (mISceneCallback != null) {
                mISceneCallback.skipAlongWayFragment();
            }
        } else {
            mSearchPackage.enRouteKeywordSearch(key);
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
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(this);
        mNaviPackage.setPreviewStatus(false);
    }

    /**
     * @param currentImersiveStatus ImmersiveStatus
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange currentImersiveStatus：" +
                currentImersiveStatus);
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
            mLayerPackage.setFollowMode(mMapTypeId, false);
            initControlState();
            initTimer();
        } else {
            naviContinue();
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
                ThreadManager.getInstance().postUi(this::naviContinue);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 初始化控制状态
     */
    private void initControlState() {
        mControlVisible.set(true);
        mGroupOneVisible.set(true);
        mGroupTwoVisible.set(false);
        mScreenView.updateOverview(mIsFixedOverview ? NaviConstant.OverviewType.OVERVIEW_FIXED : NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        isShowMoreSetup(true);
        final int broadcastMode = mSettingPackage.getConfigKeyBroadcastMode();
        mScreenView.updateBroadcast(broadcastMode);
        final MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        mScreenView.updateCarModel(currentMapMode);
        Logger.i(TAG, " initControlState isFixedOverview：" + mIsFixedOverview + ",isPreViewShowing：" + mIsPreViewShowing +
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

    /**
     * @param sceneCallback 添加场景回调
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    public void onNetConnectSuccess() {
        refreshRoute();
    }

    @Override
    public void onNetUnavailable() {
        refreshRoute();
    }

    @Override
    public void onNetBlockedStatusChanged() {
        refreshRoute();
    }

    @Override
    public void onNetLosing() {
        refreshRoute();
    }

    @Override
    public void onNetLinkPropertiesChanged() {
        refreshRoute();
    }

    @Override
    public void onNetDisConnect() {
        refreshRoute();
    }
}
