package com.fy.navi.scene.impl.navi;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
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
import com.fy.navi.service.adapter.navi.bls.NaviDataFormatHelper;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteWayID;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.List;
import java.util.concurrent.ScheduledFuture;

public class SceneNaviControlImpl extends BaseSceneModel<SceneNaviControlView> implements ISceneNaviControl {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private LayerPackage mLayerPackage;
    private MapPackage mMapPackage;
    private RoutePackage mRoutePackage;
    private SearchPackage mSearchPackage;
    private SettingPackage mSettingPackage;
    private ScheduledFuture scheduledFuture;
    private int times = NumberUtils.NUM_8;
    public ObservableField<Boolean> controlVisible;
    public ObservableField<Boolean> groupOneVisible;
    public ObservableField<Boolean> groupTwoVisible;
    public ObservableField<Boolean> groupMoreSetupVisible;
    private ISceneCallback mISceneCallback;
    // true:退出全览 false:看全览
    private boolean isPreViewShowing = false;
    private boolean isFixedOverview = false;//是否是固定全览状态
    private boolean mIsMute;

    public SceneNaviControlImpl(SceneNaviControlView mScreenView) {
        super(mScreenView);
        mNaviPackage = NaviPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        controlVisible = new ObservableField<>(false);
        groupOneVisible = new ObservableField<>(true);
        groupTwoVisible = new ObservableField<>(false);
        groupMoreSetupVisible = new ObservableField<>(true);
        controlVisible.set(false);
    }

    @Override
    public void closeNavi() {
        Logger.i(TAG, "closeNavi");
        mRoutePackage.clearRouteLine(mMapTypeId);
        mNaviPackage.stopNavigation();
        cancelTimer();
    }

    @Override
    public void naviContinue() {
        Logger.i(TAG, "naviContinue");
        controlVisible.set(false);
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        if (!isFixedOverview) {
            // 更新全览状态
            mLayerPackage.setFollowMode(mMapTypeId, true);
            mMapPackage.setZoomLevel(mMapTypeId, 17);
            isPreViewShowing = false;
            mMapPackage.goToCarPosition(mMapTypeId, false, false);
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
        groupOneVisible.set(false);
        groupTwoVisible.set(true);
    }

    @Override
    public void switchOverview() {
        initTimer();
        Logger.i(TAG, "switchOverview isFixedOverview：" + isFixedOverview +
                ",isPreViewShowing：" + isPreViewShowing);
        // 固定全览状态下退出全览
        if (isFixedOverview) {
            //退出固定全览
            isFixedOverview = false;
            exitPreview();
        } else { // 非固定全览下退出全览
            if (isPreViewShowing) {
                isShowMoreSetup(true);
                exitPreview();
            } else {
                //触发全览
                isShowMoreSetup(false);
                isPreViewShowing = true;
                mRoutePackage.naviShowPreview(mMapTypeId);
                mLayerPackage.openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, false);
                mLayerPackage.setFollowMode(mMapTypeId, false);
                mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_SELECT);
                // 显示固定全览
                mScreenView.updateVariation(NaviConstant.VariationType.VARIATION_SELECT);
            }
        }
    }

    /**
     * @param isShow 是否显示设置按钮
     */
    private void isShowMoreSetup(boolean isShow) {
        // 显示或隐藏设置按钮
        groupMoreSetupVisible.set(isShow);
        mScreenView.changeOverViewControlLength(isShow);
    }

    /**
     * 退出全览
     */
    private void exitPreview() {
        Logger.i(TAG, "exitPreview");
        // 退出全览
        mMapPackage.exitPreview(mMapTypeId);
        // 更新ui显示为“看全览”
        mScreenView.updateOverview(NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        // 回到当前位置
        mMapPackage.goToCarPosition(mMapTypeId, false, false);
        // 更新全览状态flag
        isPreViewShowing = false;
        mLayerPackage.setFollowMode(mMapTypeId, true);
        mLayerPackage.openDynamicLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP, SettingPackage.getInstance().getAutoScale());
        updateVariationVoice();
    }

    @Override
    public void onVariation() {
        initTimer();
        Logger.i(TAG, "onVariation isPreViewShowing：" + isPreViewShowing);
        if (isPreViewShowing && !isFixedOverview) {//固定全览
            isShowMoreSetup(true);
            mScreenView.changeOverViewControlLength(true);
            updateVariationVoice();
            isFixedOverview = true;
            cancelTimer();//固定全览时退出全览倒计时
            ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        } else {
            mIsMute = mNaviPackage.isMute();
            mNaviPackage.setMute(!mIsMute);
            updateVariationVoice();
        }
    }


    @Override
    public void refreshRoute() {
        Logger.i(TAG, "refreshRoute");
        initTimer();
        List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            RouteParam routeParam = allPoiParamList.get(allPoiParamList.size() - 1);
            mRoutePackage.requestRoute(mMapTypeId, NaviDataFormatHelper.getPoiInfoEntity(routeParam), routeParam.getPoiType(), true, RouteWayID.ROUTE_WAY_REFRESH);
        }
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
    public void alongSearch(String key) {
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
        return controlVisible;
    }

    @Override
    public ObservableField<Boolean> getGroupOneField() {
        return groupOneVisible;
    }

    @Override
    public ObservableField<Boolean> getGroupTwoField() {
        return groupTwoVisible;
    }

    @Override
    public ObservableField<Boolean> getGroupMoreSetupField() {
        return groupMoreSetupVisible;
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

    public void onImmersiveStatusChange(ImersiveStatus currentImersiveStatus) {
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

    public void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelTimer();
        times = NumberUtils.NUM_8;
        scheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (times == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(this::naviContinue);
            }
            times--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    private void initControlState() {
        controlVisible.set(true);
        groupOneVisible.set(true);
        groupTwoVisible.set(false);
        mScreenView.updateOverview(isFixedOverview ? NaviConstant.OverviewType.OVERVIEW_FIXED : NaviConstant.OverviewType.OVERVIEW_DEFAULT);
        updateVariationVoice();
        isShowMoreSetup(true);
        int broadcastMode = mSettingPackage.getConfigKeyBroadcastMode();
        mScreenView.updateBroadcast(broadcastMode);
        MapMode currentMapMode = mMapPackage.getCurrentMapMode(mMapTypeId);
        mScreenView.updateCarModel(currentMapMode);
        Logger.i(TAG, " initControlState isFixedOverview：" + isFixedOverview + ",isPreViewShowing：" + isPreViewShowing +
                ",broadcastMode：" + broadcastMode + ",MapMode：" + currentMapMode);
    }

    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(scheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(scheduledFuture);
            scheduledFuture = null;
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

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }
}
