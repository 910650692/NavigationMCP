package com.sgm.navi.hmi.navi;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;
import static com.sgm.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_CONTROL;
import static com.sgm.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_ETA;
import static com.sgm.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TBT;
import static com.sgm.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TMC;

import android.Manifest;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.telephony.PhoneStateListener;
import android.telephony.TelephonyManager;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentNaviGuidanceBinding;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.dialog.RouteLoadingDialog;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.scene.impl.search.SearchFragmentFactory;
import com.sgm.navi.scene.ui.navi.ChargeTipEntity;
import com.sgm.navi.scene.ui.navi.SceneNaviControlMoreView;
import com.sgm.navi.scene.ui.navi.SceneNaviSapaDetailView;
import com.sgm.navi.scene.ui.navi.SceneNaviSapaView;
import com.sgm.navi.scene.ui.navi.SceneNaviViaDetailView;
import com.sgm.navi.scene.ui.navi.SceneNaviViaListView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.HandCardType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.android.utils.ScreenTypeUtils;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.StackManager;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class NaviGuidanceFragment extends BaseFragment<FragmentNaviGuidanceBinding, NaviGuidanceViewModel> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_VIEW;
    private SceneNaviViaListView mSceneNaviViaListView;
    private SceneNaviViaDetailView mSceneNaviViaDetailView;
    private SceneNaviControlMoreView mSceneNaviControlMoreView;
    private SceneNaviSapaDetailView mSceneNaviSapaDetailView;
    private SceneNaviSapaView mSceneNaviSapaView;
    private PhoneStateListener mPhoneStateListener;
    private int mBroadCastModeBeforeCall = NumberUtils.NUM_ERROR;
    private int mBroadCastModeAfterCall = NumberUtils.NUM_ERROR;
    private BroadcastReceiver mTimeFormatReceiver;
    private boolean mIsBroadcastRegistered;
    private boolean mIsPhoneListenerAdded;
    private boolean mIs24HourFormat;
    private NaviEtaInfo mCurrentNaviInfo;
    private RouteLoadingDialog mRouteRequestLoadingDialog;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addTelPhoneListener();
        mTimeFormatReceiver = new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                if (Intent.ACTION_TIME_CHANGED.equals(intent.getAction()) ||
                        Intent.ACTION_LOCALE_CHANGED.equals(intent.getAction())) {
                    Logger.d(TAG, "onReceive ACTION_TIME_CHANGED or ACTION_LOCALE_CHANGED");
                    boolean is24Hour = getTimeFormatIs24Hour();
                    if (is24Hour != mIs24HourFormat) {
                        if (mBinding != null && mBinding.sceneNaviEta != null) {
                            mBinding.sceneNaviEta.refreshArriveTime();
                        }
                        mIs24HourFormat = is24Hour;
                    }
                }
            }
        };
        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_TIME_CHANGED);
        filter.addAction(Intent.ACTION_LOCALE_CHANGED);
        requireContext().registerReceiver(mTimeFormatReceiver, filter);
        mIsBroadcastRegistered = true;
        mViewModel.musicTabVisibility.set(ScreenTypeUtils.getInstance().isFullScreen() && FloatWindowReceiver.isShowMusicTab);
    }

    private void addTelPhoneListener() {
        if (ContextCompat.checkSelfPermission(requireContext(),
                Manifest.permission.READ_PHONE_STATE) != PackageManager.PERMISSION_GRANTED) {
            return;
        }
        mPhoneStateListener = new PhoneStateListener() {
            @Override
            public void onCallStateChanged(int state, String phoneNumber) {
                super.onCallStateChanged(state, phoneNumber);
                if (state == TelephonyManager.CALL_STATE_OFFHOOK) {
                    // 接通后
                    Logger.i(TAG, "电话通话状态 OFFHOOK");
                    int broadcastMode = SettingPackage.getInstance().getConfigKeyBroadcastMode();
                    mBroadCastModeBeforeCall = broadcastMode;
                    if (broadcastMode != NaviConstant.BroadcastType.BROADCAST_CONCISE) {
                        switchBroadcastMode(NaviConstant.BroadcastType.BROADCAST_CONCISE);
                    }
                } else if (state == TelephonyManager.CALL_STATE_IDLE) {
                    // 挂断后
                    Logger.i(TAG, "电话通话状态 IDLE");
                    mBroadCastModeAfterCall = SettingPackage.getInstance().
                            getConfigKeyBroadcastMode();
                    // 通话中手动切换了模式
                    if (mBroadCastModeAfterCall != NaviConstant.BroadcastType.BROADCAST_CONCISE) {
                        return;
                        // 通话中没有切换模式
                    }
                    if (mBroadCastModeBeforeCall == NaviConstant.BroadcastType.BROADCAST_CONCISE) {
                        // 如果通话前是简洁模式，则不切换
                        return;
                    }
                    switchBroadcastMode(mBroadCastModeBeforeCall);
                }
            }
        };
        TelephonyManager telephonyManager = (TelephonyManager) requireContext().
                getSystemService(Context.TELEPHONY_SERVICE);
        if (telephonyManager != null) {
            telephonyManager.listen(mPhoneStateListener, PhoneStateListener.LISTEN_CALL_STATE);
        }
        mIsPhoneListenerAdded = true;
    }

    private boolean getTimeFormatIs24Hour() {
        String timeFormat = android.provider.Settings.System.getString(
                requireContext().getContentResolver(),
                android.provider.Settings.System.TIME_12_24
        );
        return "24".equals(timeFormat);
    }

    private void switchBroadcastMode(int mode) {
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.switchBroadcastMode(mode);
        }
    }
    private ISceneCallback mSceneCallback;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_navi_guidance;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        saveSceneStatus();
    }

    /**
     * 保存场景状态
     */
    private void saveSceneStatus() {
        if (mViewModel == null) {
            Logger.e(TAG, "saveSceneStatus mViewModel is null");
            return;
        }
        HashMap<NaviSceneId, Integer> map = mViewModel.getSceneStatus();
        map.clear();
        saveNaviSceneStatus(mBinding.sklRoot, map);
        saveNaviSceneStatus(mBinding.naviSceneContainer, map);
        saveNaviSceneStatus(mBinding.sclTopContainer, map);
        saveLazySceneStatus(map);
        mViewModel.saveOverViewStatus();
    }

    private void saveNaviSceneStatus(ViewGroup root, HashMap<NaviSceneId, Integer> map) {
        for (int i = 0; i < root.getChildCount(); i ++ ) {
            View childView = root.getChildAt(i);
            if (childView == null) {
                continue;
            }
            if (childView instanceof NaviSceneBase) {
                map.put(((NaviSceneBase<?, ?>) childView).mSceneId,
                        ((NaviSceneBase<?, ?>) childView).getSceneState());
            }
        }
    }

    private void saveLazySceneStatus(HashMap<NaviSceneId, Integer> map) {
        saveLazySceneStatus(mSceneNaviViaListView, map);
        saveLazySceneStatus(mSceneNaviViaDetailView, map);
        saveLazySceneStatus(mSceneNaviControlMoreView, map);
        saveLazySceneStatus(mSceneNaviSapaView, map);
        saveLazySceneStatus(mSceneNaviSapaDetailView, map);
    }

    private void saveLazySceneStatus(NaviSceneBase naviSceneBase,
                                     HashMap<NaviSceneId, Integer> map) {
        if (naviSceneBase != null) {
            map.put(naviSceneBase.mSceneId, naviSceneBase.getSceneState());
        }
    }

    @Override
    public void onInitView() {
        setScreenIdAndCategory(mBinding.sklRoot);
        setScreenIdAndCategory(mBinding.naviSceneContainer);
        setScreenIdAndCategory(mBinding.sclTopContainer);
        mBinding.sceneNaviPreference.registerRoutePreferenceObserver("navi fragment", mViewModel);
    }

    private void setScreenIdAndCategory(ViewGroup root) {
        for (int i = 0; i < root.getChildCount(); i ++ ) {
            View childView = root.getChildAt(i);
            if (childView == null) {
                continue;
            }
            if (childView instanceof NaviSceneBase) {
                ((NaviSceneBase<?, ?>) childView).setCategory(NumberUtils.NUM_1);
                ((NaviSceneBase<?, ?>) childView).setScreenId(MapType.valueOf(mScreenId));
            }
        }
    }

    @Override
    public void onInitData() {
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.updateBroadcast();
        }

    }

    @Override
    public void onGetFragmentData() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        String naviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Logger.i(TAG, "onGetFragmentData", "naviStatus:", naviStatus);
        if (mViewModel == null) {
            Logger.e(TAG, "mViewModel is null");
            return;
        }
        // 如果当前导航状态不是导航中，则开始导航
        if (!NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
            mViewModel.startNavigation(getArguments());
            if (mCurrentNaviInfo == null) {
                startLoading();
            }
        } else {
            // 恢复导航页面数据
            mViewModel.restoreNavigationByRebuild();
            initLazyView();
        }
        mViewModel.refreshMapMode();
        mViewModel.setDefultPlateNumberAndAvoidLimitSave();
        mViewModel.initShowScene(NAVI_SCENE_CONTROL, NAVI_SCENE_TBT, NAVI_SCENE_ETA, NAVI_SCENE_TMC);
        mIs24HourFormat = getTimeFormatIs24Hour();
    }

    private void startLoading() {
        mBinding.sclTopContainer.setVisibility(GONE);
        mBinding.tlv.setVisibility(VISIBLE);
        mBinding.tlv.startLoading();
    }

    private void stopLoading() {
        mBinding.tlv.stopLoading();
        mBinding.tlv.setVisibility(GONE);
        mBinding.sclTopContainer.setVisibility(VISIBLE);
    }

    @Override
    public void onReStoreFragment() {
        Logger.i(TAG, "onReStoreFragment");
        if (null != mViewModel) {
            restoreLazySceneStatus();
            mViewModel.restoreNavigation();
            restoreSceneStatus();
            HashMap<NaviSceneId, Integer> sceneStatus = mViewModel.getSceneStatus();
            Integer status = sceneStatus.get(NaviSceneId.NAVI_SCENE_LAST_MILE);
            mViewModel.mNaviLastMileVisibility.set(status == NaviSceneBase.SCENE_STATE_SHOW ? true : false);
        }
        mIs24HourFormat = getTimeFormatIs24Hour();
    }

    private void restoreLazySceneStatus() {
        initLazyView();
        HashMap<NaviSceneId, Integer> map = mViewModel.getSceneStatus();
        restoreLazySceneStatus(mSceneNaviViaListView, map);
        restoreLazySceneStatus(mSceneNaviViaDetailView, map);
        restoreLazySceneStatus(mSceneNaviControlMoreView, map);
        restoreLazySceneStatus(mSceneNaviSapaView, map);
        restoreLazySceneStatus(mSceneNaviSapaDetailView, map);
    }

    private void restoreLazySceneStatus(NaviSceneBase naviSceneBase,
                                        HashMap<NaviSceneId, Integer> map) {
        if (naviSceneBase != null && map.containsKey(naviSceneBase.getMSceneId())) {
            Integer sceneState = map.get(naviSceneBase.getMSceneId());
            if (sceneState != null) {
                naviSceneBase.setSceneState(sceneState);
                View view = (View) naviSceneBase;
                view.setVisibility(sceneState == NaviSceneBase.SCENE_STATE_SHOW ? VISIBLE : GONE);
            }
        }
    }

    /**
     * 恢复场景状态
     * 不手动恢复，黑白模式切换后页面碰撞会有问题
     */
    private void restoreSceneStatus() {
        if (null == mViewModel) {
            Logger.e(TAG, "restoreSceneStatus mViewModel is null");
            return;
        }
        HashMap<NaviSceneId, Integer> map = mViewModel.getSceneStatus();
        if (ConvertUtils.isEmpty(map)) {
            Logger.e(TAG, "restoreSceneStatus list is null");
            return;
        }
        restoreNaviSceneStatus(mBinding.sklRoot, map);
        restoreNaviSceneStatus(mBinding.naviSceneContainer, map);
        restoreNaviSceneStatus(mBinding.sclTopContainer, map);
        mViewModel.restoreOverViewStatus();
        refreshView();
        NaviSceneManager.getInstance().restoreList();
    }

    private void restoreNaviSceneStatus(ViewGroup root, HashMap<NaviSceneId, Integer> map) {
        for (int i = 0; i < root.getChildCount(); i ++ ) {
            View childView = root.getChildAt(i);
            if (childView == null) {
                continue;
            }
            if (childView instanceof NaviSceneBase) {
                if (map.containsKey(((NaviSceneBase<?, ?>) childView).getMSceneId())) {
                    Integer sceneState = map.get(((NaviSceneBase<?, ?>) childView).getMSceneId());
                    if (sceneState != null) {
                        ((NaviSceneBase<?, ?>) childView).setSceneState(sceneState);
                    }
                }
            }
        }
    }

    private void refreshView() {
        Logger.i(TAG, "refreshView");
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.refreshView();
        }
        mBinding.sceneNaviControl.refreshView();
    }

    /**
     * 区间车速、绿波车速
     *
     * @param speedCameraInfo speed camera info
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        mBinding.sceneNaviSpeed.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    /**
     * 服务区信息
     *
     * @param sapaInfoEntity sapa info entity
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        if (mSceneNaviSapaView != null) {
            mSceneNaviSapaView.onNaviSAPAInfo(sapaInfoEntity);
        }
        mBinding.sceneNaviLanes.onShowTollGateLane(sapaInfoEntity);
    }

    /**
     * 导航信息
     *
     * @param naviEtaInfo navi eta info
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        if (mCurrentNaviInfo == null && naviEtaInfo != null) {
            stopLoading();
        }
        if (ConvertUtils.isEmpty(naviEtaInfo)) {
            return;
        }
        mCurrentNaviInfo = naviEtaInfo;
        mBinding.sceneNaviTbt.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviEta.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviTmc.onNaviInfo(naviEtaInfo);
        if (!ConvertUtils.isEmpty(naviEtaInfo.viaRemain) || mBinding.sceneNaviViaInfo.getVisibility() != GONE) {
            mBinding.sceneNaviViaInfo.onNaviInfo(naviEtaInfo);
        }
        mBinding.sceneNaviLastMile.onNaviInfo(naviEtaInfo);
    }

    /**
     * 更新路线名称
     *
     * @param routeName 路线名称
     */
    public void updateRouteName(final String routeName) {
        mBinding.stvNaviRouteName.setText(routeName);
    }

    /**
     * 导航停止
     */
    public void onNaviStop() {
        mBinding.sceneNaviCrossImage.onNaviStop();
    }

    /**
     * 路口大图
     *
     * @param isShowImage   是否显示图片
     * @param naviImageInfo 导航图片信息
     */
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        boolean isNavi = StackManager.getInstance().
                getCurrentFragment(mScreenId) instanceof NaviGuidanceFragment;
        final boolean isRealNeedShow = isShowImage && isNavi;
        Logger.i(TAG,
                "isRealNeedShow:", isRealNeedShow);
        // 如果途经点列表/底部控制栏更多/路线偏好/sapa详情页页面显示，不能执行路口大图的显示逻辑
        boolean isViaListShow = mSceneNaviViaListView != null &&
                mSceneNaviViaListView.getVisibility() == VISIBLE;
        boolean isControlMoreShow = mSceneNaviControlMoreView != null &&
                mSceneNaviControlMoreView.getVisibility() == VISIBLE;
        boolean isNaviSapaDetailShow = mSceneNaviSapaDetailView != null &&
                mSceneNaviSapaDetailView.getVisibility() == VISIBLE;
        boolean isCanShowCrossImage = !isViaListShow && !isControlMoreShow &&
                mBinding.sceneNaviPreference.getVisibility() != VISIBLE && !isNaviSapaDetailShow;
        if (!isCanShowCrossImage) {
            return;
        }
        mBinding.sceneNaviEta.onCrossImageShow(isRealNeedShow);
        mBinding.sceneNaviTbt.onCrossImageInfo(isRealNeedShow, naviImageInfo);
        mBinding.sceneNaviCrossImage.onCrossImageInfo(isRealNeedShow, naviImageInfo);
    }

    /**
     * 单独用来改变tbt进度的显示，因为有时候UI碰撞路口大图会主动消失，这时候就取消tbt进度条的显示
     *
     * @param isShowImage 是否显示图片
     */
    public void onCrossImageInfo(final boolean isShowImage) {
        mBinding.sceneNaviTbt.onCrossImageInfo(isShowImage);
    }

    /**
     * 更新路口大图进度
     *
     * @param routeRemainDist 路口大图进度
     */
    public void updateCrossProgress(final long routeRemainDist) {
        Logger.i(TAG, "updateCrossProgress", "routeRemainDist:", routeRemainDist);
        /*if (routeRemainDist <= 0) {
            mBinding.sceneNaviCrossImage.onCrossImageInfo(false, null);
            mBinding.sceneNaviTbt.onCrossImageInfo(false, null);
        }*/
        mBinding.sceneNaviCrossImage.updateCrossProgress(routeRemainDist);
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                mBinding.sceneNaviTbt.updateCrossProgress(routeRemainDist);
            }
        });
    }

    /**
     * 更新TMC灯光条（路况信息）
     *
     * @param naviTmcInfo navi tmc info
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, boolean isShowAutoAdd) {
        if (mBinding.sceneNaviTmc.getVisibility() == VISIBLE) {
            mBinding.sceneNaviTmc.setIsShowAutoAdd(isShowAutoAdd);
            mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
        }
    }

    /**
     * 转向图标信息、以及传出出入口信息
     *
     * @param info maneuver info
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        mBinding.sceneNaviTbt.onManeuverInfo(info);
        mBinding.sceneNaviEta.onManeuverInfo(info);
    }

    /**
     * 导航到达目的地
     *
     * @param traceId  trace id
     * @param naviType navi type
     */
    public void onNaviArrive(final long traceId, final int naviType) {
        mBinding.sceneNaviTbt.onNaviArrive(traceId, naviType);
    }

    /**
     * 车道线信息
     *
     * @param isShowLane 是否显示车道线
     * @param laneInfo   lane info
     */
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 沉浸态状态改变回调
     *
     * @param currentImersiveStatus current immersive status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        mBinding.sceneNaviControl.onImmersiveStatusChange(currentImersiveStatus);
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.onImmersiveStatusChange(currentImersiveStatus);
        }
        mBinding.sceneNaviContinue.onImmersiveStatusChange(currentImersiveStatus);
        mBinding.sceneNaviCrossImage.onImmersiveStatusChange(currentImersiveStatus);
    }

    /**
     * 添加场景回调
     *
     * @param sceneCallback scene callback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mSceneCallback = sceneCallback;
        mBinding.sceneNaviControl.addSceneCallback(sceneCallback);
        mBinding.sceneNaviLastMile.addSceneCallback(sceneCallback);
        mBinding.sceneNaviViaInfo.addSceneCallback(sceneCallback);
        mBinding.sceneNaviParallel.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSpeed.addSceneCallback(sceneCallback);
        mBinding.sceneNaviCrossImage.addSceneCallback(sceneCallback);
        mBinding.sceneNaviEta.addSceneCallback(sceneCallback);
        mBinding.sceneNaviLanes.addSceneCallback(sceneCallback);
        mBinding.sceneNaviPreference.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTbt.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTmc.addSceneCallback(sceneCallback);
        mBinding.sceneNaviViaArrive.addSceneCallback(sceneCallback);
        mBinding.sceneNaviChargeTip.addSceneCallback(sceneCallback);
        mBinding.sceneNaviContinue.addSceneCallback(sceneCallback);
        mBinding.sceneHandingCard.addSceneCallback(sceneCallback);
        mBinding.sceneNaviCardDetail.addSceneCallback(sceneCallback);
    }

    /**
     * 显示途经点列表
     *
     * @param isVisible 是否展示途径点列表
     */
    public void showNaviViaList(final boolean isVisible) {
        if (mSceneNaviViaListView != null) {
            mSceneNaviViaListView.showNaviViaList(isVisible);
        }
    }

    /**
     * 更新via列表状态
     *
     * @param list 途径点列表
     */
    public void updateViaListState(List<NaviViaEntity> list, boolean forceUpdate) {
        if (ConvertUtils.isEmpty(list)) {
            return;
        }
        if (mViewModel != null) {
            mViewModel.saveViaList(list);
        }
        if (mSceneNaviViaListView != null) {
            mSceneNaviViaListView.updateViaListState(list, forceUpdate);
        }
    }

    public void refreshViaInfo() {
        mBinding.sceneNaviViaInfo.refreshViaInfo();
    }

    /**
     * 途经点通过回调
     *
     * @param viaIndex via index
     */
    public void onUpdateViaPass(final long viaIndex) {
        mBinding.sceneNaviViaInfo.onUpdateViaPass(viaIndex);
    }

    /**
     * 删除途经点结果回调
     *
     * @param result result
     * @param entity entity
     */
    public void notifyDeleteViaPointResult(final boolean result, final NaviViaEntity entity) {
        if (mSceneNaviViaListView != null) {
            mSceneNaviViaListView.notifyDeleteViaPointResult(result, entity);
        }
    }

    /**
     * 跳转服务区详情页方法
     *
     * @param type           type
     * @param sapaInfoEntity sapa info entity
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        if (mSceneNaviSapaDetailView != null) {
            mSceneNaviSapaDetailView.skipNaviSapaDetailScene(type, sapaInfoEntity);
        }
    }

    public void notifyBatteryWarning(ChargeTipEntity entity) {
        mBinding.sceneNaviChargeTip.updateUi(entity);
    }

    /**
     * 导航继续
     */
    public void naviContinue() {
        mBinding.sceneNaviContinue.naviContinueByVoice();
    }

    /**
     * @param type 平行路切换类型 0:主辅路切换 1:桥上下切换
     */
    public void naviParallelSwitch(final int type) {
        mBinding.sceneNaviParallel.naviParallelSwitch(type);
    }

    /**
     * @param type 0:退出全览 1:切换全览 2:切换全览并固定
     */
    public void naviPreviewSwitch(final int type) {
        mBinding.sceneNaviControl.naviPreviewSwitch(type);
    }

    /**
     * @param isConnected isConnected
     */
    public void onNetStatusChange(boolean isConnected) {
        mBinding.sceneNaviPreference.onNetStatusChange(isConnected);
    }

    /**
     * 显示控制详情
     */
    public void showControlDetails() {
        mBinding.sceneNaviControl.showControlDetails();
    }

    public void goSearchView(final String keyWord, final int searchType) {
        final Fragment fragment;
        fragment = (Fragment) ARouter.getInstance().build(RoutePath.Search.SEARCH_RESULT_FRAGMENT)
                .navigation();
        Bundle bundle = SearchFragmentFactory.createKeywordFragment(
                AutoMapConstant.SourceFragment.FRAGMENT_ALONG_WAY,
                AutoMapConstant.SearchType.ALONG_WAY_SEARCH, keyWord, null);
        bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
        addFragment((BaseFragment) fragment, bundle, false);
        hideNaviContent();
    }

    public void goAlongWayList() {
        final Fragment fragment;
        fragment = (Fragment) ARouter.getInstance()
                .build(RoutePath.Search.ALONG_WAY_SEARCH_FRAGMENT)
                .navigation();
        Bundle bundle = new Bundle();
        bundle.putInt(NaviConstant.NAVI_CONTROL, 1);
        addFragment((BaseFragment) fragment, bundle, false);
        hideNaviContent();
    }

    public void closeSearchView() {
        mBinding.naviSceneContainer.setVisibility(VISIBLE);
    }

    public void onUpdateTMCLightBarAutoAdd(boolean isShow) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShow);
    }

    public void onUpdateElectVehicleETAInfo(final List<FyElecVehicleETAInfo> infos) {
        if (mSceneNaviViaListView != null) {
            mSceneNaviViaListView.updateElectVehicleETAInfo(infos);
        }
    }

    public void showNaviContent() {
        if (mBinding == null) {
            Logger.i(TAG, "showNaviContent mBinding is null");
            return;
        }
        Logger.i(TAG, "showNaviContent");
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.mNaviLeftContentVisibility.set(true);
        }
        mBinding.naviSceneContainer.setVisibility(VISIBLE);
        // 如果路口大图还是显示状态就继续显示
        if (mBinding.sceneNaviCrossImage.getVisibility() == VISIBLE) {
            mBinding.sceneNaviCrossImage.showLayerCross();
        }
    }

    public void hideNaviContent() {
        Logger.i(TAG, "hideNaviContent");
        // 如果路口大图是展示状态就进行隐藏
        if (mBinding.sceneNaviCrossImage.getVisibility() == VISIBLE) {
            mBinding.sceneNaviCrossImage.hideLayerCross();
        }
        if (!ConvertUtils.isNull(mViewModel)) {
            mViewModel.mNaviLeftContentVisibility.set(false);
        }
    }

    public void backToNaviFragment() {
        final BaseFragment currentFragment;
        currentFragment = StackManager.getInstance().getCurrentFragment(mScreenId);
        if (!(currentFragment instanceof NaviGuidanceFragment)) {
            closeAllFragmentUpNavi();
        }
    }

    @Override
    protected void onNewIntent(final Bundle bundle) {
        super.onNewIntent(bundle);
        final boolean clearAutoAddVia = bundle.getBoolean(NaviConstant.CLEAR_AUTO_ADD_VIA, false);
        final int isNaviControl = bundle.getInt(NaviConstant.NAVI_CONTROL, 0);
        Logger.i(TAG, "onNewIntent isNaviControl:", isNaviControl, " clearAutoAddVia:", clearAutoAddVia);
        if (isNaviControl == 1) {
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    showNaviContent();
                    if (mViewModel != null &&
                            Boolean.FALSE.equals(mViewModel.mNaviViaListVisibility.get()) &&
                            NaviPackage.getInstance().getPreviewStatus()) {
                        OpenApiHelper.exitPreview(MapType.MAIN_SCREEN_MAIN_MAP);
                    }
                }
            });
        }
        if (clearAutoAddVia && mViewModel != null) {
            mViewModel.deleteAutoAddChargeStation();
        }
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        Logger.i(TAG, "onHiddenChanged = " + hidden);
        if (!hidden) {
            if (mBinding.sceneNaviCrossImage.getVisibility() == VISIBLE) {
                mBinding.sceneNaviCrossImage.showLayerCross();
            }
            if (null != mViewModel) {
                mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
                if (mSceneNaviControlMoreView != null) {
                    mSceneNaviControlMoreView.updateBroadcast();
                    mSceneNaviControlMoreView.updateCarModel();
                }
            } else {
                Logger.i(TAG, "onHiddenChanged mViewModel is null");
            }
        } else {
            if (mBinding.sceneNaviCrossImage.getVisibility() == VISIBLE) {
                mBinding.sceneNaviCrossImage.hideLayerCross();
            }
        }
    }

    /**
     * 更新偏好设置
     */
    public void updatePreference() {
        Logger.i(TAG, "updatePreference");
        if (mBinding != null && mBinding.sceneNaviPreference != null) {
            mBinding.sceneNaviPreference.updateRoutePreference(SettingPackage.getInstance().
                    getRoutePreference());
        }
    }

    /**
     * 修改TBT面板的圆角样式
     */
    public void updateViewRadius() {
        if (mViewModel == null) {
            return;
        }
        if (Objects.equals(mViewModel.mNaviCrossImageVisibility.get(), Boolean.TRUE)) {
            mBinding.sceneNaviEta.setBackgroundResource
                    (com.sgm.navi.scene.R.drawable.bg_navi_tbt_bottom);
            return;
        }
        boolean isLastCarLine = Objects.equals(mViewModel.mNaviLanesVisibility.get(),
                Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastCarLine) {
            mBinding.sceneNaviLanes.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_bottom);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            return;
        }
        boolean isLastViaInfo = Objects.equals(mViewModel.mNaviViaInfoVisibility.get(),
                Boolean.TRUE);
        if (isLastViaInfo) {
            mBinding.sceneNaviLanes.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            return;
        }
        boolean isLastTmc = Objects.equals(mViewModel.mNaviTmcVisibility.get(), Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviLanesVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastTmc) {
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_bottom);
            return;
        }
        boolean isLastEta = Objects.equals(mViewModel.mNaviEtaVisibility.get(), Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviTmcVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviLanesVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastEta) {
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.sgm.navi.scene.R.drawable.bg_navi_tbt_bottom);
        }
    }

    public void onCurrentRoadSpeed(int speed) {
        mBinding.sceneNaviSpeed.onCurrentRoadSpeed(speed);
    }

    public void showViaDetail(boolean b) {
        if (null != mSceneNaviViaDetailView) {
            mSceneNaviViaDetailView.showViaDetail(b);
        }
    }

    public void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (null != mSceneNaviViaDetailView) {
            mSceneNaviViaDetailView.updateNewestViaPoint(naviViaEntity);
        }
    }

    public void onMeterAction() {
        mBinding.sceneNaviControl.onMeterAction();
    }

    public void onPassByClick() {
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.onPassByClick();
        }
    }

    public void initLazyView() {
        if (!mBinding.sceneNaviViaList.isInflated()) {
            assert mBinding.sceneNaviViaList.getViewStub() != null;
            mSceneNaviViaListView = (SceneNaviViaListView) mBinding.sceneNaviViaList.
                    getViewStub().inflate();
            initLazyView(mSceneNaviViaListView);
        }
        if (!mBinding.sceneNaviViaDetail.isInflated()) {
            assert mBinding.sceneNaviViaDetail.getViewStub() != null;
            mSceneNaviViaDetailView = (SceneNaviViaDetailView) mBinding.sceneNaviViaDetail.
                    getViewStub().inflate();
            initLazyView(mSceneNaviViaDetailView);
        }
        if (!mBinding.sceneNaviControlMore.isInflated()) {
            assert mBinding.sceneNaviControlMore.getViewStub() != null;
            mSceneNaviControlMoreView = (SceneNaviControlMoreView) mBinding.sceneNaviControlMore.
                    getViewStub().inflate();
            initLazyView(mSceneNaviControlMoreView);
        }
        if (!mBinding.sceneNaviSapa.isInflated()) {
            assert mBinding.sceneNaviSapa.getViewStub() != null;
            mSceneNaviSapaView = (SceneNaviSapaView) mBinding.sceneNaviSapa.getViewStub().inflate();
            initLazyView(mSceneNaviSapaView);
        }
        if (!mBinding.sceneNaviSapaDetail.isInflated()) {
            assert mBinding.sceneNaviSapaDetail.getViewStub() != null;
            mSceneNaviSapaDetailView = (SceneNaviSapaDetailView) mBinding.sceneNaviSapaDetail.
                    getViewStub().inflate();
            initLazyView(mSceneNaviSapaDetailView);
        }
    }

    private void initLazyView(NaviSceneBase naviSceneBase) {
        if (naviSceneBase != null) {
            naviSceneBase.setCategory(NumberUtils.NUM_1);
            naviSceneBase.addNaviScene();
            naviSceneBase.addSceneCallback(mSceneCallback);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mIsPhoneListenerAdded) {
            TelephonyManager telephonyManager = (TelephonyManager) requireContext().
                    getSystemService(Context.TELEPHONY_SERVICE);
            if (telephonyManager != null) {
                telephonyManager.listen(mPhoneStateListener, PhoneStateListener.LISTEN_NONE);
            }
            mPhoneStateListener = null;
        }
        mSceneCallback = null;
        if (mIsBroadcastRegistered) {
            requireContext().unregisterReceiver(mTimeFormatReceiver);
            mIsBroadcastRegistered = false;
        }
    }

    public void setViaListVisibility(boolean isVisible) {
        if (mSceneNaviViaListView != null) {
            mSceneNaviViaListView.setVisibility(isVisible ? VISIBLE : GONE);
        }
    }

    public void setViaDetailVisibility(boolean isVisible) {
        if (mSceneNaviViaDetailView != null) {
            mSceneNaviViaDetailView.setVisibility(isVisible ? VISIBLE : GONE);
        }
    }

    public void setControlMoreVisibility(boolean isVisible) {
        if (mSceneNaviControlMoreView != null) {
            mSceneNaviControlMoreView.setVisibility(isVisible ? VISIBLE : GONE);
        }
    }

    public void setSapaVisibility(boolean isVisible) {
        if (mSceneNaviSapaView != null) {
            mSceneNaviSapaView.setVisibility(isVisible ? VISIBLE : GONE);
        }
    }

    public void setSapaDetailVisibility(boolean isVisible) {
        if (mSceneNaviSapaDetailView != null) {
            mSceneNaviSapaDetailView.setVisibility(isVisible ? VISIBLE : GONE);
        }
    }

    public void onMapClick() {
        if (mSceneNaviViaListView != null &&
                mSceneNaviViaListView.getVisibility() == VISIBLE) {
            mSceneNaviViaListView.updateSceneVisible(false);
        }
        if (mSceneNaviControlMoreView != null &&
                mSceneNaviControlMoreView.getVisibility() == VISIBLE) {
            mSceneNaviControlMoreView.updateSceneVisible(false);
        }
        if (mBinding != null && mBinding.sceneNaviPreference.getVisibility() == VISIBLE) {
            NaviSceneManager.getInstance().notifySceneStateChangeReset(
                    INaviSceneEvent.SceneStateChangeType.SceneCloseState, NaviSceneId.NAVI_SCENE_PREFERENCE, true);
        }
        if (mBinding != null && mBinding.sceneNaviCardDetail.getVisibility() == VISIBLE) {
            mBinding.sceneNaviCardDetail.resetCountdown();
        }
    }

    public void onNaviInfoByViaArrived(NaviEtaInfo naviEtaInfo) {
        if (ConvertUtils.isEmpty(naviEtaInfo)) {
            return;
        }
        if (mBinding == null) {
            return;
        }
        mBinding.sceneNaviTmc.onNaviInfoByViaArrived(naviEtaInfo);
    }

    public void updateSclTopClickState(boolean isClickable) {
        if (mBinding == null) {
            return;
        }
        mBinding.sceneNaviSclTop.setClickable(isClickable);
        mBinding.sceneNaviSclTop.setEnabled(isClickable);
    }

    public void onMapSwipe() {
        if (mSceneNaviControlMoreView != null &&
                mSceneNaviControlMoreView.getVisibility() == VISIBLE) {
            mSceneNaviControlMoreView.updateSceneVisible(false);
        }
        if (mBinding != null && mBinding.sceneNaviCardDetail.getVisibility() == VISIBLE) {
            mBinding.sceneNaviCardDetail.resetCountdown();
        }
    }

    /***
     * 算路请求弹框展示
     */
    public void showProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog) && mRouteRequestLoadingDialog.isShowing()) {
            Logger.d("mRouteRequestLoadingDialog is showing");
            return;
        }
        final Context context = this.getContext();
        if (context == null) {
            return;
        }
        if (isAdded() && getActivity() != null && !getActivity().isFinishing()) {
            if (mRouteRequestLoadingDialog == null) {
                mRouteRequestLoadingDialog = new RouteLoadingDialog(context);
                mRouteRequestLoadingDialog.setDialogClickListener(mViewModel);
            }
            if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
                mRouteRequestLoadingDialog.show();
            }
        }
    }

    /***
     * 算路请求弹框关闭
     */
    public void hideProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog)) {
            mRouteRequestLoadingDialog.dismiss();
            mRouteRequestLoadingDialog = null;
        }
    }

    /**
     * 显示离线算路文言
     */
    public void showOfflineProgressUI() {
        if (!ConvertUtils.isEmpty(mRouteRequestLoadingDialog) && mRouteRequestLoadingDialog.isShowing()) {
            mRouteRequestLoadingDialog.showOfflineRouting();
        }
    }

    public void restoreHandingCardView(List<PoiInfoEntity> hangingCardPoiList,
                                       HandCardType handCardType) {
        if (mBinding != null && mBinding.sceneNaviCardDetail != null) {
            mBinding.sceneNaviCardDetail.updateUi(hangingCardPoiList, handCardType);
        }
    }

    @Override
    protected void onBackPressed() {

    }

    public void closeNavi() {
        if (mBinding != null && mBinding.sceneNaviControl != null) {
            mBinding.sceneNaviControl.closeNavi();
        }
    }
}
