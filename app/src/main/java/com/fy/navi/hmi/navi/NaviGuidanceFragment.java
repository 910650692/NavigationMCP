package com.fy.navi.hmi.navi;

import static android.view.View.INVISIBLE;
import static android.view.View.VISIBLE;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_CONTROL;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_ETA;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TBT;
import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TMC;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentNaviGuidanceBinding;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.search.SearchFragmentFactory;
import com.fy.navi.scene.ui.navi.ChargeTipEntity;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.FyElecVehicleETAInfo;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class NaviGuidanceFragment extends BaseFragment<FragmentNaviGuidanceBinding, NaviGuidanceViewModel> {
    private static final String TAG = "NaviGuidanceFragment";

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
        ArrayList<Integer> list = mViewModel.getSceneStatus();
        list.clear();
        list.add(mBinding.sceneNaviViaInfo.getSceneState());
        list.add(mBinding.sceneNaviLastMile.getSceneState());
        list.add(mBinding.sceneNaviParallel.getSceneState());
        list.add(mBinding.sceneNaviLanes.getSceneState());
        list.add(mBinding.sceneNaviViaList.getSceneState());
        list.add(mBinding.sceneNaviControlMore.getSceneState());
        list.add(mBinding.sceneNaviPreference.getSceneState());
        list.add(mBinding.sceneNaviTmc.getSceneState());
        list.add(mBinding.sceneNaviEta.getSceneState());
        list.add(mBinding.sceneNaviSpeed.getSceneState());
        list.add(mBinding.sceneNaviSapa.getSceneState());
        list.add(mBinding.sceneNaviCrossImage.getSceneState());
        list.add(mBinding.sceneNaviControl.getSceneState());
        list.add(mBinding.sceneNaviViaArrive.getSceneState());
        list.add(mBinding.sceneNaviSapaDetail.getSceneState());
        list.add(mBinding.sceneNaviChargeTip.getSceneState());
        list.add(mBinding.sceneNaviContinue.getSceneState());
        list.add(mBinding.sceneNaviCardDetail.getSceneState());
    }

    @Override
    public void onInitView() {
        mBinding.sceneNaviControl.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviControlMore.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviPreference.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviTbt.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviCrossImage.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSpeed.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSapa.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviLanes.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaList.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviLastMile.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaInfo.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaArrive.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSapaDetail.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneHandingCard.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviPreference.registerRoutePreferenceObserver("navi fragment", mViewModel);
    }

    @Override
    public void onInitData() {
        mBinding.sceneNaviControlMore.updateBroadcast();
    }

    @Override
    public void onGetFragmentData() {
        Logger.i(TAG, "onGetFragmentData");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        mViewModel.startNavigation(getArguments());
        mViewModel.setDefultPlateNumberAndAvoidLimitSave();
        mViewModel.initShowScene(NAVI_SCENE_CONTROL, NAVI_SCENE_TBT, NAVI_SCENE_ETA, NAVI_SCENE_TMC);
    }

    @Override
    public void onReStoreFragment() {
        Logger.i(TAG, "onReStoreFragment");
        if (null != mViewModel) {
            mViewModel.restoreNavigation();
            restoreSceneStatus();
        }
    }

    /**
     * 恢复场景状态
     * 不手动恢复，黑白模式切换后页面碰撞会有问题
     */
    private void restoreSceneStatus() {
        ArrayList<Integer> list = mViewModel.getSceneStatus();
        mBinding.sceneNaviViaInfo.setSceneState(list.get(0));
        mBinding.sceneNaviLastMile.setSceneState(list.get(1));
        mBinding.sceneNaviParallel.setSceneState(list.get(2));
        mBinding.sceneNaviLanes.setSceneState(list.get(3));
        mBinding.sceneNaviViaList.setSceneState(list.get(4));
        mBinding.sceneNaviControlMore.setSceneState(list.get(5));
        mBinding.sceneNaviPreference.setSceneState(list.get(6));
        mBinding.sceneNaviTmc.setSceneState(list.get(7));
        mBinding.sceneNaviEta.setSceneState(list.get(8));
        mBinding.sceneNaviSpeed.setSceneState(list.get(9));
        mBinding.sceneNaviSapa.setSceneState(list.get(10));
        mBinding.sceneNaviCrossImage.setSceneState(list.get(11));
        mBinding.sceneNaviControl.setSceneState(list.get(12));
        mBinding.sceneNaviViaArrive.setSceneState(list.get(13));
        mBinding.sceneNaviSapaDetail.setSceneState(list.get(14));
        mBinding.sceneNaviChargeTip.setSceneState(list.get(15));
        mBinding.sceneNaviContinue.setSceneState(list.get(16));
        mBinding.sceneNaviCardDetail.setSceneState(list.get(17));
        NaviSceneManager.getInstance().restoreList();
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
        mBinding.sceneNaviSapa.onNaviSAPAInfo(sapaInfoEntity);
        mBinding.sceneNaviLanes.onShowTollGateLane(sapaInfoEntity);
    }

    /**
     * 导航信息
     *
     * @param naviEtaInfo navi eta info
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        mBinding.sceneNaviTbt.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviEta.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviTmc.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviViaInfo.onNaviInfo(naviEtaInfo);
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
        Logger.i("crossImageDebug", "isShowImage:" + isShowImage +
                " isNaviGuidanceGragment = " + isNavi);
        final boolean isRealNeedShow = isShowImage && isNavi;
        Logger.i("crossImageDebug",
                "isRealNeedShow:" + isRealNeedShow);
        mBinding.sceneNaviCrossImage.onCrossImageInfo(isRealNeedShow, naviImageInfo);
        mBinding.sceneNaviTbt.onCrossImageInfo(isRealNeedShow, naviImageInfo);
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
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShowAutoAdd);
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
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
        mBinding.sceneNaviControlMore.onImmersiveStatusChange(currentImersiveStatus);
        mBinding.sceneNaviContinue.onImmersiveStatusChange(currentImersiveStatus);
        mBinding.sceneNaviCrossImage.onImmersiveStatusChange(currentImersiveStatus);
    }

    /**
     * 添加场景回调
     *
     * @param sceneCallback scene callback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mBinding.sceneNaviControl.addSceneCallback(sceneCallback);
        mBinding.sceneNaviControlMore.addSceneCallback(sceneCallback);
        mBinding.sceneNaviViaList.addSceneCallback(sceneCallback);
        mBinding.sceneNaviLastMile.addSceneCallback(sceneCallback);
        mBinding.sceneNaviViaInfo.addSceneCallback(sceneCallback);
        mBinding.sceneNaviParallel.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSpeed.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSapa.addSceneCallback(sceneCallback);
        mBinding.sceneNaviCrossImage.addSceneCallback(sceneCallback);
        mBinding.sceneNaviEta.addSceneCallback(sceneCallback);
        mBinding.sceneNaviLanes.addSceneCallback(sceneCallback);
        mBinding.sceneNaviPreference.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTbt.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTmc.addSceneCallback(sceneCallback);
        mBinding.sceneNaviViaArrive.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSapaDetail.addSceneCallback(sceneCallback);
        mBinding.sceneDriveReport.addSceneCallback(sceneCallback);
        mBinding.sceneNaviChargeTip.addSceneCallback(sceneCallback);
        mBinding.sceneNaviContinue.addSceneCallback(sceneCallback);
        mBinding.sceneHandingCard.addSceneCallback(sceneCallback);
        mBinding.sceneNaviCardDetail.addSceneCallback(sceneCallback);
        // 经纬度添加途经点失败的测试广播
//        ContextCompat.registerReceiver(getContext(), new BroadcastReceiver() {
//            @Override
//            public void onReceive(Context context, Intent intent) {
//                SapaInfoEntity.SAPAItem item = new SapaInfoEntity.SAPAItem();
//                item.setRemainDist(8307);
//                item.setType(1);
//                item.setName("G1503沪嘉收费站");
//                item.setPos(new GeoPoint(121.26777111111112, 31.3634025));
//                item.setSapaDetail(0);
//                item.setRemainTime(384);
//                item.setServicePOIID("");
//                item.setBuildingStatus(0);
//                SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
//                ArrayList<SapaInfoEntity.SAPAItem> list = new ArrayList<>();
//                list.add(item);
//                sapaInfoEntity.setList(list);
//                skipNaviSapaDetailScene(1, sapaInfoEntity);
//            }
//        }, new IntentFilter("shi.song"), ContextCompat.RECEIVER_EXPORTED);
    }

    /**
     * 显示途经点列表
     *
     * @param isVisible 是否展示途径点列表
     */
    public void showNaviViaList(final boolean isVisible) {
        mBinding.sceneNaviViaList.showNaviViaList(isVisible);
    }

    /**
     * 更新via列表状态
     *
     * @param list 途径点列表
     */
    public void updateViaListState(List<NaviViaEntity> list) {
        mBinding.sceneNaviViaList.updateViaListState(list);
    }

    /**
     * 途经点通过回调
     *
     * @param viaIndex via index
     */
    public void onUpdateViaPass(final long viaIndex) {
        mBinding.sceneNaviViaInfo.onUpdateViaPass(viaIndex);
        mBinding.sceneNaviViaArrive.onUpdateViaPass(viaIndex);
    }

    /**
     * 开始导航
     */
    public void startNavigation() {
        mBinding.sceneNaviViaArrive.startNavigation();
        mBinding.sceneNaviViaInfo.startNavigation();
    }

    /**
     * 删除途经点结果回调
     *
     * @param result result
     * @param entity entity
     */
    public void notifyDeleteViaPointResult(final boolean result, final NaviViaEntity entity) {
        mBinding.sceneNaviViaList.notifyDeleteViaPointResult(result, entity);
    }

    /**
     * 跳转服务区详情页方法
     *
     * @param type           type
     * @param sapaInfoEntity sapa info entity
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        mBinding.sceneNaviSapaDetail.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    public void notifyBatteryWarning(ChargeTipEntity entity) {
        mBinding.sceneNaviChargeTip.updateUi(entity);
    }

    /**
     * 行程报告回调
     *
     * @param entity entity
     */
    public void onDriveReport(final NaviDriveReportEntity entity) {
        mBinding.sceneDriveReport.onDriveReport(entity);
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
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        mBinding.sceneNaviControl.naviPreviewSwitch(type);
    }

    /**
     * @param isConnected isConnected
     */
    public void onNetStatusChange(boolean isConnected) {
        mBinding.sceneNaviPreference.onNetStatusChange(isConnected);
        mBinding.sceneNaviControlMore.onNetStatusChange(isConnected);
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
        mBinding.sceneNaviViaList.updateElectVehicleETAInfo(infos);
    }

    public void showNaviContent() {
        Logger.i(TAG, "showNaviContent");
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
        mBinding.naviSceneContainer.setVisibility(INVISIBLE);
    }

    public void backToNaviFragment() {
        final BaseFragment currentFragment;
        currentFragment = StackManager.getInstance().getCurrentFragment(mScreenId);
        if (!(currentFragment instanceof NaviGuidanceFragment)) {
            closeAllFragmentUpNavi();
            mBinding.naviSceneContainer.setVisibility(VISIBLE);
        }
    }

    @Override
    protected void onNewIntent(final Bundle bundle) {
        super.onNewIntent(bundle);
        final int isNaviControl = bundle.getInt(NaviConstant.NAVI_CONTROL, 0);
        Logger.i(TAG, "onNewIntent isNaviControl:" + isNaviControl);
        if (isNaviControl == 1) {
            showNaviContent();
        }
    }

    @Override
    public void onHiddenChanged(boolean hidden) {
        super.onHiddenChanged(hidden);
        if (!hidden) {
            if (null != mViewModel) {
                mViewModel.isRequestRouteForPlateNumberAndAvoidLimitChange();
                mBinding.sceneNaviControlMore.updateBroadcast();
            } else {
                Logger.i(TAG, "onHiddenChanged mViewModel is null");
            }
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
                    (com.fy.navi.scene.R.drawable.bg_navi_tbt_bottom);
            return;
        }
        boolean isLastCarLine = Objects.equals(mViewModel.mNaviLanesVisibility.get(),
                Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastCarLine) {
            mBinding.sceneNaviLanes.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_bottom);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            return;
        }
        boolean isLastViaInfo = Objects.equals(mViewModel.mNaviViaInfoVisibility.get(),
                Boolean.TRUE);
        if (isLastViaInfo) {
            mBinding.sceneNaviLanes.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            return;
        }
        boolean isLastTmc = Objects.equals(mViewModel.mNaviTmcVisibility.get(), Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviLanesVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastTmc) {
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_middle);
            mBinding.sceneNaviTmc.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_bottom);
            return;
        }
        boolean isLastEta = Objects.equals(mViewModel.mNaviEtaVisibility.get(), Boolean.TRUE) &&
                Objects.equals(mViewModel.mNaviTmcVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviLanesVisibility.get(), Boolean.FALSE) &&
                Objects.equals(mViewModel.mNaviViaInfoVisibility.get(), Boolean.FALSE);
        if (isLastEta) {
            mBinding.sceneNaviEta.setBackgroundResource(
                    com.fy.navi.scene.R.drawable.bg_navi_tbt_bottom);
        }
    }
}
