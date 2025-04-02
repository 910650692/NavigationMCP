package com.fy.navi.hmi.navi;

import static android.view.View.GONE;
import static android.view.View.VISIBLE;

import com.android.utils.log.Logger;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentNaviGuidanceBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.ChargeTipEntity;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
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

import java.util.List;

public class NaviGuidanceFragment extends BaseFragment<FragmentNaviGuidanceBinding, NaviGuidanceViewModel> {
    @Override
    public int onLayoutId() {
        return R.layout.fragment_navi_guidance;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.sceneNaviControl.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviPreference.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviTbt.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviCrossImage.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSpeed.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSapa.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviLanes.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaList.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviLastMile.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaInfo.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviParkingList.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaArrive.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSapaDetail.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviPreference.registerRoutePreferenceObserver("navi fragment", mViewModel);
    }

    @Override
    public void onInitData() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        mViewModel.startNavigation(getArguments());
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
        mBinding.sceneNaviParkingList.onNaviInfo(naviEtaInfo);
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
        final boolean isRealNeedShow = isShowImage && (StackManager.getInstance().getCurrentFragment(mScreenId) instanceof NaviGuidanceFragment);
        Logger.i("onCrossImageInfo", "isRealNeedShow:" + isRealNeedShow);
        mBinding.sceneNaviCrossImage.onCrossImageInfo(isRealNeedShow, naviImageInfo);
    }

    /**
     * 更新路口大图进度
     *
     * @param routeRemainDist 路口大图进度
     */
    public void updateCrossProgress(final long routeRemainDist) {
        mBinding.sceneNaviCrossImage.updateCrossProgress(routeRemainDist);
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
        mBinding.sceneNaviViaList.addSceneCallback(sceneCallback);
        mBinding.sceneNaviParkingList.addSceneCallback(sceneCallback);
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
        mBinding.sceneNaviSearch.addSceneCallback(sceneCallback);
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
        mBinding.sceneNaviControl.naviContinue();
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
        mBinding.sceneNaviControl.onNetStatusChange(isConnected);
    }

    /**
     * 显示控制详情
     */
    public void showControlDetails() {
        mBinding.sceneNaviControl.showControlDetails();
    }

    public void goSearchView(final String keyWord, final int searchType) {
        mBinding.naviSceneContainer.setVisibility(GONE);
        mBinding.sceneNaviSearch.setVisibility(VISIBLE);
        mBinding.sceneNaviSearch.goSearchView(keyWord, searchType);
    }

    public void goAlongWayList() {
        mBinding.naviSceneContainer.setVisibility(GONE);
        mBinding.sceneNaviSearch.setVisibility(VISIBLE);
        mBinding.sceneNaviSearch.goAlongWayList();
    }

    public void closeSearchView() {
        mBinding.naviSceneContainer.setVisibility(VISIBLE);
        mBinding.sceneNaviSearch.setVisibility(GONE);
    }

    public void onUpdateTMCLightBarAutoAdd(boolean isShow) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShow);
    }

    public boolean isNeedCloseNaviChargeTipLater() {
        return mBinding.sceneNaviChargeTip.getVisibility() == VISIBLE;
    }
}
