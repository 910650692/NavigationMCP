package com.fy.navi.hmi.navi;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentNaviGuidanceBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.ui.base.BaseFragment;

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
        mBinding.sceneNaviControl.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviPreference.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviTbt.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviCrossImage.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviSpeed.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviSapa.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviLanes.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviViaList.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviLastMile.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviViaInfo.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviParkingList.setScreenId(MapTypeId.valueOf(mScreenId));
        mBinding.sceneNaviViaArrive.setScreenId(MapTypeId.valueOf(mScreenId));

        mBinding.sceneNaviPreference.registerRoutePreferenceObserver("navi fragment", mViewModel);
    }

    @Override
    public void onInitData() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        mViewModel.startNavigation(getArguments());
    }

    public void onNaviSpeedCameraInfo(SpeedOverallEntity speedCameraInfo) {
        mBinding.sceneNaviSpeed.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        mBinding.sceneNaviSapa.onNaviSAPAInfo(sapaInfoEntity);
        mBinding.sceneNaviLanes.onShowTollGateLane(sapaInfoEntity);
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        mBinding.sceneNaviTbt.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviEta.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviTmc.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviViaInfo.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviLastMile.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviParkingList.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviCrossImage.onNaviInfo(naviEtaInfo);
    }

    public void updateRouteName(String routeName) {
        mBinding.stvNaviRouteName.setText(routeName);
    }

    public void onNaviStop() {
        mBinding.sceneNaviCrossImage.onNaviStop();
    }

    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        mBinding.sceneNaviCrossImage.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
    }

    public void onManeuverInfo(NaviManeuverInfo info) {
        mBinding.sceneNaviTbt.onManeuverInfo(info);
        mBinding.sceneNaviEta.onManeuverInfo(info);
    }

    public void onNaviArrive(long traceId, int naviType) {
        mBinding.sceneNaviTbt.onNaviArrive(traceId, naviType);
    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
    }

    public void onImmersiveStatusChange(ImersiveStatus currentImersiveStatus) {
        mBinding.sceneNaviControl.onImmersiveStatusChange(currentImersiveStatus);
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
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
    }

    public void updateViaListState(boolean isExpand) {
        mBinding.sceneNaviViaList.updateViaListState(isExpand);
    }

    public void showNaviViaList(List<NaviViaEntity> list) {
        mBinding.sceneNaviViaList.showNaviViaList(list);
    }

    public void onUpdateViaPass(long viaIndex) {
        mBinding.sceneNaviViaInfo.onUpdateViaPass(viaIndex);
        mBinding.sceneNaviViaArrive.onUpdateViaPass(viaIndex);
    }

    public void startNavigation() {
        mBinding.sceneNaviViaArrive.startNavigation();
        mBinding.sceneNaviViaInfo.startNavigation();
    }

    public void notifyDeleteViaPointResult(boolean result, NaviViaEntity entity) {
        mBinding.sceneNaviViaList.notifyDeleteViaPointResult(result, entity);
    }
}
