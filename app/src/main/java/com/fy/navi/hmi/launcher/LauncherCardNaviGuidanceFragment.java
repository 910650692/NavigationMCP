package com.fy.navi.hmi.launcher;

import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentLauncherNaviGuidanceBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.service.define.map.MapType;
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

/**
 * Author: QiuYaWei
 * Date: 2025/2/23
 * Description: [{@link com.fy.navi.hmi.navi.NaviGuidanceFragment} 如果上面的类修改，此处可能也需要修改，此类有上面复制而来]
 */
public class LauncherCardNaviGuidanceFragment extends BaseFragment<FragmentLauncherNaviGuidanceBinding, LauncherNaviGuidanceViewModel> {
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

        mBinding.sceneNaviPreference.registerRoutePreferenceObserver("navi fragment", mViewModel);
    }

    @Override
    public void onInitData() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
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
        // TODO: 2025/3/23 lvww 导航业务逻辑修改 需要Launcher根据自己业务进行修改
        mBinding.sceneNaviCrossImage.updateCrossProgress(naviEtaInfo.getAllDist());
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
    }

    public void showNaviViaList(List<NaviViaEntity> list) {
        // TODO: 2025/3/23 此处有修改，后续你根据自己业务进行整改
        mBinding.sceneNaviViaList.updateViaListState(list);
    }
}
