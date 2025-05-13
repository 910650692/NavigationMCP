package com.fy.navi.hmi.cluster.cluster_navi_info;

import static android.view.View.INVISIBLE;
import static android.view.View.VISIBLE;

import android.os.Bundle;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.cluster.ClusterNaviGuidanceViewModel;
import com.fy.navi.hmi.databinding.FragmentClusterNaviInfoBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.Objects;

public class NaviClusterGuidanceFragment extends BaseFragment<FragmentClusterNaviInfoBinding, ClusterNaviGuidanceViewModel> {
    private static final String TAG = "NaviGuidanceTwoTwoFragment";
    /**
     * 添加 newInstance 方法
     */
    public static NaviClusterGuidanceFragment newInstance() {
        return new NaviClusterGuidanceFragment();
    }
    @Override
    public int onLayoutId() {
        return R.layout.fragment_cluster_navi_info;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        Logger.d(TAG, "onSaveInstanceState");
        saveSceneStatus();
    }

    /**
     * 保存场景状态
     */
    private void saveSceneStatus() {
        Logger.d(TAG, "saveSceneStatus");
        ArrayList<Integer> list = mViewModel.getSceneStatus();
        list.clear();
        list.add(mBinding.sceneNaviViaInfo.getSceneState());
        list.add(mBinding.sceneNaviLanes.getSceneState());
        list.add(mBinding.sceneNaviTmc.getSceneState());
        list.add(mBinding.sceneNaviEta.getSceneState());
        list.add(mBinding.sceneNaviSpeed.getSceneState());
        list.add(mBinding.sceneNaviSapa.getSceneState());
        list.add(mBinding.sceneNaviCrossImage.getSceneState());
    }

    @Override
    public void onInitView() {
        mBinding.sceneNaviTbt.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviCrossImage.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSpeed.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviSapa.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviLanes.setScreenId(MapType.valueOf(mScreenId));
        mBinding.sceneNaviViaInfo.setScreenId(MapType.valueOf(mScreenId));
    }

    @Override
    public void onInitData() {
    }

    /**
     * 恢复场景状态
     * 不手动恢复，黑白模式切换后页面碰撞会有问题
     */
    private void restoreSceneStatus() {
        ArrayList<Integer> list = mViewModel.getSceneStatus();
        //显示经过途经点信息
        mBinding.sceneNaviViaInfo.setSceneState(list.get(0));
        //车道线
        mBinding.sceneNaviLanes.setSceneState(list.get(3));
        //路况条
        mBinding.sceneNaviTmc.setSceneState(list.get(7));
        //ETA 数据
        mBinding.sceneNaviEta.setSceneState(list.get(8));
        //限速
        mBinding.sceneNaviSpeed.setSceneState(list.get(9));
        //服务区
        mBinding.sceneNaviSapa.setSceneState(list.get(10));
        //路口大图
        mBinding.sceneNaviCrossImage.setSceneState(list.get(11));
        NaviSceneManager.getInstance().restoreList();
    }

    /**
     * 区间车速、绿波车速
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        //更新限速信息
        mBinding.sceneNaviSpeed.onNaviSpeedCameraInfo(speedCameraInfo);
    }

    /**
     * 服务区信息
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        //更新服务区信息
        mBinding.sceneNaviSapa.onNaviSAPAInfo(sapaInfoEntity);
        //更新车道线信息
        mBinding.sceneNaviLanes.onShowTollGateLane(sapaInfoEntity);
    }

    /**
     * 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        //更新tbt信息
        mBinding.sceneNaviTbt.onNaviInfo(naviEtaInfo);
        //更新eta信息
        mBinding.sceneNaviEta.onNaviInfo(naviEtaInfo);
        //更新路况条信息
        mBinding.sceneNaviTmc.onNaviInfo(naviEtaInfo);
        //更新途经点信息
        mBinding.sceneNaviViaInfo.onNaviInfo(naviEtaInfo);
    }

    /**
     * 更新路线名称
     */
    public void updateRouteName(final String routeName) {
        //更新路线名称
        mBinding.stvNaviRouteName.setText(routeName);
    }

    /**
     * 导航停止
     */
    public void onNaviStop() {
        //路况大图停止导航   隐藏路口大图
        mBinding.sceneNaviCrossImage.onNaviStop();
        mBinding.naviSceneContainer.setVisibility(INVISIBLE);
    }

    /**
     * 开始导航
     */
    public void startNavigation() {
        //显示经过途经点信息  开始导航
        mBinding.sceneNaviViaInfo.startNavigation();
        mBinding.naviSceneContainer.setVisibility(VISIBLE);
    }

    /**
     * 路口大图
     * @param isShowImage   是否显示图片
     * @param naviImageInfo 导航图片信息
     */
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        boolean isNavi = StackManager.getInstance().
                getCurrentFragment(mScreenId) instanceof NaviClusterGuidanceFragment;
        Logger.d("crossImageDebug", "isShowImage:" + isShowImage +
                " isNaviGuidanceGragment = " + isNavi);
        final boolean isRealNeedShow = isShowImage && isNavi;
        Logger.d("crossImageDebug",
                "isRealNeedShow:" + isRealNeedShow);
        //路况大图
        mBinding.sceneNaviCrossImage.onCrossImageInfo(isRealNeedShow, naviImageInfo);
        //TBT
        mBinding.sceneNaviTbt.onCrossImageInfo(isRealNeedShow, naviImageInfo);
    }

    /**
     * 单独用来改变tbt进度的显示，因为有时候UI碰撞路口大图会主动消失，这时候就取消tbt进度条的显示
     * @param isShowImage 是否显示图片
     */
    public void onCrossImageInfo(final boolean isShowImage) {
        Logger.d(TAG, "onCrossImageInfo", "isShowImage:" + isShowImage);
        //TBT
        mBinding.sceneNaviTbt.onCrossImageInfo(isShowImage);
    }

    /**
     * 更新路口大图进度
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
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, boolean isShowAutoAdd) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShowAutoAdd);
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
    }

    /**
     * 转向图标信息、以及传出出入口信息
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        mBinding.sceneNaviTbt.onManeuverInfo(info);
        mBinding.sceneNaviEta.onManeuverInfo(info);
    }

    /**
     * 导航到达目的地
     */
    public void onNaviArrive(final long traceId, final int naviType) {
        mBinding.sceneNaviTbt.onNaviArrive(traceId, naviType);
    }

    /**
     * 车道线信息
     */
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
    }

    /**
     * 沉浸态状态改变回调
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        mBinding.sceneNaviCrossImage.onImmersiveStatusChange(currentImersiveStatus);
    }

    /**
     * 添加场景回调
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mBinding.sceneNaviViaInfo.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSpeed.addSceneCallback(sceneCallback);
        mBinding.sceneNaviSapa.addSceneCallback(sceneCallback);
        mBinding.sceneNaviCrossImage.addSceneCallback(sceneCallback);
        mBinding.sceneNaviEta.addSceneCallback(sceneCallback);
        mBinding.sceneNaviLanes.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTbt.addSceneCallback(sceneCallback);
        mBinding.sceneNaviTmc.addSceneCallback(sceneCallback);
    }

    /**
     * 途经点通过回调
     * @param viaIndex via index
     */
    public void onUpdateViaPass(final long viaIndex) {
        mBinding.sceneNaviViaInfo.onUpdateViaPass(viaIndex);
    }


    public void onUpdateTMCLightBarAutoAdd(boolean isShow) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShow);
    }



    public void showNaviContent() {
        Logger.i(TAG, "showNaviContent");
        mBinding.naviSceneContainer.setVisibility(VISIBLE);
        // 如果路口大图还是显示状态就继续显示
        if (mBinding.sceneNaviCrossImage.getVisibility() == VISIBLE) {
            mBinding.sceneNaviCrossImage.showLayerCross();
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
