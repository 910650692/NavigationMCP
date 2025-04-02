package com.fy.navi.hmi.launcher;

import android.os.Bundle;
import android.view.View;

import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.ActivityLauncherSmallCardBinding;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的卡片， 注意名称已和第三方约定完成，请不要改动名字和路径]
 */
public class MapLauncherSmallCardActivity extends BaseActivity<ActivityLauncherSmallCardBinding, BaseLauncherSmallCardViewModel> implements INaviInitListener {
    private static final String TAG = "MapLauncherSmallCardActivity";
    @Override
    public void onCreateBefore() {
        mScreenId = MapType.LAUNCHER_WIDGET_MAP.name();
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (!NaviService.isMapInited) {
            LauncherManager.getInstance().startInitService();
            NaviService.registerAppInitListener(this);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        NaviService.unRegisterAppInitListener(this);
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_launcher_small_card;
    }

    @Override
    public int onFragmentId() {
        return R.id.layout_fragment;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.naviBottom.setViewModel(mViewModel);
        if (NaviService.isMapInited) {
            mViewModel.loadMapView();
        }
        Logger.i(TAG, "onInitView");
    }

    @Override
    public void onInitData() {
        Logger.i(TAG, "onInitData");
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mapScreenView;
    }

    @Override
    public void onInitFinished(boolean isSuccess) {
        mViewModel.loadMapView();
    }

    @Override
    protected void onMoveMapCenter() {
        mBinding.naviBottom.getRoot().setVisibility(View.GONE);
        mViewModel.setMapCenterInScreen(mBinding.mapScreenView.getRight());
    }

    @Override
    protected void onResetMapCenter() {
        mViewModel.resetMapCenterInScreen();
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        mBinding.sceneNaviEta.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviTmc.onNaviInfo(naviEtaInfo);
        mBinding.sceneNaviTbt.onNaviInfo(naviEtaInfo);
    }

    public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
    }

    public void updateRouteName(String routeName) {
//        mBinding.stvNaviRouteName.setText(routeName);
    }

    public void updateCruiseCameraInfo(CruiseInfoEntity cruiseInfoEntity) {
        if (cruiseInfoEntity == null) return;
        // cruiseInfoEntity.distance 单位是米，需求：数字最多3位(不含小数点)， 如999米， 12.5公里
        if (cruiseInfoEntity.distance >= 1000) {
            mBinding.cruiseLayout.tvDistance.setText(String.format(getString(R.string.format_one_point_num), cruiseInfoEntity.distance / 1000f));
            mBinding.cruiseLayout.tvDesc.setText(R.string.kilometer);
        } else {
            mBinding.cruiseLayout.tvDistance.setText(String.valueOf(cruiseInfoEntity.distance));
            mBinding.cruiseLayout.tvDesc.setText(R.string.meter);
        }
    }

    public void updateCruiseLanInfo(LaneInfoEntity laneInfoEntity) {
        mBinding.cruiseLayout.sceneLanesView.onLaneInfo(true, laneInfoEntity);
    }
}
