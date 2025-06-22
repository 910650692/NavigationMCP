package com.sgm.navi.hmi.launcher;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityLauncherSmallCardBinding;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.ui.base.BaseActivity;

/**
 * Author: QiuYaWei
 * Date: 2025/2/13
 * Description: [车机launcher桌面上显示的卡片， 注意名称已和第三方约定完成，请不要改动名字和路径]
 */
public class MapLauncherSmallCardActivity extends BaseActivity<ActivityLauncherSmallCardBinding, LauncherSmallCardViewModel> {
    private static final String TAG = "MapLauncherSmallCardActivity";

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.LAUNCHER_WIDGET_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_launcher_small_card;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.naviBottom.setViewModel(mViewModel);
    }

    @Override
    public void onInitData() {
        Logger.i(TAG, "onInitData");
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mapScreenView;
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

    public void updateCruiseLanInfo(final boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mBinding.cruiseLayout.sceneLanesView.onLaneInfo(isShowLane, laneInfoEntity);
    }
}
