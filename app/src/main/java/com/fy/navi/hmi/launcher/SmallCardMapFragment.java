package com.fy.navi.hmi.launcher;

import androidx.databinding.library.baseAdapters.BR;

import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSmallCardMapBinding;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.ui.base.BaseFragment;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/27
 * Description: [小卡底图页]
 */
public class SmallCardMapFragment extends BaseFragment<FragmentSmallCardMapBinding, BaseSmallCardMapViewModel> {
    private static final String TAG = "SmallCard";
    @Override
    public int onLayoutId() {
        return R.layout.fragment_small_card_map;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mBinding.naviBottom.setViewModel(mViewModel);
        mViewModel.loadMapView();
    }

    @Override
    public void onInitData() {

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

    public IBaseScreenMapView getMapView() {
        return mBinding.mapScreenView;
    }
}
