package com.sgm.navi.hmi.splitscreen;

import android.graphics.Rect;

import androidx.databinding.library.baseAdapters.BR;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSplitBinding;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.android.utils.screen.ScreenTypeUtils;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.StackManager;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/12
 * Description: [在这里描述文件功能]
 */
public class SplitFragment extends BaseFragment<FragmentSplitBinding, SplitViewModel> {
    private final String TAG = "SplitFragment";
    private Rect mPreviewRect = new Rect(0, 0, 0, 0);

    @Override
    public int onLayoutId() {
        return R.layout.fragment_split;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        if (!ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            closeFragment(true);
        } else {
            mViewModel.initView();
            mBinding.sceneNaviTbt.showOrHideGpsSign(false);
            BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name());
            if (baseActivity instanceof MapActivity) {
                ((MapActivity)baseActivity).notifyStepOneThirdScreen();
            }
        }
    }

    @Override
    public void onInitData() {

    }

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        mBinding.sceneNaviEta.onNaviInfo(naviETAInfo);
        mBinding.sceneNaviTbt.onNaviInfo(naviETAInfo);
        mBinding.sceneNaviTmc.onNaviInfo(naviETAInfo);
        calculatePreviewRect();
    }

    @SuppressWarnings("SWAPPED_ARGUMENTS")
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfoEntity);
    }

    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo, boolean isShowAutoAdd) {
        mBinding.sceneNaviTmc.setIsShowAutoAdd(isShowAutoAdd);
        mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
    }

    public void calculatePreviewRect() {
        if (mPreviewRect.right == 0) {
            mBinding.sceneNaviTbt.post(() -> {
                mPreviewRect = new Rect(mBinding.sceneNaviTbt.getLeft(), mBinding.llGuidanceBoard.getBottom(), mBinding.sceneNaviTbt.getRight(), mBinding.slZhiJia.getTop());
                Logger.i(TAG, "calculatePreviewRect-Success!", "mPreviewRect:" + mPreviewRect);
            });
        }
    }

    public void onManeuverInfo(NaviManeuverInfo nextManeuverEntity) {
        mBinding.sceneNaviTbt.onManeuverInfo(nextManeuverEntity);
        mBinding.sceneNaviEta.onManeuverInfo(nextManeuverEntity);
    }

    public boolean onNextManeuverInfo(NextManeuverEntity nextManeuverEntity) {
        return false;
    }

    public Rect getPreviewRect() {
        return mPreviewRect;
    }

    private static final class InstanceHolder {
        private static final SplitFragment instance = new SplitFragment();
    }

    public static SplitFragment getInstance() {
        return SplitFragment.InstanceHolder.instance;
    }
}
