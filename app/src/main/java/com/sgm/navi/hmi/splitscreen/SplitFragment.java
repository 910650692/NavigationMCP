package com.sgm.navi.hmi.splitscreen;

import android.graphics.Rect;
import android.util.TypedValue;

import androidx.databinding.library.baseAdapters.BR;

import com.android.utils.DeviceUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSplitBinding;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.ui.base.BaseFragment;

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

    /***
     * 设置路口大图显示区域
     */
    public void setCrossRect() {
        mBinding.sceneNaviTbt.post(() -> {
            Rect rectTbt = new Rect();
            mBinding.sceneNaviTbt.getGlobalVisibleRect(rectTbt);
            // 这个高度和xml里面保持一致
            float dpHeight = getResources().getDimension(com.sgm.navi.ui.R.dimen.one_third_screen_cross_pic_height);
            int crossHeight = (int) TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP, dpHeight, getResources().getDisplayMetrics());
            final int tbtHeight = DeviceUtils.isCar(getContext()) ? mBinding.sceneNaviTbt.getHeight() : 0;
            Logger.d(TAG, "rectTbt:" + rectTbt.toShortString(), "height:" + mBinding.sceneNaviTbt.getHeight());
            Rect rectCross = new Rect(rectTbt.left, rectTbt.bottom + tbtHeight, rectTbt.right, rectTbt.bottom + crossHeight + tbtHeight);
            Logger.d(TAG, "CrossRect:" + rectCross.toShortString());
            mViewModel.setCrossRect(rectCross);
        });
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
}
