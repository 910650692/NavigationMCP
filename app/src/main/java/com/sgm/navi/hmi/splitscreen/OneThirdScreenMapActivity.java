package com.sgm.navi.hmi.splitscreen;

import android.content.res.Configuration;
import android.graphics.Rect;
import android.util.TypedValue;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.databinding.library.baseAdapters.BR;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityOneThirdScreenMapBinding;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.sgm.navi.ui.base.BaseActivity;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/5/21
 * Description: [ND车型：1/3屏]
 */
public class OneThirdScreenMapActivity extends BaseActivity<ActivityOneThirdScreenMapBinding, OneThirdScreenViewModel> {
    private static final String TAG = "NDOneThirdScreenMapActivity";
    private int currentUiMode;
    private Rect mPreviewRect = new Rect(0, 0, 0, 0);
    @Override
    public void onCreateBefore() {
        super.onCreateBefore();
        mScreenId = MapType.LAUNCHER_DESK_MAP.name();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_one_third_screen_map;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.loadMapView();
        mBinding.sceneNaviTbt.showOrHideGpsSign(false);
        currentUiMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        mViewModel.onConfigurationChanged(ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
    }

    @Override
    public void onInitData() {

    }

    @Nullable
    public IBaseScreenMapView getMapView() {
        if (ConvertUtils.isNull(mBinding)) {
            return null;
        }
        return mBinding.mapView;
    }

    /***
     * 获取车标屏幕中心点
     * @return
     */
    public int[] getCarSelfPosition() {
        final int screenHeight = ScreenUtils.Companion.getInstance().getRealScreenHeight(this);
        final int bottom = (int) getResources().getDimension(com.sgm.navi.ui.R.dimen.one_third_screen_car_logo_bottom);
        int[] pos = new int[2];
        pos[0] = mBinding.getRoot().getWidth() / 2;
        pos[1] = mViewModel.isOnNaviGating() ? bottom : screenHeight / 2;
        Logger.d(TAG, "pos[0]:" + pos[0], "pos[1]:" + pos[1]);
        return pos;
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
                mPreviewRect = new Rect(mBinding.mapView.getLeft(), mBinding.llGuidanceBoard.getBottom(), mBinding.mapView.getRight(), mBinding.slZhiJia.getTop());
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
            final int tbtHeight = DeviceUtils.isCar(this) ? mBinding.sceneNaviTbt.getHeight() : 0;
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
        if (ConvertUtils.isNull(nextManeuverEntity)) return false;
        if (nextManeuverEntity.isNextManeuverOffLine() && nextManeuverEntity.getNextIconResource() != -1) {
            mBinding.sivHudSou31.setBackgroundResource(
                    nextManeuverEntity.getNextIconResource());
            mBinding.stvTextNext.setText(nextManeuverEntity.getNextText());
            return true;
        } else if (!nextManeuverEntity.isNextManeuverOffLine() &&
                nextManeuverEntity.getNextIconDrawable() != null) {
            mBinding.sivHudSou31.setImageDrawable(nextManeuverEntity.getNextIconDrawable());
            mBinding.stvTextNext.setText(nextManeuverEntity.getNextText());
            return true;
        } else {
            return false;
        }
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        final int tmpUiMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
        if (currentUiMode != tmpUiMode) {
            mViewModel.onConfigurationChanged(ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
            recreate();
            Logger.i(TAG, "主题发生变化，重置UI！");
        }
        currentUiMode = tmpUiMode;
    }

    public Rect getPreviewRect() {
        return mPreviewRect;
    }
}