package com.fy.navi.scene.ui.navi;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviControlViewBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.SceneNaviControlImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapMode;

/**
 * 底部控制scene
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviControlView extends NaviSceneBase<SceneNaviControlViewBinding, SceneNaviControlImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ImersiveStatus mImersiveStatus;

    public SceneNaviControlView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviControlView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviControlView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected SceneNaviControlViewBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
        return SceneNaviControlViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviControlImpl initSceneImpl() {
        return new SceneNaviControlImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviControl(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_CONTROL;
    }

    @Override
    protected void init() {
        super.init();
        initVehicleType();
    }

    @Override
    public void show() {
        super.show();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.initTimer();
        }
    }

    /**
     * @param currentImersiveStatus status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (currentImersiveStatus != mImersiveStatus) {
            mImersiveStatus = currentImersiveStatus;
        } else {
            return;
        }
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    /**
     * @param overviewType type
     */
    public void updateOverview(@NaviConstant.OverviewType final int overviewType) {
        switch (overviewType) {
            case NaviConstant.OverviewType.OVERVIEW_DEFAULT:
                mViewBinding.stvOverviewSwitch.setText(R.string.navi_overview_switch);
                break;
            case NaviConstant.OverviewType.OVERVIEW_SELECT:
            case NaviConstant.OverviewType.OVERVIEW_FIXED:
                mViewBinding.stvOverviewSwitch.setText(R.string.navi_overview_out);
                break;
            default:
                break;
        }
    }

    /**
     * 全览页面控制页面会缩短，这边动态调整
     * @param isShowMoreSet 是否显示更多设置
     */
    public void changeOverViewControlLength(final boolean isShowMoreSet) {
        final Context context = getContext();
        final int dpPixels;
        if (mViewBinding.sclSettings == null) {
            Logger.e(TAG, "changeOverViewControlLength: mViewBinding.sclSettings is null");
            return;
        }
        final ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                mViewBinding.sclSettings.getLayoutParams();
        if (isShowMoreSet) {
            dpPixels = context.getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_455);
        } else {
            dpPixels = context.getResources().getDimensionPixelSize(com.fy.navi.ui.R.dimen.dp_302);
        }
        params.width = dpPixels;
        mViewBinding.sclSettings.setLayoutParams(params);
        invalidate();
    }

    /**
     * @param type type
     */
    public void updateVariation(@NaviConstant.VariationType final int type) {
        switch (type) {
            case NaviConstant.VariationType.VARIATION_MUTE:
                mViewBinding.stvVariation.setText(R.string.navi_mute);
                mViewBinding.sivVariation.setBackgroundResource(
                        R.drawable.img_mute_broadcast_black_58);
                break;
            case NaviConstant.VariationType.VARIATION_BROADCAST:
                mViewBinding.stvVariation.setText(R.string.navi_broadcast);
                mViewBinding.sivVariation.setBackgroundResource(
                        R.drawable.img_succinct_broadcast_black_58);
                break;
            case NaviConstant.VariationType.VARIATION_SELECT:
                mViewBinding.stvVariation.setText(R.string.navi_overview_fixed);
                mViewBinding.sivVariation.setBackgroundResource(
                        R.drawable.img_off_look_through_black_58);
                break;
            default:
                break;

        }
    }

    /**
     * @param type type
     */
    public void updateBroadcast(@NaviConstant.BroadcastType final int type) {
        switch (type) {
            case NaviConstant.BroadcastType.BROADCAST_CONCISE:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_concise);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_succinct_58);
                break;
            case NaviConstant.BroadcastType.BROADCAST_DETAIL:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_detail);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_detailed_58);
                break;
            case NaviConstant.BroadcastType.BROADCAST_MINIMALISM:
                mViewBinding.stvBroadcast.setText(R.string.navi_broadcast_minimalism);
                mViewBinding.sivBroadcast.setBackgroundResource(R.drawable.img_broadcast_brief_58);
                break;
            default:
                break;

        }
    }

    /**
     * @param mode 车头朝向类型
     */
    public String updateCarModel(final MapMode mode) {
        String carModel = "";
        switch (mode) {
            case NORTH_2D:
                carModel = getContext().getString(R.string.navi_north_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2db_58);
                break;
            case UP_2D:
                carModel = getContext().getString(R.string.navi_up_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2d_58);
                break;
            case UP_3D:
                carModel = getContext().getString(R.string.navi_up_3d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_3d_58);
                break;
            default:
                break;
        }
        mViewBinding.stvCarHead.setText(carModel);
        return carModel;
    }

    /**
     * 导航继续
     */
    public void naviContinue() {
        mScreenViewModel.naviContinue();
    }

    /**
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        mScreenViewModel.naviPreviewSwitch(type);
    }

    /**
     * 显示控制详情
     */
    public void showControlDetails() {
        mScreenViewModel.moreSetup();
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    private void initVehicleType() {
        int vehicleType = mScreenViewModel.getCarType();
        // 纯油
        if (vehicleType == 0) {
            mViewBinding.sivCharge.setBackground(getContext().getDrawable
                    (R.drawable.img_lavatory_58));
            mViewBinding.stvCharge.setText(R.string.st_quick_search_lavatory);
            mViewBinding.sivLavatory.setBackground(getContext().getDrawable(
                    R.drawable.img_end_point_58));
            mViewBinding.stvLavatory.setText(R.string.navi_along_parking);
            mViewBinding.sivService.setBackground(getContext().getDrawable(
                    R.drawable.img_service_area_rim_58));
            mViewBinding.stvService.setText(R.string.navi_along_service);
            // 纯电
        } else if (vehicleType == 1) {
            mViewBinding.sivOil.setBackground(getContext().getDrawable(
                    R.drawable.img_lightning_58));
            mViewBinding.stvOil.setText(R.string.st_quick_search_charge);
            mViewBinding.sivCharge.setBackground(getContext().getDrawable
                    (R.drawable.img_lavatory_58));
            mViewBinding.stvCharge.setText(R.string.st_quick_search_lavatory);
            mViewBinding.sivLavatory.setBackground(getContext().getDrawable(
                    R.drawable.img_end_point_58));
            mViewBinding.stvLavatory.setText(R.string.navi_along_parking);
            mViewBinding.sivService.setBackground(getContext().getDrawable(
                    R.drawable.img_service_area_rim_58));
            mViewBinding.stvService.setText(R.string.navi_along_service);
        }
        // 默认写的是混动
    }

    /**
     * @param isConnected isConnected
     */
    public void onNetStatusChange(boolean isConnected) {
        Logger.i(TAG, "onNetStatusChange isConnected:" + isConnected);
        mScreenViewModel.refreshRoute();
    }
}
