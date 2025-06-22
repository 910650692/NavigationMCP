package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.ConvertUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.databinding.SceneNaviControlViewBinding;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.navi.SceneNaviControlImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;

/**
 * 底部控制scene
 *
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviControlView extends NaviSceneBase<SceneNaviControlViewBinding, SceneNaviControlImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CONTROL;
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
    }

    @Override
    public void show() {
        super.show();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.initTimer();
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.cancelTimer();
        }
    }

    @Override
    public void close() {
        super.close();
        if (!ConvertUtils.isNull(mScreenViewModel)) {
            mScreenViewModel.cancelTimer();
        }
    }

    public void refreshView() {
        if (mScreenViewModel != null) {
            mScreenViewModel.refreshView();
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
     *
     * @param isShowMoreSet 是否显示更多设置
     */
    public void changeOverViewControlLength(final boolean isShowMoreSet) {
        Logger.i(TAG, "changeOverViewControlLength", "isShowMoreSet:", isShowMoreSet,
                "vis:", (mScreenViewModel != null &&
                Boolean.TRUE.equals(mScreenViewModel.getGroupMoreSetupField().get())));
        final Context context = getContext();
        final int dpPixels;
        if (mViewBinding != null && mViewBinding.sclSettings == null) {
            Logger.e(TAG, "changeOverViewControlLength: mViewBinding.sclSettings is null");
            return;
        }
        final ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                mViewBinding.sclSettings.getLayoutParams();
        Logger.i(TAG, "originWidth:", params.width);
        if (isShowMoreSet) {
            dpPixels = context.getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.control_view_normal_width);
        } else {
            dpPixels = context.getResources().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.control_view_short_width);
        }
        Logger.i(TAG, "dpPixels:", dpPixels);
        params.width = dpPixels;
        mViewBinding.sclSettings.setLayoutParams(params);
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
     * 导航继续
     */
    public void naviContinue() {
        if (mScreenViewModel == null) {
            return;
        }
        mScreenViewModel.naviContinue();
    }

    /**
     * @param type 0:退出全览 1:切换全览
     */
    public void naviPreviewSwitch(final int type) {
        if (mScreenViewModel == null) {
            return;
        }
        mScreenViewModel.naviPreviewSwitch(type);
    }

    /**
     * 显示控制条更多
     */
    public void showControlDetails() {
        if (mScreenViewModel == null) {
            return;
        }
        mScreenViewModel.moreSetup();
    }

    public void onMeterAction() {
        ToastUtils.Companion.getInstance().showCustomToastView(AppCache.getInstance().getMContext().getString(R.string.navi_meter_start), 3000);
        if (mScreenViewModel == null) {
            return;
        }
        mScreenViewModel.clickToShowOverview();
        mScreenViewModel.onFixedOverView();
    }
}
