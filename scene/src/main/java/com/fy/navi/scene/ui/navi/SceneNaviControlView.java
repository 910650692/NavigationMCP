package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_CONTROL;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviControlViewBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.SceneNaviControlImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.map.MapMode;

/**
 * 底部控制scene
 */
public class SceneNaviControlView extends NaviSceneBase<SceneNaviControlViewBinding, SceneNaviControlImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviControlView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviControlView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviControlView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_CONTROL;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_CONTROL, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_CONTROL, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_CONTROL, false);
        }
    }

    @Override
    protected SceneNaviControlViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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

    public void onImmersiveStatusChange(ImersiveStatus currentImersiveStatus) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addSceneCallback(sceneCallback);
    }

    public void updateOverview(@NaviConstant.OverviewType int overviewType) {
        switch (overviewType) {
            case NaviConstant.OverviewType.OVERVIEW_DEFAULT:
                mViewBinding.stvOverviewSwitch.setText(R.string.navi_overview_switch);
                break;
            case NaviConstant.OverviewType.OVERVIEW_SELECT:
            case NaviConstant.OverviewType.OVERVIEW_FIXED:
                mViewBinding.stvOverviewSwitch.setText(R.string.navi_overview_out);
                break;
        }
    }

    /**
     * 全览页面控制页面会缩短，这边动态调整
     * @param isShowMoreSet 是否显示更多设置
     */
    public void changeOverViewControlLength(boolean isShowMoreSet) {
        Context context = getContext();
        int dpPixels;
        ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
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

    public void updateVariation(@NaviConstant.VariationType int type) {
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

        }
    }

    public void updateBroadcast(@NaviConstant.BroadcastType int type) {
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

        }
    }

    public void updateCarModel(MapMode mode) {
        switch (mode) {
            case NORTH_2D:
                mViewBinding.stvCarHead.setText(R.string.navi_north_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2db_58);
                break;
            case UP_2D:
                mViewBinding.stvCarHead.setText(R.string.navi_up_2d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_2d_58);
                break;
            case UP_3D:
                mViewBinding.stvCarHead.setText(R.string.navi_up_3d);
                mViewBinding.sivCarHead.setBackgroundResource(R.drawable.img_navigation_3d_58);
                break;

        }
    }
}
