package com.sgm.navi.scene.ui.navi.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.sgm.navi.scene.databinding.SubcomponentLanesBinding;
import com.sgm.navi.scene.impl.navi.common.SceneCommonStruct;
import com.sgm.navi.scene.impl.navi.common.SceneEnumRes;
import com.sgm.navi.ui.view.SkinConstraintLayout;

//车道线子组件
public class SubcomponentLanes extends SkinConstraintLayout {
    private SubcomponentLanesBinding mBinding;

    public SubcomponentLanes(@NonNull final Context context) {
        super(context);
        initView(context);
    }

    public SubcomponentLanes(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public SubcomponentLanes(@NonNull final Context context, @Nullable final AttributeSet attrs,
                             final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    /**
     * @param context context
     */
    private void initView(final Context context) {
        mBinding = SubcomponentLanesBinding.inflate(
                LayoutInflater.from(context), this, true);
    }

    /**
     * 设置是否高亮
     * @param isVisible isVisible
     */
    public void setVisibleHighlight(final boolean isVisible) {
    }

    /**
     * 设置车道线箭头
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveDefaultArrow(final SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableDefaultLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置车道线底部公交专用等提示
     * @param laneBottomAction laneBottomAction
     */
    public void setBackgroundLanesDriveDefaultBottom(
            final SceneCommonStruct.TimeLaneBottomAction laneBottomAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(
                    SceneEnumRes.getDrawableTimeLaneType(laneBottomAction).getDayDrawableId());
        }
    }

    /**
     * 设置推荐车道线箭头
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveRecommendArrow(
            final SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableRecommendLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置分时车道线箭头
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveTimeArrow(final SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableTimeLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置分时推荐车道线箭头
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveRecommendTimeArrow(
            final SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableRecommendTimeLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置推荐分时车道线底部公交专用等提示
     * @param laneBottomAction laneBottomAction
     */
    public void setBackgroundLanesDriveRecommendBottom(
            final SceneCommonStruct.TimeLaneBottomAction laneBottomAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableRecommendTimeLane(laneBottomAction).getDayDrawableId());
        }
    }

    /**
     * 设置普通场景
     */
    public void sceneCommonArrow() {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setVisibility(View.VISIBLE);
        }
    }

    /**
     * 设置公交专用等场景
     */
    public void sceneBottom() {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setVisibility(View.VISIBLE);
        }
    }
}

