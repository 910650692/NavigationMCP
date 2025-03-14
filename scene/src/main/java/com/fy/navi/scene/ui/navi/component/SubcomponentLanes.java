package com.fy.navi.scene.ui.navi.component;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SubcomponentLanesBinding;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.impl.navi.common.SceneEnumRes;
import com.fy.navi.ui.view.SkinConstraintLayout;

//车道线子组件
public class SubcomponentLanes extends SkinConstraintLayout {
    private SubcomponentLanesBinding mBinding;

    public SubcomponentLanes(@NonNull Context context) {
        super(context);
        initView(context);
    }

    public SubcomponentLanes(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
        initView(context);
    }

    public SubcomponentLanes(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        initView(context);
    }

    private void initView(Context context) {
        mBinding = SubcomponentLanesBinding.inflate(LayoutInflater.from(context), this, true);
    }

    /**
     * 设置是否高亮
     */
    public void setVisibleHighlight(boolean isVisible) {
    }

    /**
     * 设置车道线箭头
     */
    public void setBackgroundLanesDriveDefaultArrow(SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableDefaultLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置车道线底部公交专用等提示
     */
    public void setBackgroundLanesDriveDefaultBottom(SceneCommonStruct.TimeLaneBottomAction laneBottomAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableTimeLaneType(laneBottomAction).getDayDrawableId());
        }
    }

    /**
     * 设置推荐车道线箭头
     */
    public void setBackgroundLanesDriveRecommendArrow(SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableRecommendLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置分时车道线箭头
     */
    public void setBackgroundLanesDriveTimeArrow(SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableTimeLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置分时推荐车道线箭头
     */
    public void setBackgroundLanesDriveRecommendTimeArrow(SceneCommonStruct.LaneAction laneAction) {
        if (mBinding != null) {
            mBinding.sivDriveWayArrow.setBackgroundResource(SceneEnumRes.getDrawableRecommendTimeLane(laneAction).getDayDrawableId());
        }
    }

    /**
     * 设置推荐分时车道线底部公交专用等提示
     */
    public void setBackgroundLanesDriveRecommendBottom(SceneCommonStruct.TimeLaneBottomAction laneBottomAction) {
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

