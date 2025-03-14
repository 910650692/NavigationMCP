package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_LANES;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviLanesViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviLanesImpl;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.scene.impl.navi.common.NaviUiUtil;

/**
 * 车道线
 */
public class SceneNaviLanesView extends NaviSceneBase<SceneNaviLanesViewBinding, SceneNaviLanesImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviLanesView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviLanesView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviLanesView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_LANES;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_LANES, this);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {

    }

    @Override
    protected SceneNaviLanesViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviLanesViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviLanesImpl initSceneImpl() {
        return new SceneNaviLanesImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviLanes(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onLaneInfo(isShowLane, laneInfo);
        }
    }

    public void onShowTollGateLane(SapaInfoEntity tollGateInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onShowTollGateLane(tollGateInfo);
        }
    }

    public void setVisibleLaneInfo(boolean isVisible) {
        //<!--车道线-->
        Logger.d(TAG, "SceneNaviLanesView setVisibleLaneInfo：isVisible：" + isVisible);
        setVisibility(isVisible ? VISIBLE : GONE);
    }

    /**
     * 小设置车道线是否可见
     */
    public void setVisibleLaneDefault(int index, boolean isVisible) {
        Logger.d(TAG, "SceneNaviLanesView setVisibleLaneDefault index：" + index + ",isVisible：" + isVisible);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 1:
                mViewBinding.tlrResources16.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 2:
                mViewBinding.tlrResources17.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 3:
                mViewBinding.tlrResources18.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 4:
                mViewBinding.tlrResources19.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 5:
                mViewBinding.tlrResources110.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 6:
                mViewBinding.tlrResources111.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            case 7:
                mViewBinding.tlrResources1.setVisibility(isVisible ? VISIBLE : GONE);
                break;
            default:
                break;
        }
    }

    /**
     * 设置分时推荐车道线箭头
     */
    public void setBackgroundLanesDriveRecommendTimeArrow(int index, SceneCommonStruct.LaneAction LaneAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveRecommendTimeArrow index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendTimeArrow(LaneAction_value);
                break;
            default:
                break;
        }
    }

    /**
     * 设置推荐车道线箭头
     */
    public void setBackgroundLanesDriveRecommendArrow(int index, SceneCommonStruct.LaneAction LaneAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveRecommendArrow index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendArrow(LaneAction_value);
                break;
            default:
                break;
        }
    }

    /**
     * 设置分时车道线箭头
     */
    public void setBackgroundLanesDriveTimeArrow(int index, SceneCommonStruct.LaneAction LaneAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveTimeArrow index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveTimeArrow(LaneAction_value);
                break;
        }
    }

    /**
     * 设置车道线箭头
     */
    public void setBackgroundLanesDriveDefaultArrow(int index, SceneCommonStruct.LaneAction LaneAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveDefaultArrow index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveDefaultArrow(LaneAction_value);
                break;
            default:
                break;
        }
    }

    /**
     * 设置公交专用等场景
     */
    public void sceneBottom(int index) {
        Logger.d(TAG, "SceneNaviLanesView sceneBottom index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.sceneBottom();
                break;
            case 1:
                mViewBinding.tlrResources16.sceneBottom();
                break;
            case 2:
                mViewBinding.tlrResources17.sceneBottom();
                break;
            case 3:
                mViewBinding.tlrResources18.sceneBottom();
                break;
            case 4:
                mViewBinding.tlrResources19.sceneBottom();
                break;
            case 5:
                mViewBinding.tlrResources110.sceneBottom();
                break;
            case 6:
                mViewBinding.tlrResources111.sceneBottom();
                break;
            case 7:
                mViewBinding.tlrResources1.sceneBottom();
                break;
            default:
                break;
        }
    }

    /**
     * 设置推荐分时车道线底部公交专用等提示
     */
    public void setBackgroundLanesDriveRecommendBottom(int index, SceneCommonStruct.TimeLaneBottomAction TimeLaneBottomAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveRecommendBottom index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendBottom(TimeLaneBottomAction_value);
                break;
            default:
                break;
        }
    }

    /**
     * 设置是否高亮
     */
    public void setVisibleHighlight(int index, boolean isVisible) {
        Logger.d(TAG, "SceneNaviLanesView setVisibleHighlight index：" + index + ",isVisible：" + isVisible);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setVisibleHighlight(isVisible);
                break;
            case 1:
                mViewBinding.tlrResources16.setVisibleHighlight(isVisible);
                break;
            case 2:
                mViewBinding.tlrResources17.setVisibleHighlight(isVisible);
                break;
            case 3:
                mViewBinding.tlrResources18.setVisibleHighlight(isVisible);
                break;
            case 4:
                mViewBinding.tlrResources19.setVisibleHighlight(isVisible);
                break;
            case 5:
                mViewBinding.tlrResources110.setVisibleHighlight(isVisible);
                break;
            case 6:
                mViewBinding.tlrResources111.setVisibleHighlight(isVisible);
                break;
            case 7:
                mViewBinding.tlrResources1.setVisibleHighlight(isVisible);
                break;
            default:
                break;
        }
    }

    /**
     * 设置车道线底部公交专用等提示
     */
    public void setBackgroundLanesDriveDefaultBottom(int index, SceneCommonStruct.TimeLaneBottomAction TimeLaneBottomAction_value) {
        Logger.d(TAG, "SceneNaviLanesView setBackgroundLanesDriveDefaultBottom index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveDefaultBottom(TimeLaneBottomAction_value);
                break;
            default:
                break;
        }
    }

    /**
     * 设置普通场景
     */
    public void sceneCommonArrow(int index) {
        Logger.d(TAG, "SceneNaviLanesView sceneCommonArrow index：" + index);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.sceneCommonArrow();
                break;
            case 1:
                mViewBinding.tlrResources16.sceneCommonArrow();
                break;
            case 2:
                mViewBinding.tlrResources17.sceneCommonArrow();
                break;
            case 3:
                mViewBinding.tlrResources18.sceneCommonArrow();
                break;
            case 4:
                mViewBinding.tlrResources19.sceneCommonArrow();
                break;
            case 5:
                mViewBinding.tlrResources110.sceneCommonArrow();
                break;
            case 6:
                mViewBinding.tlrResources111.sceneCommonArrow();
                break;
            case 7:
                mViewBinding.tlrResources1.sceneCommonArrow();
                break;
            default:
                break;
        }
    }

    public void sceneLaneInfoDefault() {
        Logger.d(TAG, "SceneNaviLanesView sceneLaneInfoDefault：");
        setVisibility(VISIBLE);
        NaviUiUtil.hideView(mViewBinding.tlrResources161, mViewBinding.tlrResources16, mViewBinding.tlrResources17,
                mViewBinding.tlrResources18, mViewBinding.tlrResources19, mViewBinding.tlrResources110,
                mViewBinding.tlrResources111, mViewBinding.tlrResources1);
    }
}
