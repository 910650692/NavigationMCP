package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.databinding.SceneNaviLanesViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviLanesImpl;
import com.sgm.navi.scene.impl.navi.common.NaviUiUtil;
import com.sgm.navi.scene.impl.navi.common.SceneCommonStruct;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;

/**
 * 车道线
 *
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviLanesView extends NaviSceneBase<SceneNaviLanesViewBinding, SceneNaviLanesImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_LANES;
    private boolean mIsShowLane = false;

    public SceneNaviLanesView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviLanesView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviLanesView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                              final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_LANES;
    }

    @Override
    protected SceneNaviLanesViewBinding createViewBinding(final LayoutInflater inflater,
                                                          final ViewGroup viewGroup) {
        return SceneNaviLanesViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    public void show() {
        Logger.d(TAG, "mIsShowLane：", mIsShowLane);
        if (mIsShowLane) {
            super.show();
        } else {
            mScreenViewModel.updateSceneVisible(false);
        }
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

    /**
     * @param isShowLane 是否显示车道
     * @param laneInfo   车道信息
     */
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        mIsShowLane = isShowLane;
        if (mScreenViewModel != null) {
            mScreenViewModel.onLaneInfo(isShowLane, laneInfo);
        }
    }

    /**
     * @param tollGateInfo 车道信息
     */
    public void onShowTollGateLane(final SapaInfoEntity tollGateInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onShowTollGateLane(tollGateInfo);
        }
    }

    /**
     * 小设置车道线是否可见
     *
     * @param index     index
     * @param isVisible 是否显示
     */
    public void setVisibleLaneDefault(final int index, final boolean isVisible) {
        Logger.d(TAG, "SceneNaviLanesView setVisibleLaneDefault index：", index,
                ",isVisible：", isVisible);
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

    public void setLaneExtenBackground(final int index, final int extenType) {
        Logger.d(TAG, "setLaneExtenBackground index：", index,
                ",extenType：", extenType);
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setExtenBackground(extenType);
                break;
            case 1:
                mViewBinding.tlrResources16.setExtenBackground(extenType);
                break;
            case 2:
                mViewBinding.tlrResources17.setExtenBackground(extenType);
                break;
            case 3:
                mViewBinding.tlrResources18.setExtenBackground(extenType);
                break;
            case 4:
                mViewBinding.tlrResources19.setExtenBackground(extenType);
                break;
            case 5:
                mViewBinding.tlrResources110.setExtenBackground(extenType);
                break;
            case 6:
                mViewBinding.tlrResources111.setExtenBackground(extenType);
                break;
            case 7:
                mViewBinding.tlrResources1.setExtenBackground(extenType);
                break;
            default:
                break;
        }
    }

    /**
     * 设置分时推荐车道线箭头
     *
     * @param index      index
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveRecommendTimeArrow(final int index, final SceneCommonStruct.LaneAction laneAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", laneAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendTimeArrow(laneAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置推荐车道线箭头
     *
     * @param index      index
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveRecommendArrow(final int index, final SceneCommonStruct.LaneAction laneAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", laneAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendArrow(laneAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置分时车道线箭头
     *
     * @param index      index
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveTimeArrow(final int index, final SceneCommonStruct.LaneAction laneAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", laneAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveTimeArrow(laneAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置车道线箭头
     *
     * @param index      index
     * @param laneAction laneAction
     */
    public void setBackgroundLanesDriveDefaultArrow(final int index, final SceneCommonStruct.LaneAction laneAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", laneAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveDefaultArrow(laneAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置公交专用等场景
     *
     * @param index index
     */
    public void sceneBottom(final int index) {
        Logger.d(TAG, "SceneNaviLanesView sceneBottom index：", index);
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
     *
     * @param index                index
     * @param timeLaneBottomAction laneinfo
     */
    public void setBackgroundLanesDriveRecommendBottom(final int index,
                                                       final SceneCommonStruct.TimeLaneBottomAction
                                                               timeLaneBottomAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", timeLaneBottomAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveRecommendBottom(timeLaneBottomAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置是否高亮
     *
     * @param index     index
     * @param isVisible 是否可见
     */
    public void setVisibleHighlight(final int index, final boolean isVisible) {
        Logger.d(TAG, "SceneNaviLanesView setVisibleHighlight index：", index,
                ",isVisible：", isVisible);
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
     *
     * @param index                index
     * @param timeLaneBottomAction laneInfo
     */
    public void setBackgroundLanesDriveDefaultBottom(final int index,
                                                     final SceneCommonStruct.TimeLaneBottomAction
                                                             timeLaneBottomAction) {
        if (Logger.openLog) {
            Logger.d(TAG, "index:", index, " laneAction:", timeLaneBottomAction.name());
        }
        switch (index) {
            case 0:
                mViewBinding.tlrResources161.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 1:
                mViewBinding.tlrResources16.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 2:
                mViewBinding.tlrResources17.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 3:
                mViewBinding.tlrResources18.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 4:
                mViewBinding.tlrResources19.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 5:
                mViewBinding.tlrResources110.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 6:
                mViewBinding.tlrResources111.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            case 7:
                mViewBinding.tlrResources1.setBackgroundLanesDriveDefaultBottom(timeLaneBottomAction);
                break;
            default:
                break;
        }
    }

    /**
     * 设置普通场景
     *
     * @param index index
     */
    public void sceneCommonArrow(final int index) {
        Logger.d(TAG, "SceneNaviLanesView sceneCommonArrow index：", index);
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

    /**
     * 显示默认的车道线
     */
    public void sceneLaneInfoDefault() {
        Logger.d(TAG, "SceneNaviLanesView sceneLaneInfoDefault");
//        setVisibility(VISIBLE);
        NaviUiUtil.hideView(mViewBinding.tlrResources161, mViewBinding.tlrResources16, mViewBinding.tlrResources17,
                mViewBinding.tlrResources18, mViewBinding.tlrResources19, mViewBinding.tlrResources110,
                mViewBinding.tlrResources111, mViewBinding.tlrResources1);
    }
}
