package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.ISceneNaviViaList;
import com.fy.navi.scene.ui.navi.SceneNaviViaListView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviViaListImpl extends BaseSceneModel<SceneNaviViaListView> implements ISceneNaviViaList {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_LIST_IMPL;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;

    public SceneNaviViaListImpl(final SceneNaviViaListView screenView) {
        super(screenView);
    }

    @Override
    public void skipAlongWayFragment() {
        if (mCallBack != null) {
            mCallBack.skipAlongWayFragment();
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        cancelTimer();
    }

    /**
     * init timer
     */
    public void initTimer() {
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                // 只有当前是navi页面才执行倒计时关闭页面
                boolean currentNavi = mCallBack != null && mCallBack.getCurrentFragmentIsNavi();
                if (currentNavi) {
                    ThreadManager.getInstance().postUi(() -> {
                        updateSceneVisible(false);
                        cancelTimer();
                    });
                } else {
                    initTimer();
                }
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * cancel timer
     */
    public void cancelTimer() {
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * @param isVisible visible
     */
    public void updateSceneVisible(final boolean isVisible) {
        Logger.i(TAG, "SceneNaviViaListImpl", "isVisible:", isVisible, "currentVis:",
                mScreenView.isVisible());
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_VIA_POINT_LIST);
    }
}
