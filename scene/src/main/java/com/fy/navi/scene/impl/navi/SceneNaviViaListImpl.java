package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.api.navi.ISceneNaviViaList;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviViaListView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviViaListImpl extends BaseSceneModel<SceneNaviViaListView> implements ISceneNaviViaList {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;

    public SceneNaviViaListImpl(final SceneNaviViaListView screenView) {
        super(screenView);
    }

    @Override
    public void skipAlongWayFragment() {
        if (mISceneCallback != null) {
            mISceneCallback.skipAlongWayFragment();
        }
    }

    /**
     * @param sceneCallback callback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    /**
     * init timer
     */
    public void initTimer() {
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(() -> {
                    updateSceneVisible(false);
                    cancelTimer();
                });
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
    private void updateSceneVisible(final boolean isVisible) {
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviViaListImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_VIA_POINT_LIST);
    }
}
