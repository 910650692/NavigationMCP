package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
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
    private ScheduledFuture scheduledFuture;
    private int times = NumberUtils.NUM_8;

    public SceneNaviViaListImpl(SceneNaviViaListView mScreenView) {
        super(mScreenView);
    }

    @Override
    public void skipAlongWayFragment() {
        if (mISceneCallback != null) {
            mISceneCallback.skipAlongWayFragment();
        }
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    public void initTimer() {
        cancelTimer();
        times = NumberUtils.NUM_8;
        scheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (times == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(() -> {
                    updateSceneVisible(false);
                    cancelTimer();
                });
            }
            times--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    public void cancelTimer() {
        if (!ConvertUtils.isEmpty(scheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(scheduledFuture);
            scheduledFuture = null;
        }
    }

    private void updateSceneVisible(boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_VIA_POINT_UNFOLD);
    }
}
