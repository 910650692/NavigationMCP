package com.fy.navi.scene.impl.navi;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviViaArriveView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;

public class SceneNaviViaArriveViewImpl extends BaseSceneModel<SceneNaviViaArriveView> {
    public static final String TAG = "SceneNaviViaArriveViewImpl";
    private ISceneCallback mISceneCallback;
    public SceneNaviViaArriveViewImpl(SceneNaviViaArriveView mScreenView) {
        super(mScreenView);
    }

    public void addISceneCallback(ISceneCallback mISceneCallback) {
        this.mISceneCallback = mISceneCallback;
    }

    /**
     * 点击确定后立即更新ui为途经点已到达
     */
    public void onUpdateViaPass() {
        mISceneCallback.onUpdateViaPass();
    }

    public void updateSceneVisible(boolean isVisible) {
        Logger.i(TAG, "updateSceneVisible isVisible = " + isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_VIA_ARRIVED_POP);
    }

}
