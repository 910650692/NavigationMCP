package com.sgm.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.SceneNaviViaArriveView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;

public class SceneNaviViaArriveViewImpl extends BaseSceneModel<SceneNaviViaArriveView> {
    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_ARRIVE_IMPL;
    public SceneNaviViaArriveViewImpl(final SceneNaviViaArriveView screenView) {
        super(screenView);
    }

    /**
     * 点击确定后立即更新ui为途经点已到达
     */
    public void onUpdateViaPass() {
        if (!ConvertUtils.isNull(mCallBack)) {
            mCallBack.onUpdateViaPass();
        }
    }

    /**
     * @param isVisible the isVisible to set
     */
    public void updateSceneVisible(final boolean isVisible) {
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(TAG, "SceneNaviViaArriveViewImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_VIA_ARRIVED_POP);
    }

}
