package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.ui.navi.ChargeTipEntity;
import com.fy.navi.scene.ui.navi.SceneNaviChargeBtnType;
import com.fy.navi.scene.ui.navi.SceneNaviChargeTipView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviChargeTipViewImpl extends BaseSceneModel<SceneNaviChargeTipView> {

    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CHARGE_TIP_IMPL;
    private ScheduledFuture scheduledFuture;
    private int times = NumberUtils.NUM_8;
    public SceneNaviChargeTipViewImpl(SceneNaviChargeTipView screenView) {
        super(screenView);
    }

    public void initTimer() {
        cancelTimer();
        times = NumberUtils.NUM_8;
        scheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (times == NumberUtils.NUM_0) {
                Logger.i(TAG, "close-self!");
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
        Logger.i(TAG, "updateSceneVisible", "isVisible:", isVisible, "currentIsVis:",
                mScreenView.isVisible());
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), mScreenView.getSceneId());
    }


    /**
     * 预约充电桩地锁已打开
     */
    public void chargeStationUnlock() {
        final ChargeTipEntity entity = new ChargeTipEntity();
        entity.setTitle(AppCache.getInstance().getMContext().getResources().getString(R.string.tip_msg_8));
        entity.setAction(AppCache.getInstance().getMContext().getResources().getString(R.string.msg_action_know));
        entity.setType(SceneNaviChargeBtnType.I_KNOW);
        mScreenView.updateUi(entity);
    }
}
