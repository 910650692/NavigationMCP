package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneNaviContinueView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviContinueImpl extends BaseSceneModel<SceneNaviContinueView> {

    public static final String TAG = "SceneNaviContinueImpl";

    private LayerPackage mLayerPackage;
    private NaviPackage mNaviPackage;
    private MapPackage mMapPackage;

    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;

    public SceneNaviContinueImpl(SceneNaviContinueView mScreenView) {
        super(mScreenView);
        mLayerPackage = LayerPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    public void notifySceneStateChange(final boolean isVisible) {
        Logger.i(TAG, "notifySceneStateChange", isVisible + " mScreenView.isVisible()：" +
                mScreenView.isVisible());
        if (mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                        INaviSceneEvent.SceneStateChangeType.SceneShowState :
                        INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_CONTINUE);
    }

    /**
     * @param currentImersiveStatus 沉浸态触摸态回调
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        Logger.i(TAG, "onImmersiveStatusChange currentImersiveStatus：" +
                currentImersiveStatus);
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
            mLayerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            initTimer();
            notifySceneStateChange(true);
        } else {
            naviContinue();
            notifySceneStateChange(false);
        }
    }

    /**
     * 开始倒计时
     */
    private void initTimer() {
        Logger.i(TAG, "initTimer");
        cancelTimer();
        mTimes = NumberUtils.NUM_8;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(this::naviContinue);
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 取消倒计时
     */
    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    /**
     * 导航继续
     */
    public void naviContinue() {
        Logger.i(TAG, "naviContinue");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(
                MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        if (!mNaviPackage.getFixedOverViewStatus()) {
            OpenApiHelper.exitPreview(mMapTypeId);
        }
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId, ImersiveStatus.IMERSIVE);
        // 隐藏继续当行按钮
        notifySceneStateChange(false);
    }

}
