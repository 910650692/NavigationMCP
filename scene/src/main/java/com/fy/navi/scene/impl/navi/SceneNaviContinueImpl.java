package com.fy.navi.scene.impl.navi;

import androidx.fragment.app.Fragment;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.scene.ui.navi.SceneNaviContinueView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navi.OpenApiHelper;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.concurrent.ScheduledFuture;

public class SceneNaviContinueImpl extends BaseSceneModel<SceneNaviContinueView> {

    public static final String TAG = "SceneNaviContinueImpl";

    private ImersiveStatus mImersiveStatus;
    private LayerPackage mLayerPackage;
    private NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private SearchPackage mSearchPackage;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_8;

    public SceneNaviContinueImpl(SceneNaviContinueView mScreenView) {
        super(mScreenView);
        mLayerPackage = LayerPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mSearchPackage = SearchPackage.getInstance();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    public void notifySceneStateChange(final boolean isVisible) {
        Logger.i(TAG, "notifySceneStateChange", isVisible + " mScreenView.isVisible()：" + mScreenView.isVisible());
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
        Logger.i(TAG, "onImmersiveStatusChange-currentImersiveStatus：" + currentImersiveStatus + " mImersiveStatus:" + mImersiveStatus
                + " mIsFixedOverView:" + NaviPackage.getInstance().getFixedOverViewStatus());
        if (mImersiveStatus != currentImersiveStatus) {
            mImersiveStatus = currentImersiveStatus;
        } else {
            if (currentImersiveStatus == ImersiveStatus.TOUCH) {
                initTimer();
            }
            return;
        }
        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
            mLayerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            initTimer();
            notifySceneStateChange(true);
        } else {
            judgeIfNeedNaviContinue();
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
                ThreadManager.getInstance().postUi(new Runnable() {
                    @Override
                    public void run() {
                        judgeIfNeedNaviContinue();
                    }
                });
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    private void judgeIfNeedNaviContinue() {
        // 在非引导页面显示的继续导航按钮需要一直显示，所以在非导航页面倒计时结束后重新开启导航
        if (null != mCallBack) {
            boolean currentIsNavi = mCallBack.getCurrentFragmentIsNavi();
            // 如果有需要全览的列表正在显示不直接进入导航态
            boolean isNeedPreViewShowList = mCallBack.isNeedPreViewShowList();
            Logger.i(TAG, "initTimer currentIsNavi：" + currentIsNavi +
                    " isNeedPreViewShowList = " + isNeedPreViewShowList);
            if (!currentIsNavi || isNeedPreViewShowList) {
                initTimer();
            } else {
                // 加入判断条件，只有在继续按钮显示的情况下才进行导航
                if (null != mScreenView) {
                    if (mScreenView.isVisible()) {
                        naviContinue();
                    }
                }
            }
        }
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


    public void naviContinue() {
        Logger.i(TAG, "naviContinue");
        // 加入防抖
        if (TimerHelper.isCanDo()) {
            if (!mNaviPackage.getFixedOverViewStatus() && mNaviPackage.getPreviewStatus()) {
                OpenApiHelper.exitPreview(mMapTypeId);
            } else if (!mNaviPackage.getFixedOverViewStatus()){
                mMapPackage.goToCarPosition(mMapTypeId, false, false);
                mLayerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
                // bugID：1023666 导航中缩放地图然后点击继续导航，恢复到导航跟随态的过程时间太长
                OpenApiHelper.setCurrentZoomLevel(mMapTypeId);
            }
            mSearchPackage.clearLabelMark();
            // 隐藏继续当行按钮
            notifySceneStateChange(false);
        }
    }

    /**
     * 导航继续点击
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_CONTINUE)
    public void naviContinueClick() {
        Logger.i(TAG, "naviContinueClick");
        naviContinue();
        ImmersiveStatusScene.getInstance().setImmersiveStatus(mMapTypeId,
                ImersiveStatus.IMERSIVE);
        if (null != mScreenView) {
            mScreenView.backToNaviFragment();
        }
        // taskId:1015285 点击继续导航后如果途经点面板在显示状态需要关闭
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneCloseState,
                NaviSceneId.NAVI_SCENE_VIA_POINT_LIST);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        cancelTimer();
    }
}
