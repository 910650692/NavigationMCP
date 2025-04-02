package com.fy.navi.scene.impl.navi;

import androidx.databinding.ObservableField;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.SceneNaviDriveReportView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;

public class SceneNaviDriveReportImpl extends BaseSceneModel<SceneNaviDriveReportView> {
    public static final String TAG = "SceneNaviDriveReportImpl";
    // 总路程
    public ObservableField<String> mTotalMiles;
    // 总时间
    public ObservableField<String> mTotalTime;

    /**
     * @param screenView 页面
     */
    public SceneNaviDriveReportImpl(final SceneNaviDriveReportView screenView) {
        super(screenView);
        mTotalMiles = new ObservableField<>("");
        mTotalTime = new ObservableField<>("");
    }

    /**
     * @param entity 驾驶报告实体
     */
    public void onDriveReport(final NaviDriveReportEntity entity) {
        Logger.i(TAG, "onDriveReport");
        mTotalTime.set(entity.getNaviStatisticsInfoEntity().getDrivenTime() + "");
        mTotalMiles.set(entity.getNaviStatisticsInfoEntity().getDrivenDist() + "");
        updateSceneVisible(true);
    }

    /**
     * 关闭场景
     */
    public void closeScene() {
        updateSceneVisible(false);
    }

    /**
     * @param isVisible 是否可见
     */
    private void updateSceneVisible(final boolean isVisible){
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviDriveReportImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.
                SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_DRIVE_REPORT);
    }
}
