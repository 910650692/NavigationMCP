package com.fy.navi.scene.impl.navi;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviSpeedView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.SpeedOverallEntity;

import java.util.ArrayList;

public class SceneNaviSpeedImpl extends BaseSceneModel<SceneNaviSpeedView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    /**
     * 区间测速
     */
    private int mLimitSpeed = 0; // 当前区间限速值
    private int mAverageSpeed = 0; // 当前区间平均车速
    private int mRemainDistance = 0; // 区间路段剩余距离

    /**
     * 电子👁限速
     */
    private int mCurCameraLimitSpeed = 0;
    private ISceneCallback mISceneCallback;

    public SceneNaviSpeedImpl(SceneNaviSpeedView mScreenView) {
        super(mScreenView);
    }

    public void onNaviSpeedCameraInfo(SpeedOverallEntity speedCameraInfo) {
        if (mISceneCallback == null) {
            return;
        }
        if (speedCameraInfo == null) {
            updateSceneVisible(false);
            return;
        }
        int speedType = speedCameraInfo.getSpeedType();
        Logger.i(TAG, "speedType " + speedType);
        if (speedType == NaviConstant.SpeedType.SPEED_OVERALL) {
            getLimitSpeed(speedCameraInfo.getLimitSpeedList());
            if (isValidSpeed(mLimitSpeed)) {
                mRemainDistance = speedCameraInfo.getRemainDistance();
            } else {
                mAverageSpeed = 0;
                mRemainDistance = 0;
            }
            if (mAverageSpeed != speedCameraInfo.getAverageSpeed()) {
                mAverageSpeed = speedCameraInfo.getAverageSpeed();
            }
            Logger.i(TAG, "SceneNaviSpeedImpl: limit：" + mLimitSpeed + ",average：" + mAverageSpeed + ",distance：" + mRemainDistance);
            if (mLimitSpeed == 0 || mAverageSpeed == 0) {
                updateSceneVisible(false);
                return;
            }
            updateSceneVisible(true);
            mScreenView.updateOverallInfo(mLimitSpeed, mAverageSpeed, mRemainDistance);
        } else if (speedType == NaviConstant.SpeedType.SPEED_GREEN_WAVE) {
            if (speedCameraInfo.getMinSpeed() == 0 || speedCameraInfo.getMaxSpeed() == 0) {
                updateSceneVisible(false);
                return;
            }
            updateSceneVisible(true);
            mScreenView.updateGreenWaveInfo(speedCameraInfo, 100);
        }
    }

    private int getLimitSpeed(ArrayList<Short> speeds) {
        if (!ConvertUtils.isEmpty(speeds)) {
            for (int speed : speeds) {
                if (isValidSpeed(speed) && speed > mLimitSpeed) {
                    mLimitSpeed = speed;
                }
            }
        } else {
            mLimitSpeed = 0;
        }
        return mLimitSpeed;
    }

    private boolean isValidSpeed(int speed) {
        return 0 < speed && speed < 0xff;
    }

    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    private void updateSceneVisible(boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_SPEED);
    }
}
