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

    public SceneNaviSpeedImpl(final SceneNaviSpeedView screenView) {
        super(screenView);
    }

    /**
     * @param speedCameraInfo 限速信息
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        Logger.i(TAG, "onNaviSpeedCameraInfo speedCameraInfo = " +
                speedCameraInfo.toString());
        if (mISceneCallback == null) {
            return;
        }
        if (speedCameraInfo == null) {
            updateSceneVisible(false);
            return;
        }
        final int speedType = speedCameraInfo.getSpeedType();
        Logger.d(TAG, "speedType " + speedType);
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
            Logger.d(TAG, "SceneNaviSpeedImpl: limit：" + mLimitSpeed + ",average：" +
                    mAverageSpeed + ",distance：" + mRemainDistance);
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

    /**
     * @param speeds 限速值
     * @return 限速值
     */
    private int getLimitSpeed(final ArrayList<Short> speeds) {
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

    /**
     * @param speed 速度值
     * @return 是否有效
     */
    private boolean isValidSpeed(final int speed) {
        return 0 < speed && speed < 0xff;
    }

    /**
     * @param sceneCallback 回调
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    /**
     * @param isVisible 是否可见
     */
    private void updateSceneVisible(final boolean isVisible) {
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviSpeedImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState),
                NaviSceneId.NAVI_SCENE_SPEED);
    }
}
