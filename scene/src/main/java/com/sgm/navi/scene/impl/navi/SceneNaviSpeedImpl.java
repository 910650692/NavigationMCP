package com.sgm.navi.scene.impl.navi;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.SceneNaviSpeedView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;

public class SceneNaviSpeedImpl extends BaseSceneModel<SceneNaviSpeedView> implements
        SignalCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_SPEED_IMPL;
    /**
     * 区间测速
     */
    private int mLimitSpeed = 0; // 当前区间限速值
    private int mAverageSpeed = 0; // 当前区间平均车速
    private int mRemainDistance = 0; // 区间路段剩余距离

    public ObservableField<String> mCurrentSpeedTxt;
    private int mCurrentSpeed;
    // bug:1044165 区间测速和绿波路段来回回调导致闪烁，加入区间测速的显示标识，如果正在显示区间测速，不隐藏整个页面
    private boolean mIsSpeedOverShow;

    @Override
    protected void onCreate() {
        super.onCreate();
        SignalPackage.getInstance().registerObserver(
                SceneNaviSpeedImpl.class.getSimpleName(), this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        SignalPackage.getInstance().unregisterObserver(SceneNaviSpeedImpl.class.getSimpleName());
    }

    /**
     * 电子👁限速
     */
    private int mCurCameraLimitSpeed = 0;

    public SceneNaviSpeedImpl(final SceneNaviSpeedView screenView) {
        super(screenView);
        mCurrentSpeedTxt = new ObservableField<>("0");
    }

    /**
     * @param speedCameraInfo 限速信息
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        Logger.i(TAG, "onNaviSpeedCameraInfo speedCameraInfo = ",
                speedCameraInfo.toString());
        if (mCallBack == null) {
            return;
        }
        if (speedCameraInfo == null) {
            updateSceneVisible(false);
            mIsSpeedOverShow = false;
            return;
        }
        final int speedType = speedCameraInfo.getSpeedType();
        Logger.i(TAG, "speedType ", speedType);
        // 限速类型为区间测速
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
            Logger.i(TAG, "SceneNaviSpeedImpl: limit：", mLimitSpeed, ",average：",
                    mAverageSpeed, ",distance：", mRemainDistance);
            if (mLimitSpeed == 0 || mAverageSpeed == 0) {
                Logger.i(TAG, "区间测速 限速不显示");
                updateSceneVisible(false);
                mIsSpeedOverShow = false;
                return;
            }
            updateSceneVisible(true);
            mIsSpeedOverShow = true;
            mScreenView.updateOverallInfo(mLimitSpeed, mAverageSpeed, mCurrentSpeed, mRemainDistance);
        } else if (speedType == NaviConstant.SpeedType.SPEED_GREEN_WAVE) {
            if (speedCameraInfo.getMinSpeed() == 0 || speedCameraInfo.getMaxSpeed() == 0) {
                Logger.i(TAG, "绿波路段 限速不显示");
                if (!mIsSpeedOverShow) {
                    updateSceneVisible(false);
                }
                return;
            }
            updateSceneVisible(true);
            Logger.i(TAG, "显示限速信息");
            mScreenView.updateGreenWaveInfo(speedCameraInfo, mCurrentSpeed);
        }
    }

    /**
     * 注意，这里要获取最大限速值
     * @param speeds 限速值
     * @return 限速值
     */
    private int getLimitSpeed(final ArrayList<Short> speeds) {
        int mMaxLimitSpeed = 0;
        if (!ConvertUtils.isEmpty(speeds)) {
            for (int speed : speeds) {
                if (isValidSpeed(speed)) {
                    if (speed > mMaxLimitSpeed) {
                        mMaxLimitSpeed = speed;
                        mLimitSpeed = mMaxLimitSpeed;
                    }
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

    /**
     * 车速变化的回调
     * @param speed 单位km/h
     */
    @Override
    public void onSpeedChanged(float speed) {
        Logger.i(TAG, "onSpeedChanged:", speed);
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                int currentSpeed = 0;
                if (speed > 0) {
                    currentSpeed = Math.round(speed);
                }
                mCurrentSpeed = currentSpeed;
                if (null != mCurrentSpeedTxt) {
                    mCurrentSpeedTxt.set(currentSpeed + "");
                }
            }
        });
    }
}
