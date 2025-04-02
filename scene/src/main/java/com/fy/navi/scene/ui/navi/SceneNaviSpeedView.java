package com.fy.navi.scene.ui.navi;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviSpeedViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviSpeedImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.SpeedOverallEntity;

/**
 * 速度scene
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviSpeedView extends NaviSceneBase<SceneNaviSpeedViewBinding,
        SceneNaviSpeedImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;

    public SceneNaviSpeedView(final Context context) {
        super(context);
    }

    public SceneNaviSpeedView(final Context context, final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSpeedView(final Context context, final AttributeSet attrs,
                              final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_SPEED;
    }

    @Override
    protected String getSceneName() {
        return NaviSceneId.NAVI_SCENE_SPEED.name();
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }


    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_SPEED, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SPEED, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            Logger.i(TAG, "SceneNaviSpeedView hide");
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SPEED, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            Logger.i(TAG, "SceneNaviSpeedView hide");
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SPEED, false);
        }
    }

    @Override
    protected SceneNaviSpeedViewBinding createViewBinding(final LayoutInflater inflater,
                                                          final ViewGroup viewGroup) {
        return SceneNaviSpeedViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSpeedImpl initSceneImpl() {
        return new SceneNaviSpeedImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSpeedLimit(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public boolean onTouchEvent(final MotionEvent event) {
        return true;
    }

    /**
     * @param speedLimit 限制速度
     * @param averageSpeed 平均速度
     * @param remain 剩余距离
     */
    @SuppressLint("UseCompatLoadingForDrawables")
    public void updateOverallInfo(final int speedLimit, final int averageSpeed, final int remain) {
        mViewBinding.stvCurrentSpeed.setText(String.valueOf(averageSpeed));
        mViewBinding.stvSpeedLimit.setText(String.valueOf(speedLimit));
        mViewBinding.stvSpeedLimitKey.setText(getContext().getText(R.string.navi_speed_overall));
        // 超速时更换背景
        mViewBinding.stvCurrentSpeed.setTextColor(getContext().getColor(averageSpeed > speedLimit ?
                R.color.navi_color_C73333_100 : R.color.navi_color_2461EA_100));
        mViewBinding.stvCurrentSpeedKey.setTextColor(
                getContext().getColor(averageSpeed > speedLimit ? R.color.navi_color_C73333_100 :
                        R.color.navi_color_2461EA_100));
        mViewBinding.svCurrentSpeed.setBackground(
                getContext().getDrawable(averageSpeed > speedLimit ?
                        R.drawable.guide_car_speed_stroke : R.drawable.guide_car_speed));
        int remainDistance = remain;
        if (remainDistance > 1000) {
            float remainKm = remainDistance / 100f;
            remainKm = Math.round(remainKm) / (float) 10;
            mViewBinding.stvDistance.setText(String.valueOf(remainKm));
        } else {
            if (remainDistance < 0) {
                remainDistance = 0;
            }
            mViewBinding.stvDistance.setText(String.valueOf(remainDistance));
            mViewBinding.stvDistanceKey.setText(getContext().getText(R.string.navi_remaining_distance));
        }
    }

    /**
     * @param entity entity
     * @param currentSpeed 当前的车速
     */
    @SuppressLint("SetTextI18n")
    public void updateGreenWaveInfo(final SpeedOverallEntity entity, final int currentSpeed) {
        mViewBinding.stvCurrentSpeed.setText(String.valueOf(currentSpeed));
        mViewBinding.stvSpeedLimit.setText(entity.getMinSpeed() + "-" + entity.getMaxSpeed());
        mViewBinding.stvSpeedLimitKey.setText(getContext().getText(R.string.navi_speed_suggest));
        mViewBinding.stvDistance.setText(String.valueOf(entity.getLightCount()));
        mViewBinding.stvDistanceKey.setText(getContext().getText(R.string.navi_remaining_green_lights));
    }

    /**
     * @param speedCameraInfo 区间车速、绿波车速信息
     */
    public void onNaviSpeedCameraInfo(final SpeedOverallEntity speedCameraInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
        }
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }
}
