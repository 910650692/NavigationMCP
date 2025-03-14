package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_SPEED;

import android.annotation.SuppressLint;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

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
 */
public class SceneNaviSpeedView extends NaviSceneBase<SceneNaviSpeedViewBinding, SceneNaviSpeedImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviSpeedView(Context context) {
        super(context);
    }

    public SceneNaviSpeedView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSpeedView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_SPEED;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }


    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_SPEED, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_SPEED, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_SPEED, true);
        }
    }

    @Override
    protected SceneNaviSpeedViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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
    public boolean onTouchEvent(MotionEvent event) {
        return true;
    }

    @SuppressLint("UseCompatLoadingForDrawables")
    public void updateOverallInfo(int speedLimit, int averageSpeed, int remain) {
        mViewBinding.stvCurrentSpeed.setText(String.valueOf(averageSpeed));
        mViewBinding.stvSpeedLimit.setText(String.valueOf(speedLimit));
        mViewBinding.stvSpeedLimitKey.setText(getContext().getText(R.string.navi_speed_overall));
        // 超速时更换背景
        mViewBinding.stvCurrentSpeed.setTextColor(getContext().getColor(averageSpeed > speedLimit ? R.color.navi_list_item_tv_one : R.color.navi_toll_bg_color));
        mViewBinding.stvCurrentSpeedKey.setTextColor(getContext().getColor(averageSpeed > speedLimit ? R.color.navi_list_item_tv_one : R.color.navi_toll_bg_color));
        mViewBinding.svCurrentSpeed.setBackground(getContext().getDrawable(averageSpeed > speedLimit ? R.drawable.guide_car_speed_stroke : R.drawable.guide_car_speed));
        if (remain > 1000) {
            float remainKm = remain / 100f;
            remainKm = Math.round(remainKm) / (float) 10;
            mViewBinding.stvDistance.setText(String.valueOf(remainKm));
        } else {
            if (remain < 0) {
                remain = 0;
            }
            mViewBinding.stvDistance.setText(String.valueOf(remain));
            mViewBinding.stvDistanceKey.setText(getContext().getText(R.string.navi_remaining_distance));
        }
    }

    @SuppressLint("SetTextI18n")
    public void updateGreenWaveInfo(SpeedOverallEntity entity, int currentSpeed) {
        mViewBinding.stvCurrentSpeed.setText(String.valueOf(currentSpeed));
        mViewBinding.stvSpeedLimit.setText(entity.getMinSpeed() + "-" + entity.getMaxSpeed());
        mViewBinding.stvSpeedLimitKey.setText(getContext().getText(R.string.navi_speed_suggest));
        mViewBinding.stvDistance.setText(String.valueOf(entity.getLightCount()));
        mViewBinding.stvDistanceKey.setText(getContext().getText(R.string.navi_remaining_green_lights));
    }

    public void onNaviSpeedCameraInfo(SpeedOverallEntity speedCameraInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviSpeedCameraInfo(speedCameraInfo);
        }
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback(sceneCallback);
        }
    }
}
