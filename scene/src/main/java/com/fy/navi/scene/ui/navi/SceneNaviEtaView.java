package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_ETA;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviEtaViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviEtaImpl;
import com.fy.navi.scene.impl.navi.common.AutoUIDrawable;
import com.fy.navi.scene.impl.navi.common.AutoUIString;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.impl.navi.common.SceneEnumRes;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;

/**
 * eta信息scene(途经点面板)
 */
public class SceneNaviEtaView extends NaviSceneBase<SceneNaviEtaViewBinding, SceneNaviEtaImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviEtaView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviEtaView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviEtaView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_ETA;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_ETA, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_ETA, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_ETA, false);
        }
    }

    @Override
    protected SceneNaviEtaViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviEtaViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviEtaImpl initSceneImpl() {
        return new SceneNaviEtaImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviEta(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    public void onManeuverInfo(NaviManeuverInfo info) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onManeuverInfo(info);
        }
    }

    @Override
    public void onDestroy() {
    }

    //设置剩余距离和时间
    public void setTextNaviEtaRouteRemainDefault(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteRemainDefault：" + textContent.getString(getContext()));
        mViewBinding.stvRemainingDistanceTime.setText(textContent.getString(getContext()));
    }

    //设置到达时间和距离
    @SuppressLint("SetTextI18n")
    public void setTextNaviEtaRouteArrivalDefault(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteArrivalDefault：" + textContent.getString(getContext()));
        mViewBinding.stvArriveTime.setText(textContent.getString(getContext()));
    }

    //设置到达天数
    public void setTextNaviEtaArrivalDay(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaArrivalDay：" + textContent.getString(getContext()));
        mViewBinding.stvArrivalDay.setText(textContent.getString(getContext()));
    }

    //设置近接动作
    public void setTextNaviNextTurn(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviNextTurn：" + textContent.getString(getContext()));
        mViewBinding.stvTextNext.setText(textContent.getString(getContext()));
    }

    //设置近接动作图片
    public void setBackgroundNaviNextTurnIcon(AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviNextTurnIcon：");
        mViewBinding.sivHudSou31.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    //设置离线近接动作图片
    public void setBackgroundNaviOfflineNextTurnIcon(SceneCommonStruct.TbtIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineNextTurnIcon：" + iconAction.name());
        mViewBinding.sivHudSou31.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).getDayDrawableId());
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }
}
