package com.fy.navi.scene.ui.navi;
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
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviEtaView extends NaviSceneBase<SceneNaviEtaViewBinding, SceneNaviEtaImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;

    public SceneNaviEtaView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviEtaView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviEtaView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                            final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_ETA;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_ETA, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_ETA, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_ETA, false);
        }
    }

    @Override
    protected SceneNaviEtaViewBinding createViewBinding(final LayoutInflater inflater,
                                                        final ViewGroup viewGroup) {
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

    /**
     * @param naviETAInfo 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    /**
     * @param info 转向信息
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onManeuverInfo(info);
        }
    }

    @Override
    public void onDestroy() {
    }

    /**
     * 设置剩余距离和时间
     * @param textContent content
     */
    public void setTextNaviEtaRouteRemainDefault(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteRemainDefault：" + textContent.getString(getContext()));
        mViewBinding.stvRemainingDistanceTime.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到达时间和距离
     * @param textContent content
     */
    @SuppressLint("SetTextI18n")
    public void setTextNaviEtaRouteArrivalDefault(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteArrivalDefault：" +
                textContent.getString(getContext()));
        mViewBinding.stvArriveTime.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到达天数
     * @param textContent context
     */
    public void setTextNaviEtaArrivalDay(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaArrivalDay：" + textContent.getString(getContext()));
        mViewBinding.stvArrivalDay.setText(textContent.getString(getContext()));
    }

    /**
     * @param textContent context
     */
    //设置近接动作
    public void setTextNaviNextTurn(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviNextTurn：" +
                textContent.getString(getContext()));
        mViewBinding.stvTextNext.setText(textContent.getString(getContext()));
    }

    /**
     * @param drawableValue drawable
     */
    public void setBackgroundNaviNextTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviNextTurnIcon：");
        mViewBinding.sivHudSou31.setBackground(
                new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    /**
     * 设置离线近接动作图片
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineNextTurnIcon(
            final SceneCommonStruct.TbtIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineNextTurnIcon：" + iconAction.name());
        mViewBinding.sivHudSou31.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).getDayDrawableId());
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }
}
