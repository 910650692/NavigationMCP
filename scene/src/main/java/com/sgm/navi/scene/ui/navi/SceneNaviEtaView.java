package com.sgm.navi.scene.ui.navi;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.databinding.SceneNaviEtaViewBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviEtaImpl;
import com.sgm.navi.scene.impl.navi.common.AutoUIDrawable;
import com.sgm.navi.scene.impl.navi.common.AutoUIString;
import com.sgm.navi.scene.impl.navi.common.SceneCommonStruct;
import com.sgm.navi.scene.impl.navi.common.SceneEnumRes;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;

/**
 * eta信息scene(途经点面板)
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviEtaView extends NaviSceneBase<SceneNaviEtaViewBinding, SceneNaviEtaImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_ETA;

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
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_ETA;
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
        mISceneCallback = null;
    }

    /**
     * 设置剩余距离和时间
     * @param textContent content
     */
    public void setTextNaviEtaRouteRemainDefault(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteRemainDefault：",
                textContent.getString(getContext()));
        mViewBinding.stvRemainingDistanceTime.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到达时间和距离
     * @param textContent content
     */
    @SuppressLint("SetTextI18n")
    public void setTextNaviEtaRouteArrivalDefault(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaRouteArrivalDefault：",
                textContent.getString(getContext()));
        mViewBinding.stvArriveTime.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到达天数
     * @param textContent context
     */
    public void setTextNaviEtaArrivalDay(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviEtaArrivalDay：",
                textContent.getString(getContext()));
        mViewBinding.stvArrivalDay.setText(textContent.getString(getContext()));
    }

    /**
     * @param textContent context
     */
    //设置近接动作
    public void setTextNaviNextTurn(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviNextTurn：",
                textContent.getString(getContext()));
        String text = textContent.getString(getContext());
        mViewBinding.stvTextNext.setText(text);
        if (null != mISceneCallback) {
            mISceneCallback.updateNextText(text);
        }
    }

    /**
     * @param drawableValue drawable
     */
    public void setBackgroundNaviNextTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviNextTurnIcon");
        BitmapDrawable bitmapDrawable = new BitmapDrawable(getResources(),
                drawableValue.getBitmap());
        mViewBinding.sivHudSou31.setBackground(bitmapDrawable);
        if (null != mISceneCallback) {
            mISceneCallback.updateNextIcon(-1, bitmapDrawable);
        }
    }

    /**
     * 设置离线近接动作图片
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineNextTurnIcon(
            final SceneCommonStruct.TbtExitIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineNextTurnIcon：",
                iconAction.name());
        int resource = SceneEnumRes.getDrawableEnumName(iconAction).getDayDrawableId();
        mViewBinding.sivHudSou31.setBackgroundResource(resource);
        if (null != mISceneCallback) {
            mISceneCallback.updateNextIcon(resource, null);
        }
    }

    public void onCrossImageShow(boolean isRealNeedShow) {
        Logger.i(TAG, "GuidanceTbtView onCrossImageInfo：", isRealNeedShow);
        if (null != mScreenViewModel) {
            mScreenViewModel.onCrossImageShow(isRealNeedShow);
        }
    }

    public void refreshArriveTime() {
        if (null != mScreenViewModel) {
            mScreenViewModel.refreshArriveTime();
        }
    }

    @Override
    public void show() {
        super.show();
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(NaviPackage.getInstance().getCurrentNaviEtaInfo());
        }
    }
}
