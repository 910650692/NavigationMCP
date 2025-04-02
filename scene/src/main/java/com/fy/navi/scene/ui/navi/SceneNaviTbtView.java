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
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviTbtViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviTbtImpl;
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
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;

/**
 * tbt看板scene
 *
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviTbtView extends NaviSceneBase<SceneNaviTbtViewBinding, SceneNaviTbtImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;
    private int mGpsStrength;

    public SceneNaviTbtView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviTbtView(@NonNull final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviTbtView(@NonNull final Context context, @Nullable final AttributeSet attrs,
                            final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_TBT;
    }

    @Override
    protected String getSceneName() {
        return NaviSceneId.NAVI_SCENE_TBT.name();
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_TBT, this);
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_TBT, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_TBT, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_TBT, false);
        }
    }

    @Override
    protected SceneNaviTbtViewBinding createViewBinding(final LayoutInflater inflater,
                                                        final ViewGroup viewGroup) {
        return SceneNaviTbtViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviTbtImpl initSceneImpl() {
        return new SceneNaviTbtImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviTbt(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
    }

    /**
     * @param naviETAInfo naviInfo
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

    /**
     * @param traceId  traceId
     * @param naviType 导航类型
     */
    public void onNaviArrive(final long traceId, final int naviType) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviArrive(traceId, naviType);
        }
    }

    @Override
    public void onDestroy() {
    }

    /**
     * 设置引导转向图片
     *
     * @param drawableValue drawable
     */
    public void setBackgroundNaviExitTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviExitTurnIcon：");
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    /**
     * 设置出口提示
     *
     * @param textContent 文本内容
     */
    public void setTextNaviExit(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviExit：" + textContent.getString(getContext()));
        // 《出口信息》出口编号 ui未体现
        mViewBinding.stvExitNum.setText(textContent.getString(getContext()));
    }

    /**
     * 设置到下一路口名称
     *
     * @param textContent 文本内容
     */
    public void setTextNaviInfoDistanceNextRoad(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoad：" + textContent.getString(getContext()));
        mViewBinding.stvDivDistance.setText(textContent.getString(getContext()));
    }

    /**
     * @param textContent 文本内容
     */
    public void setTextNaviInfoDistanceNextRoadName(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadName：" +
                textContent.getString(getContext()));
        mViewBinding.stvDivAddress.setText(textContent.getString(getContext()));
    }

    /**
     * 设置引导普通转向图片
     *
     * @param drawableValue drawable
     */
    public void setBackgroundNaviCommonTurnIcon(final AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviCommonTurnIcon：");
        mViewBinding.sivHudSou3.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    /**
     * 设置到下一路口剩余距离单位
     *
     * @param textContent 文本内容
     */
    @SuppressLint("SetTextI18n")
    public void setTextNaviInfoDistanceNextRoadUnit(final AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadUnit：" +
                textContent.getString(getContext()));
        mViewBinding.stvDivUnit.setText(textContent.getString(getContext()));
    }

    /**
     * 设置离线引导普通转向图片
     *
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineCommonTurnIcon(
            final SceneCommonStruct.TbtIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineCommonTurnIcon：" +
                iconAction.name());
        mViewBinding.sivHudSou3.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).
                getDayDrawableId());
    }

    /**
     * 设置离线引导出口转向图片
     *
     * @param iconAction iconAction
     */
    public void setBackgroundNaviOfflineExitTurnIcon(
            final SceneCommonStruct.TbtExitIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineExitTurnIcon：" +
                iconAction.name());
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).
                getDayDrawableId());
    }

    public void onUpdateGpsStrength(int gpsStrength) {
        if (mGpsStrength == gpsStrength) {
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mGpsStrength = gpsStrength;
            switch (gpsStrength) {
                case NaviConstant.GpsStrengthState.GPS_NONE:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_none_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_STRONG:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_MEDIUM:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_medium_42);
                    break;
                case NaviConstant.GpsStrengthState.GPS_WEAK:
                    mViewBinding.sivGps.setBackgroundResource(R.drawable.img_satellite_weak_42);
                    break;
            }
        });
    }
}
