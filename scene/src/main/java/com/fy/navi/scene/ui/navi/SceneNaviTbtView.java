package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_TBT;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.drawable.BitmapDrawable;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
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
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;

/**
 * tbt看板scene
 */
public class SceneNaviTbtView extends NaviSceneBase<SceneNaviTbtViewBinding, SceneNaviTbtImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviTbtView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviTbtView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviTbtView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_TBT;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_TBT, this);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_TBT, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_TBT, true);
        }
    }

    @Override
    protected SceneNaviTbtViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
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

    public void onNaviArrive(long traceId, int naviType) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviArrive(traceId, naviType);
        }
    }

    @Override
    public void onDestroy() {
    }

    //设置引导转向图片
    public void setBackgroundNaviExitTurnIcon(AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviExitTurnIcon：");
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    //设置出口提示
    public void setTextNaviExit(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviExit：" + textContent.getString(getContext()));
        // 《出口信息》出口编号 ui未体现
        mViewBinding.stvExitNum.setText(textContent.getString(getContext()));
    }

    //设置到下一路口名称
    public void setTextNaviInfoDistanceNextRoad(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoad：" + textContent.getString(getContext()));
        mViewBinding.stvDivDistance.setText(textContent.getString(getContext()));
    }

    public void setTextNaviInfoDistanceNextRoadName(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadName：" + textContent.getString(getContext()));
        mViewBinding.stvDivAddress.setText(textContent.getString(getContext()));
    }

    //设置引导普通转向图片
    public void setBackgroundNaviCommonTurnIcon(AutoUIDrawable drawableValue) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviCommonTurnIcon：");
        mViewBinding.sivHudSou3.setBackground(new BitmapDrawable(getResources(), drawableValue.getBitmap()));
    }

    //设置到下一路口剩余距离单位
    @SuppressLint("SetTextI18n")
    public void setTextNaviInfoDistanceNextRoadUnit(AutoUIString textContent) {
        Logger.d(TAG, "GuidanceTbtView setTextNaviInfoDistanceNextRoadUnit：" + textContent.getString(getContext()));
        mViewBinding.stvDivUnit.setText(textContent.getString(getContext()));
    }

    //设置离线引导普通转向图片
    public void setBackgroundNaviOfflineCommonTurnIcon(SceneCommonStruct.TbtIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineCommonTurnIcon：" + iconAction.name());
        mViewBinding.sivHudSou3.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).getDayDrawableId());
    }

    //设置离线引导出口转向图片
    public void setBackgroundNaviOfflineExitTurnIcon(SceneCommonStruct.TbtExitIconAction iconAction) {
        Logger.d(TAG, "GuidanceTbtView setBackgroundNaviOfflineExitTurnIcon：" + iconAction.name());
        // 《出口信息》图标 ui未体现
        mViewBinding.sivHudSou33.setBackgroundResource(SceneEnumRes.getDrawableEnumName(iconAction).getDayDrawableId());
    }
}
