package com.fy.navi.scene.ui.navi;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.R;
import com.fy.navi.scene.databinding.SceneNaviSapaViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviSapaImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.ui.view.SkinConstraintLayout;

/**
 * 收费站/服务区scene 显示在tbt下方的一个bar条
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviSapaView extends NaviSceneBase<SceneNaviSapaViewBinding, SceneNaviSapaImpl> {
    private static final String TAG = "SceneNaviSapaView";
    private ISceneCallback mISceneCallback;

    public SceneNaviSapaView(final Context context) {
        super(context);
    }

    public SceneNaviSapaView(final Context context, @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSapaView(final Context context, @Nullable final AttributeSet attrs,
                             final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_SERVICE_AREA;
    }

    @Override
    protected String getSceneName() {
        return NaviSceneId.NAVI_SCENE_SERVICE_AREA.name();
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }


    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_SERVICE_AREA,
                this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SERVICE_AREA, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SERVICE_AREA, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_SERVICE_AREA, false);
        }
    }

    @Override
    protected SceneNaviSapaViewBinding createViewBinding(final LayoutInflater inflater,
                                                         final ViewGroup viewGroup) {
        return SceneNaviSapaViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSapaImpl initSceneImpl() {
        return new SceneNaviSapaImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSapa(mScreenViewModel);
        mViewBinding.naviSapaOnlyToll.setNaviSapa(mScreenViewModel);
        mViewBinding.naviSapaOnlyService.setNaviSapa(mScreenViewModel);
        mViewBinding.naviSapaFirstToll.setNaviSapa(mScreenViewModel);
        mViewBinding.naviSapaFirstService.setNaviSapa(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * 设置第一个容器的宽度
     *
     * @param width 宽度
     * */
    public void setFirstSflParams(final int width) {
//        ViewGroup.LayoutParams layoutParams = mViewBinding.sflFirst.getLayoutParams();
//        layoutParams.width = width;
//        mViewBinding.sflFirst.setLayoutParams(layoutParams);
    }

    /**
     * 设置first容器背景
     *
     * @param resID 资源id
     * @param view view
     * */
    public void updateSflFirst(final int resID, final SkinConstraintLayout view) {
//        mViewBinding.sflFirst.removeAllViews();
//        mViewBinding.sflFirst.setBackgroundResource(resID);
//        mViewBinding.sflFirst.addView(view);
    }

    /**
     * 设置second容器背景
     *
     * @param resID 资源id
     * @param view view
     * */
    public void updateSflSecond(final int resID, final SkinConstraintLayout view) {
//        mViewBinding.sflSecond.removeAllViews();
//        mViewBinding.sflSecond.setBackgroundResource(resID);
//        mViewBinding.sflSecond.addView(view);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    /**
     * @param sapaInfoEntity 服务区信息
     */
    public void onNaviSAPAInfo(final SapaInfoEntity sapaInfoEntity) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviSAPAInfo(sapaInfoEntity);
        }
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
        mScreenViewModel.addSceneCallback(sceneCallback);
    }

    public void updateOnlyServiceUi() {
        Logger.i(TAG, "updateOnlyServiceUi()");
        if (mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.getVisibility() == GONE) {
            mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.setVisibility(VISIBLE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                    mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.getLayoutParams();
            params.endToEnd = 295;
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setLayoutParams(params);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceName.setMaxWidth(170);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setMaxWidth(125);
            invalidate();
        } else {
            mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.setVisibility(GONE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                    mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.getLayoutParams();
            params.endToEnd = 24;
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setLayoutParams(params);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceName.setMaxWidth(400);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setMaxWidth(200);
            invalidate();
        }
    }

    public void updateFirstServiceUi() {
        Logger.i(TAG, "updateFirstServiceUi");
        if (mViewBinding.naviSapaFirstService.clFirstServiceChargeData.getVisibility() == GONE) {
            mViewBinding.naviSapaFirstService.clFirstServiceChargeData.setVisibility(VISIBLE);
            mViewBinding.naviSapaFirstService.stvFirstServiceName.setVisibility(GONE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams) mViewBinding.
                    naviSapaFirstService.stvFirstServiceDistance.getLayoutParams();
            params.startToStart = 30;
            params.bottomToBottom = 12;
            mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
            invalidate();
        } else {
            mViewBinding.naviSapaFirstService.clFirstServiceChargeData.setVisibility(GONE);
            mViewBinding.naviSapaFirstService.stvFirstServiceName.setVisibility(VISIBLE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams) mViewBinding.
                    naviSapaFirstService.stvFirstServiceDistance.getLayoutParams();
            params.startToStart = 223;
            params.bottomToBottom = 26;
            mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
            invalidate();
        }
    }

    public void resetUi() {
        Logger.i(TAG, "resetUi");
        mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.setVisibility(GONE);
        ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.getLayoutParams();
        params.endToEnd = 24;
        mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setLayoutParams(params);
        mViewBinding.naviSapaOnlyService.stvOnlyServiceName.setMaxWidth(400);
        mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setMaxWidth(200);
        mViewBinding.naviSapaFirstService.clFirstServiceChargeData.setVisibility(GONE);
        mViewBinding.naviSapaFirstService.stvFirstServiceName.setVisibility(VISIBLE);
        params = (ConstraintLayout.LayoutParams) mViewBinding.
                naviSapaFirstService.stvFirstServiceDistance.getLayoutParams();
        params.startToStart = 223;
        params.bottomToBottom = 26;
        mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
        invalidate();
    }
}
