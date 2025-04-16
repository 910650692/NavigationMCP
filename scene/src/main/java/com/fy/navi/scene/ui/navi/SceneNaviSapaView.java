package com.fy.navi.scene.ui.navi;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviSapaViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviSapaImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.ui.view.SkinConstraintLayout;

/**
 * 收费站/服务区scene 显示在tbt下方的一个bar条
 * @author fy
 * @version $Revision.*$
 */
public class SceneNaviSapaView extends NaviSceneBase<SceneNaviSapaViewBinding, SceneNaviSapaImpl> {
    private static final String TAG = "SceneNaviSapaView";

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

    @Override
    public void onDestroy() {
        super.onDestroy();
        mISceneCallback = null;
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
            params.rightMargin = 295;
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setLayoutParams(params);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceName.setMaxWidth(170);
            mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setMaxWidth(125);
            invalidate();
        } else {
            mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.setVisibility(GONE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                    mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.getLayoutParams();
            params.rightMargin = 24;
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
            params.leftMargin = 30;
            params.bottomMargin = 12;
            mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
            invalidate();
        } else {
            mViewBinding.naviSapaFirstService.clFirstServiceChargeData.setVisibility(GONE);
            mViewBinding.naviSapaFirstService.stvFirstServiceName.setVisibility(VISIBLE);
            ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams) mViewBinding.
                    naviSapaFirstService.stvFirstServiceDistance.getLayoutParams();
            params.leftMargin = 223;
            params.bottomMargin = 26;
            mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
            invalidate();
        }
    }

    public void resetUi() {
        Logger.i(TAG, "resetUi");
        mViewBinding.naviSapaOnlyService.clFirstServiceChargeData.setVisibility(GONE);
        ConstraintLayout.LayoutParams params = (ConstraintLayout.LayoutParams)
                mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.getLayoutParams();
        params.rightMargin = 24;
        mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setLayoutParams(params);
        mViewBinding.naviSapaOnlyService.stvOnlyServiceName.setMaxWidth(400);
        mViewBinding.naviSapaOnlyService.stvOnlyServiceDistance.setMaxWidth(200);
        mViewBinding.naviSapaFirstService.clFirstServiceChargeData.setVisibility(GONE);
        mViewBinding.naviSapaFirstService.stvFirstServiceName.setVisibility(VISIBLE);
        params = (ConstraintLayout.LayoutParams) mViewBinding.
                naviSapaFirstService.stvFirstServiceDistance.getLayoutParams();
        params.leftMargin = 223;
        params.bottomMargin = 26;
        mViewBinding.naviSapaFirstService.stvFirstServiceDistance.setLayoutParams(params);
        invalidate();
    }
}
