package com.fy.navi.scene.ui.navi;


import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_SERVICE_AREA;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.Nullable;

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
 */
public class SceneNaviSapaView extends NaviSceneBase<SceneNaviSapaViewBinding, SceneNaviSapaImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviSapaView(Context context) {
        super(context);
    }

    public SceneNaviSapaView(Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSapaView(Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_SERVICE_AREA;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }


    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_SERVICE_AREA, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_SERVICE_AREA, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_SERVICE_AREA, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_SERVICE_AREA, false);
        }
    }

    @Override
    protected SceneNaviSapaViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviSapaViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSapaImpl initSceneImpl() {
        return new SceneNaviSapaImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviSapa(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    /**
     * 设置第一个容器的宽度
     **/
    public void setFirstSflParams(int width) {
//        ViewGroup.LayoutParams layoutParams = mViewBinding.sflFirst.getLayoutParams();
//        layoutParams.width = width;
//        mViewBinding.sflFirst.setLayoutParams(layoutParams);
    }

    /**
     * 设置first容器背景
     **/
    public void updateSflFirst(int resID, SkinConstraintLayout view) {
//        mViewBinding.sflFirst.removeAllViews();
//        mViewBinding.sflFirst.setBackgroundResource(resID);
//        mViewBinding.sflFirst.addView(view);
    }

    /**
     * 设置second容器背景
     **/
    public void updateSflSecond(int resID, SkinConstraintLayout view) {
//        mViewBinding.sflSecond.removeAllViews();
//        mViewBinding.sflSecond.setBackgroundResource(resID);
//        mViewBinding.sflSecond.addView(view);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviSAPAInfo(sapaInfoEntity);
        }
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        if (mScreenViewModel != null) {
            mScreenViewModel.addSceneCallback();
        }
    }

}
