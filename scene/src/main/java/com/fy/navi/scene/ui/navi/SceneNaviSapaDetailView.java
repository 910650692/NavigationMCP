package com.fy.navi.scene.ui.navi;


import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SceneNaviSapaDetailViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviSapaDetailImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.define.navi.SapaInfoEntity;

public class SceneNaviSapaDetailView extends NaviSceneBase<SceneNaviSapaDetailViewBinding,
        SceneNaviSapaDetailImpl> {
    private ISceneCallback mISceneCallback;

    public SceneNaviSapaDetailView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviSapaDetailView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviSapaDetailView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs,
                                   final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SAPA_DETAIL_INFO;
    }

    @Override
    protected String getSceneName() {
        return NaviSceneId.NAVI_SAPA_DETAIL_INFO.name();
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SAPA_DETAIL_INFO, this);
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    @Override
    protected SceneNaviSapaDetailViewBinding createViewBinding(final LayoutInflater inflater,
                                                               final ViewGroup viewGroup) {
        return SceneNaviSapaDetailViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviSapaDetailImpl initSceneImpl() {
        return new SceneNaviSapaDetailImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setSapaDetail(mScreenViewModel);
        mViewBinding.sapaServiceDetail.setSapaDetail(mScreenViewModel);
        mViewBinding.sapaTollDetail.setSapaDetail(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SAPA_DETAIL_INFO, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SAPA_DETAIL_INFO, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SAPA_DETAIL_INFO, false);
        }
    }

    /**
     * @param type 场景类型
     * @param sapaInfoEntity 导航信息
     */
    public void skipNaviSapaDetailScene(final int type, final SapaInfoEntity sapaInfoEntity) {
        mScreenViewModel.skipNaviSapaDetailScene(type, sapaInfoEntity);
    }

    
}
