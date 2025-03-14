package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.NaviSceneServiceAreaBinding;
import com.fy.navi.scene.impl.navi.SceneNaviDetailImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;

/**
 * 服务区、交通事件、收费站Scene
 */
public class SceneNaviServiceArea extends NaviSceneBase<NaviSceneServiceAreaBinding, SceneNaviDetailImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;

    public SceneNaviServiceArea(@NonNull Context context) {
        super(context);
    }

    public SceneNaviServiceArea(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviServiceArea(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_SERVICE_AREA;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_SCENE_SERVICE_AREA, this);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {

    }

    @Override
    protected NaviSceneServiceAreaBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return NaviSceneServiceAreaBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviDetailImpl initSceneImpl() {
        return new SceneNaviDetailImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviEagleEye(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }
}
