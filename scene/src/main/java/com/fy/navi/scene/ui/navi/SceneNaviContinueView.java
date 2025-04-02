package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SceneNaviContinueViewBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.SceneNaviContinueImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;

public class SceneNaviContinueView extends NaviSceneBase<SceneNaviContinueViewBinding,
        SceneNaviContinueImpl> {

    public static final String TAG = "SceneNaviContinueView";

    private ISceneCallback mISceneCallback;

    public SceneNaviContinueView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviContinueView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviContinueView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CONTINUE, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CONTINUE, false);
        }
    }

    @Override
    public void close() {
        super.close();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_CONTINUE, false);
        }
    }

    @Override
    protected SceneNaviContinueViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviContinueViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviContinueImpl initSceneImpl() {
        return new SceneNaviContinueImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviContinue(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_CONTINUE;
    }

    @Override
    protected String getSceneName() {
        return NaviSceneId.NAVI_CONTINUE.name();
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NaviSceneId.NAVI_CONTINUE, this);
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    /**
     * @param currentImersiveStatus status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

}
