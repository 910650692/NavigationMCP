package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_LAST_MILE;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.MotionEvent;
import android.view.ViewGroup;

import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SceneNaviLastMileViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviLastMileImpl;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;

/***最后一公里***/
public class SceneNaviLastMileView extends NaviSceneBase<SceneNaviLastMileViewBinding, SceneNaviLastMileImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ISceneCallback mISceneCallback;

    public SceneNaviLastMileView(Context context) {
        super(context);
    }

    public SceneNaviLastMileView(Context context, AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviLastMileView(Context context, @Nullable @org.jetbrains.annotations.Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_LAST_MILE;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_LAST_MILE, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_LAST_MILE, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_LAST_MILE, false);
        }
    }

    @Override
    protected SceneNaviLastMileViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviLastMileViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviLastMileImpl initSceneImpl() {
        return new SceneNaviLastMileImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviLastMile(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    @Override
    public boolean onTouchEvent(MotionEvent event) {
        return true;
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.checkLastMile(naviEtaInfo);
        }
    }
}
