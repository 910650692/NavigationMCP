package com.fy.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.fy.navi.scene.databinding.SceneNaviViaDetailViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviViaDetailImpl;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.NaviViaEntity;

public class SceneNaviViaDetailView extends NaviSceneBase<SceneNaviViaDetailViewBinding, SceneNaviViaDetailImpl> {

    public SceneNaviViaDetailView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviViaDetailView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviViaDetailView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_VIA_DETAIL;
    }

    @Override
    protected SceneNaviViaDetailViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviViaDetailViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviViaDetailImpl initSceneImpl() {
        return new SceneNaviViaDetailImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setViaDetail(mScreenViewModel);
    }

    @Override
    protected void initObserver() {

    }

    public void showViaDetail(boolean b) {
        if (null != mScreenViewModel) {
            mScreenViewModel.showViaDetail(b);
        }
    }

    public void updateNewestViaPoint(NaviViaEntity naviViaEntity) {
        if (null != mScreenViewModel) {
            mScreenViewModel.updateNewestViaPoint(naviViaEntity);
        }
    }
}
