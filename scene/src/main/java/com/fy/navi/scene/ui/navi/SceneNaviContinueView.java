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
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;

public class SceneNaviContinueView extends NaviSceneBase<SceneNaviContinueViewBinding,
        SceneNaviContinueImpl> {

    public static final String TAG = "SceneNaviContinueView";

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

    /**
     * @param currentImersiveStatus status
     */
    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }

    /**
     * 判断返回到导航页面
     */
    public void backToNaviFragment() {
        mISceneCallback.backToNaviFragment();
    }
}
