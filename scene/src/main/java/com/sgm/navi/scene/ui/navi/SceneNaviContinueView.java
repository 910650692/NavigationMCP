package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.sgm.navi.scene.databinding.SceneNaviContinueViewBinding;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.navi.SceneNaviContinueImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.utils.NumberUtils;

public class SceneNaviContinueView extends NaviSceneBase<SceneNaviContinueViewBinding,
        SceneNaviContinueImpl> {

    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CONTINUE;

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
        setCategory(NumberUtils.NUM_1);
        addNaviScene();
    }

    @Override
    public NaviSceneId getSceneId() {
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

    /**
     * 配合语音实现的继续导航操作
     */
    public void naviContinueByVoice() {
        Logger.i(TAG, "naviContinueByVoice");
        if (null != mScreenViewModel) {
            mScreenViewModel.naviContinueClick();
        }
    }
}
