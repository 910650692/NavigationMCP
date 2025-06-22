package com.sgm.navi.scene.ui.navi;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.sgm.navi.scene.databinding.NaviSceneServiceAreaBinding;
import com.sgm.navi.scene.impl.navi.SceneNaviDetailImpl;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneBase;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;

/**
 * 服务区、交通事件、收费站Scene
 * @author sgm
 * @version $Revision.*$
 */
public class SceneNaviServiceArea extends NaviSceneBase<NaviSceneServiceAreaBinding, SceneNaviDetailImpl> {

    public SceneNaviServiceArea(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviServiceArea(@NonNull final Context context,
                                @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviServiceArea(@NonNull final Context context, @Nullable final AttributeSet attrs,
                                final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    public NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_SERVICE_AREA;
    }

    @Override
    protected NaviSceneServiceAreaBinding createViewBinding(final LayoutInflater inflater,
                                                            final ViewGroup viewGroup) {
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
