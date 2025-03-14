package com.fy.navi.scene.ui.navi;

import static com.fy.navi.scene.ui.navi.manager.NaviSceneId.NAVI_SCENE_2D_CROSS;

import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviCrossImageViewBinding;
import com.fy.navi.scene.impl.navi.SceneNaviCrossImageImpl;
import com.fy.navi.scene.impl.navi.common.ViewRectChangeWatcher;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.navi.inter.RectChangeListener;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;

/**
 * 路口大图scene
 */
public class SceneNaviCrossImageView extends NaviSceneBase<SceneNaviCrossImageViewBinding, SceneNaviCrossImageImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ViewRectChangeWatcher mViewRectChangeWatcher;
    private ISceneCallback mISceneCallback;

    public SceneNaviCrossImageView(@NonNull Context context) {
        super(context);
    }

    public SceneNaviCrossImageView(@NonNull Context context, @Nullable AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviCrossImageView(@NonNull Context context, @Nullable AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NAVI_SCENE_2D_CROSS;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(NAVI_SCENE_2D_CROSS, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_2D_CROSS, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NAVI_SCENE_2D_CROSS, false);
        }
    }

    @Override
    protected SceneNaviCrossImageViewBinding createViewBinding(LayoutInflater inflater, ViewGroup viewGroup) {
        return SceneNaviCrossImageViewBinding.inflate(inflater, viewGroup, true);
    }

    @Override
    protected SceneNaviCrossImageImpl initSceneImpl() {
        return new SceneNaviCrossImageImpl(this);
    }

    @Override
    protected void setInitVariableId() {
        mViewBinding.setNaviCrossImage(mScreenViewModel);
    }

    @Override
    protected void initObserver() {
        mScreenViewModel.registerObserver();
    }

    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    public void onNaviStop() {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviStop();
        }
    }

    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        mScreenViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
    }

    @Override
    public void onDestroy() {
        mScreenViewModel.unregisterObserver();
        if (mViewRectChangeWatcher != null) {
            mViewRectChangeWatcher.destroy();
            mViewRectChangeWatcher = null;
        }
        super.onDestroy();
    }


    /**
     * 设置2D路口大图位置大小变化事件监听
     */
    public void setRectChange2DRoadCross(RectChangeListener rectChangedListener) {
        if (mViewRectChangeWatcher != null) {
            mViewRectChangeWatcher.destroy();
        }
        if (rectChangedListener != null) {
            mViewRectChangeWatcher = new ViewRectChangeWatcher(this, rectChangedListener);
        }
    }

    /**
     * 设置2D路口大图进度
     */
    public void setProgress2DRoadCross(int progress) {
        Logger.i(TAG, "SceneNaviCrossImageView progress：" + progress);
    }

    /**
     * 设置路口大图区域点击事件监听
     */
    public void setOnClickRoadCross(OnClickListener listener) {
    }

    @Override
    public void addSceneCallback(ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }
}
