package com.fy.navi.scene.ui.navi;
import android.content.Context;
import android.util.AttributeSet;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.log.Logger;
import com.fy.navi.scene.databinding.SceneNaviCrossImageViewBinding;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.SceneNaviCrossImageImpl;
import com.fy.navi.scene.impl.navi.common.ViewRectChangeWatcher;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.navi.inter.RectChangeListener;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;

/**
 * 路口大图scene
 * @author yf
 * @version $Revision.*$
 */
public class SceneNaviCrossImageView extends NaviSceneBase<SceneNaviCrossImageViewBinding, SceneNaviCrossImageImpl> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private ViewRectChangeWatcher mViewRectChangeWatcher;
    private ISceneCallback mISceneCallback;

    public SceneNaviCrossImageView(@NonNull final Context context) {
        super(context);
    }

    public SceneNaviCrossImageView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs) {
        super(context, attrs);
    }

    public SceneNaviCrossImageView(@NonNull final Context context,
                                   @Nullable final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_2D_CROSS;
    }

    @Override
    public INaviSceneEvent getNaviSceneEvent() {
        return NaviSceneManager.getInstance();
    }

    @Override
    protected void init() {
        NaviSceneManager.getInstance().addNaviScene(
                NaviSceneId.NAVI_SCENE_2D_CROSS, this);
    }

    @Override
    public void show() {
        super.show();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_2D_CROSS, true);
        }
    }

    @Override
    public void hide() {
        super.hide();
        if (mISceneCallback != null) {
            mISceneCallback.updateSceneVisible(NaviSceneId.NAVI_SCENE_2D_CROSS, false);
        }
    }

    @Override
    protected SceneNaviCrossImageViewBinding createViewBinding(final LayoutInflater inflater,
                                                               final ViewGroup viewGroup) {
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

    /**
     * @param naviETAInfo 导航eta信息
     */
    public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviInfo(naviETAInfo);
        }
    }

    /**
     * 导航停止
     */
    public void onNaviStop() {
        if (mScreenViewModel != null) {
            mScreenViewModel.onNaviStop();
        }
    }

    /**
     * @param isShowImage 是否显示图片
     * @param naviImageInfo 路口大图信息
     */
    public void onCrossImageInfo(final boolean isShowImage,
                                 final CrossImageEntity naviImageInfo) {
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
     * @param rectChangedListener 监听
     */
    public void setRectChange2DRoadCross(final RectChangeListener rectChangedListener) {
        if (mViewRectChangeWatcher != null) {
            mViewRectChangeWatcher.destroy();
        }
        if (rectChangedListener != null) {
            mViewRectChangeWatcher = new ViewRectChangeWatcher(this, rectChangedListener);
        }
    }

    /**
     * 设置2D路口大图进度
     * @param progress 进度
     */
    public void setProgress2DRoadCross(final int progress) {
        Logger.i(TAG, "SceneNaviCrossImageView progress：" + progress);
    }

    /**
     * 设置路口大图区域点击事件监听
     * @param listener 监听
     */
    public void setOnClickRoadCross(final OnClickListener listener) {
    }

    @Override
    public void addSceneCallback(final ISceneCallback sceneCallback) {
        mISceneCallback = sceneCallback;
    }

    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }
}
