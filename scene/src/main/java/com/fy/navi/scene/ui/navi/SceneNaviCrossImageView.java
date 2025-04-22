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
import com.fy.navi.scene.ui.navi.manager.NaviSceneBase;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.define.navi.CrossImageEntity;

/**
 * 路口大图scene
 *
 * @author yf
 * @version $Revision.*$
 */
public class SceneNaviCrossImageView extends NaviSceneBase<SceneNaviCrossImageViewBinding, SceneNaviCrossImageImpl> {
    private static final String TAG = "SceneNaviCrossImageView";
    private ViewRectChangeWatcher mViewRectChangeWatcher;

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
    }

    @Override
    protected NaviSceneId getSceneId() {
        return NaviSceneId.NAVI_SCENE_2D_CROSS;
    }

    @Override
    public void hide() {
        super.hide();
        mScreenViewModel.hideCross();
    }

    @Override
    public void show() {
        super.show();
        mScreenViewModel.setRoadCrossVisible(true);
    }

    /**
     * @param routeRemainDist 剩余距离
     */
    public void updateCrossProgress(final long routeRemainDist) {
        Logger.i(TAG, "updateCrossProgress");
        if (mScreenViewModel != null) {
            mScreenViewModel.updateCrossProgress(routeRemainDist);
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
     * @param isShowImage   是否显示图片
     * @param naviImageInfo 路口大图信息
     */
    public void onCrossImageInfo(final boolean isShowImage,
                                 final CrossImageEntity naviImageInfo) {
        Logger.i(TAG, "onCrossImageInfo", "isShowImage:" + isShowImage);
        if (mScreenViewModel != null) {
            mScreenViewModel.onCrossImageInfo(isShowImage, naviImageInfo);
        }
    }

    @Override
    public void onDestroy() {
        mScreenViewModel.unregisterObserver();
        mScreenViewModel.closeCrossVisible();
        if (mViewRectChangeWatcher != null) {
            mViewRectChangeWatcher.destroy();
            mViewRectChangeWatcher = null;
        }
        super.onDestroy();
    }


    /**
     * 设置2D路口大图位置大小变化事件监听
     *
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
     *
     * @param progress 进度
     */
    public void setProgress2DRoadCross(final int progress) {
        Logger.i(TAG, "SceneNaviCrossImageView progress：" + progress);
    }

    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (mScreenViewModel != null) {
            mScreenViewModel.onImmersiveStatusChange(currentImersiveStatus);
        }
    }
}
