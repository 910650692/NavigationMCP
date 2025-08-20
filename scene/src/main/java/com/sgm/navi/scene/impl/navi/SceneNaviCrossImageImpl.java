package com.sgm.navi.scene.impl.navi;

import android.graphics.Rect;
import android.view.View;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.SplitScreenManager;
import com.android.utils.log.Logger;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.navi.common.AutoUIViewRect;
import com.sgm.navi.scene.impl.navi.inter.RectChangeListener;
import com.sgm.navi.scene.ui.navi.SceneNaviCrossImageView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.NextManeuverEntity;
import com.android.utils.ScreenTypeUtils;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviCrossImageImpl extends BaseSceneModel<SceneNaviCrossImageView> implements ScreenTypeUtils.SplitScreenChangeListener {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_CROSS_IMAGE_IMPL;
    private final NaviPackage mNaviPackage;
    private final LayerPackage mLayerPackage;
    private final SplitScreenManager mSplitScreenManager;

    private ImersiveStatus mImersiveStatus;
    /**
     * 是否正在显示路口大图
     */
    private boolean mIsShowCrossImage;
    /**
     * 路口大图分段进度信息记录
     */
    private final List<CrossProgressInfo> mCrossProgressInfos = new ArrayList<>();
    /**
     * 路口大图数据缓存
     */
    private CrossImageEntity mRoadCrossInfo;
    public ObservableField<Boolean> mNextManeuverVisible;
    private boolean mIsSetRect;

    public SceneNaviCrossImageImpl(final SceneNaviCrossImageView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mSplitScreenManager = SplitScreenManager.getInstance();
        init();
        mNextManeuverVisible = new ObservableField<>(false);
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        ScreenTypeUtils.getInstance().addSplitScreenChangeListener(TAG, this);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        ScreenTypeUtils.getInstance().removeSplitScreenChangeListener(TAG);
    }

    /**
     * 初始化
     */
    private void init() {
        Logger.i(TAG, "init");
        setLeftPoint();
        // 初始默认关闭，有数据后会打开
        notifySceneStateChange(false);
    }

    private void setLeftPoint() {
        ResourceUtils instance = ResourceUtils.Companion.getInstance();
        int left = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_left);
        int right = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_right);
        int top = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_top);
        int bottom = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_bottom);
        Rect rect = new Rect(left, top, right, bottom);
        mNaviPackage.setRoadCrossRect(mMapTypeId, rect);
        mIsSetRect = true;
        showLayerCross();
    }

    public void setRightPoint() {
        ResourceUtils instance = ResourceUtils.Companion.getInstance();
        int left = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_split_left);
        int right = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_split_right);
        int top = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_top);
        int bottom = instance.getDimensionPixelSize(com.sgm.navi.ui.R.dimen.navi_cross_bottom);
        Rect rect = new Rect(left, top, right, bottom);
        mNaviPackage.setRoadCrossRect(mMapTypeId, rect);
        mIsSetRect = true;
        showLayerCross();
    }

    public void hideCross() {
        Logger.i(TAG, "hideCross");
        if (!ConvertUtils.isNull(mRoadCrossInfo)) {
            mLayerPackage.hideCross(mMapTypeId, mRoadCrossInfo.getType());
        } else {
            Logger.e(TAG, "mRoadCrossInfo = null");
        }
    }

    /**
     * 注销观察者
     */
    public void unregisterObserver() {
        // 确保路口大图期间退出导航时及时隐藏
        if (mRoadCrossInfo != null) {
            onCrossImageInfo(false, mRoadCrossInfo);
        }
    }

    @Override
    public void onSplitScreenChanged() {
        mSplitScreenManager.setSplitPos();
        int pos = mSplitScreenManager.getSplitPos();
        if (pos == NumberUtils.NUM_1) {
            setRightPoint();
        } else {
            setLeftPoint();
        }
    }

    private static class CrossProgressInfo {
        /**
         * 进度条分段位置起始点与结束点，其值对应NaviInfo中的路径剩余距离（routeRemain.dist)
         */
        private final long mStartPoint;
        private final long mEndPoint;

        public CrossProgressInfo(final long start, final long end) {
            mStartPoint = start;
            mEndPoint = end;
        }
    }

    /**
     * @param routeRemainDist 剩余长度
     */
    public void updateCrossProgress(final long routeRemainDist) {
        Logger.i(TAG, "2D routeRemainDist :", routeRemainDist, "mIsShowCrossImage:",
                mIsShowCrossImage);
        // 当显示路口大图时刷新进度条
        if (mIsShowCrossImage) {
            for (CrossProgressInfo progressInfo : mCrossProgressInfos) {
                if (progressInfo.mEndPoint <= routeRemainDist && routeRemainDist <= progressInfo.mStartPoint &&
                        progressInfo.mStartPoint - progressInfo.mEndPoint > 0) {
                    final long progress = (progressInfo.mStartPoint - routeRemainDist) * 100 / (progressInfo.mStartPoint - progressInfo.mEndPoint);
                    mScreenView.setProgress2DRoadCross((int) progress);
                    break;
                }
            }
        }
    }

    /**
     * @param isShowImage   是否显示路口大图
     * @param naviImageInfo 路口大图信息
     */
    public void onCrossImageInfo(final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        if (isShowImage) {
            if (naviImageInfo == null) {
                Logger.e("SceneNaviCrossImageImpl onShowCrossImage, info is null");
                return;
            }
            // 样板间不显示3D路口大图，避免效果与车道级冲突
            Logger.e(TAG, "mIsShowCrossImage= ", mIsShowCrossImage, ", info.type= ",
                    naviImageInfo.getType());
            if (mIsShowCrossImage) {
                return;
            }
            mRoadCrossInfo = naviImageInfo;
            // 显示路口大图
            setRoadCrossVisible(true);
        } else {
            Logger.d(TAG, "SceneNaviCrossImageImpl onHideCrossImage: type = ",
                    (naviImageInfo == null? "null" : naviImageInfo.getType()));
            mIsShowCrossImage = false;
            notifySceneStateChange(false);
            hideCross();
            mScreenView.setProgress2DRoadCross(0);
            mRoadCrossInfo = null;
        }
    }

    /**
     * 导航停止时，隐藏路口大图
     */
    public void onNaviStop() {
        if (mRoadCrossInfo != null) {
            onCrossImageInfo(false, mRoadCrossInfo);
        }
    }

    /**
     * 特殊场景下需要主动设置2D路口大图的可见性
     *
     * @param visible 2D路口大图的可见性
     * @return true表示成功，false表示失败
     */
    public boolean setRoadCrossVisible(final boolean visible) {
        if (mRoadCrossInfo == null) {
            return false;
        }
        Logger.i(TAG, "mRoadCrossInfo.getType()= ", mRoadCrossInfo.getType(), ",visible= ",
                visible);
        if (visible) {
            if (mRoadCrossInfo.getType() != NaviConstant.CrossType.CROSS_TYPE_3_D) {
                showLayerCross();
            }
        } else {
            notifySceneStateChange(false);
            hideCross();
            mScreenView.setProgress2DRoadCross(0);
        }
        return true;
    }

    /**
     * 显示图层中的路口大图
     */
    public void showLayerCross() {
        Logger.i(TAG, "mRoadCrossInfo:", mRoadCrossInfo, " mMapTypeId:", mMapTypeId,
                " mIsSetRect:", mIsSetRect);
        if (!mIsSetRect) {
            return;
        }
        LayerItemCrossEntity layerItemCrossEntity = new LayerItemCrossEntity();
        layerItemCrossEntity.setCrossImageEntity(mRoadCrossInfo);
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d(TAG, "1/3屏不显示路口大图！");
            return;
        }
        if (!mLayerPackage.showCross(mMapTypeId, layerItemCrossEntity)) {
            if (mRoadCrossInfo != null) {
                onCrossImageInfo(false, mRoadCrossInfo);
            }
        } else {
            mIsShowCrossImage = true;
            notifySceneStateChange(true);
            if (null != mCallBack) {
                NextManeuverEntity nextManeuverEntity = mCallBack.getNextManeuverEntity();
                if (nextManeuverEntity == null) {
                    return;
                }
                Logger.i(TAG, "下一个转向信息：", nextManeuverEntity.toString());
                if (nextManeuverEntity.isNextManeuverVisible()) {
                    if (nextManeuverEntity.isNextManeuverOffLine() &&
                            nextManeuverEntity.getNextIconResource() != -1) {
                        if (null != mScreenView) {
                            mScreenView.setNextIconResource(
                                    nextManeuverEntity.getNextIconResource());
                            mScreenView.setNextText(nextManeuverEntity.getNextText());
                        }
                        if (null != mNextManeuverVisible) {
                            mNextManeuverVisible.set(true);
                        }
                    } else if (!nextManeuverEntity.isNextManeuverOffLine() &&
                            nextManeuverEntity.getNextIconDrawable() != null) {
                        if (null != mScreenView) {
                            mScreenView.setNextIconBackground(
                                    nextManeuverEntity.getNextIconDrawable());
                            mScreenView.setNextText(nextManeuverEntity.getNextText());
                        }
                        if (null != mNextManeuverVisible) {
                            mNextManeuverVisible.set(true);
                        }
                    }
                } else {
                    if (null != mNextManeuverVisible) {
                        mNextManeuverVisible.set(false);
                    }
                }
            }
        }
    }

    /**
     * ######值提供给View的Hide和Close方法调用，其他方法请勿使用.
     */
    public void closeCrossVisible() {
        if (mRoadCrossInfo != null) {
            mIsShowCrossImage = false;
            notifySceneStateChange(false);
            hideCross();
            mScreenView.setProgress2DRoadCross(0);
        }
    }

    /**
     * 设置2D路口大图区域可见性
     *
     * @param isVisible 是否可见
     **/
    public void notifySceneStateChange(final boolean isVisible) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(TAG, "SceneNaviCrossImageImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChangeReset((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_2D_CROSS, true);
    }

    public void onImmersiveStatusChange(final ImersiveStatus currentImersiveStatus) {
        if (mImersiveStatus != currentImersiveStatus) {
            mImersiveStatus = currentImersiveStatus;
        } else {
            return;
        }
        //触碰态大图不消失
//        if (currentImersiveStatus == ImersiveStatus.TOUCH) {
//            setRoadCrossVisible(false);
//        }
    }
}
