package com.fy.navi.scene.impl.navi;

import android.view.View;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.navi.common.AutoUIViewRect;
import com.fy.navi.scene.impl.navi.inter.RectChangeListener;
import com.fy.navi.scene.ui.navi.SceneNaviCrossImageView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.layer.refix.LayerItemCrossEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviCrossImageImpl extends BaseSceneModel<SceneNaviCrossImageView> {
    private static final String TAG = "SceneNaviCrossImageView";
    private final NaviPackage mNaviPackage;
    private final LayerPackage mLayerPackage;

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

    public SceneNaviCrossImageImpl(final SceneNaviCrossImageView screenView) {
        super(screenView);
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        init();
    }

    /**
     * 初始化
     */
    private void init() {
        Logger.i(TAG, "init()");
        mScreenView.setRectChange2DRoadCross(new RectChangeListener() {
            @Override
            public void onRectChange(final View view, final AutoUIViewRect newRect,
                                     final AutoUIViewRect oldRect) {
                if (newRect == null || ConvertUtils.isNull(mRoadCrossInfo)) {
                    Logger.e(TAG, "newRect is null:" + (newRect == null), "mRoadCrossInfo is null:" + (mRoadCrossInfo == null));
                    return;
                }
                Logger.i(TAG, "newRect " + newRect + ",mIsShowCrossImage：" + mIsShowCrossImage, "is2D:" + (mRoadCrossInfo.getType() != NaviConstant.CrossType.CROSS_TYPE_3_D));
                // 当UI区域首次显示时，需要主动触发BL显示路口大图（2D矢量路口大图第一次显示的时候默认隐藏）
                if (mIsShowCrossImage && mRoadCrossInfo.getType() != NaviConstant.CrossType.CROSS_TYPE_3_D) {
                    mNaviPackage.setRoadCrossRect(mMapTypeId, newRect.getLocationOnScreen());
                    showLayerCross();
                }
            }
        });
        // 初始默认关闭，有数据后会打开
        notifySceneStateChange(false);
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
        Logger.i(TAG, "unregisterObserver()");
        // 确保路口大图期间退出导航时及时隐藏
        if (mRoadCrossInfo != null) {
            onCrossImageInfo(false, mRoadCrossInfo);
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
        Logger.i(TAG, "updateCrossProgress:" + routeRemainDist, "mIsShowCrossImage:" + mIsShowCrossImage);
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
            Logger.e(TAG, "mIsShowCrossImage= " + mIsShowCrossImage + ", info.type= " +
                    naviImageInfo.getType());
            if (mIsShowCrossImage) {
                return;
            }
            // 首先切换路口大图标记
            mIsShowCrossImage = true;
            mRoadCrossInfo = naviImageInfo;
            // 显示路口大图
            setRoadCrossVisible(true);
        } else {
            Logger.d(TAG, "SceneNaviCrossImageImpl onHideCrossImage: type = " + naviImageInfo.getType());
            mIsShowCrossImage = false;
            notifySceneStateChange(false);
            mScreenView.setProgress2DRoadCross(0);
            hideCross();
            mRoadCrossInfo = null;
        }
    }

    /**
     * 导航停止时，隐藏路口大图
     */
    public void onNaviStop() {
        Logger.e(TAG, "SceneNaviCrossImageImpl onNaviStop: id={?}, naviType={?}");
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
        Logger.i(TAG, "mRoadCrossInfo.getType()= " + mRoadCrossInfo.getType() + ",visible= " + visible);
        mIsShowCrossImage = visible;
        if (visible) {
            if (mRoadCrossInfo.getType() != NaviConstant.CrossType.CROSS_TYPE_3_D) {
                notifySceneStateChange(true);
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
        Logger.i(TAG, "showLayerCross()");
        LayerItemCrossEntity layerItemCrossEntity = new LayerItemCrossEntity();
        layerItemCrossEntity.setCrossImageEntity(mRoadCrossInfo);
        if (!mLayerPackage.showCross(mMapTypeId, layerItemCrossEntity)) {
            if (mRoadCrossInfo != null) {
                onCrossImageInfo(false, mRoadCrossInfo);
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
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_2D_CROSS);
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
