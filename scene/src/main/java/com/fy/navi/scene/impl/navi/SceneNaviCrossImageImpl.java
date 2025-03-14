package com.fy.navi.scene.impl.navi;

import android.view.View;

import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.common.AutoUIViewRect;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.impl.navi.inter.RectChangeListener;
import com.fy.navi.scene.ui.navi.SceneNaviCrossImageView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviCrossImageImpl extends BaseSceneModel<SceneNaviCrossImageView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private final LayerPackage mLayerPackage;
    private NaviEtaInfo mCurNaviInfo;
    /**
     * 是否正在显示路口大图
     */
    public boolean mIsShowCrossImage;
    /**
     * 路口大图分段进度信息记录
     */
    private final List<CrossProgressInfo> mCrossProgressInfos = new ArrayList<>();
    /**
     * 路口大图数据缓存
     */
    private CrossImageEntity mRoadCrossInfo;

    public SceneNaviCrossImageImpl(SceneNaviCrossImageView mScreenView) {
        super(mScreenView);
        mNaviPackage = NaviPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        init();
    }

    private void init() {
        Logger.i(TAG, "init()");
        mScreenView.setRectChange2DRoadCross(new RectChangeListener() {
            @Override
            public void onRectChange(View view, AutoUIViewRect newRect, AutoUIViewRect oldRect) {
                if (newRect == null) {
                    return;
                }
                Logger.i(TAG, "newRect " + newRect + ",mIsShowCrossImage：" + mIsShowCrossImage);
                // 当UI区域首次显示时，需要主动触发BL显示路口大图（2D矢量路口大图第一次显示的时候默认隐藏）
                if (mIsShowCrossImage && mRoadCrossInfo.getType() != NaviConstant.CrossType.CrossType3D) {
                    mNaviPackage.setRoadCrossRect(mMapTypeId, newRect.getLocationOnScreen());
                    if (!mLayerPackage.showCross(mMapTypeId, mRoadCrossInfo)) {
                        if (mRoadCrossInfo != null) {
                            onCrossImageInfo(false, mRoadCrossInfo);
                        }
                    }
                }
            }
        });
        // 初始默认关闭，有数据后会打开
        notifySceneStateChange(false);
    }

    public void registerObserver() {
//        mLayerPackage.setVisible(mMapTypeId, NaviConstant.CrossType.CrossTypeGrid, false);
//        mLayerPackage.setVisible(mMapTypeId, NaviConstant.CrossType.CrossTypeVector, false);
//        mLayerPackage.setVisible(mMapTypeId, NaviConstant.CrossType.CrossType3D, false);
    }

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
        public long startPoint;
        public long endPoint;

        public CrossProgressInfo(long start, long end) {
            startPoint = start;
            endPoint = end;
        }
    }

    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        mCurNaviInfo = naviInfoBean;
        if (null == mCurNaviInfo) {
            return;
        }
        long routeRemainDist = mCurNaviInfo.allDist;
        // 当显示路口大图时刷新进度条
        if (mIsShowCrossImage) {
            for (CrossProgressInfo progressInfo : mCrossProgressInfos) {
                if (progressInfo.endPoint <= routeRemainDist && routeRemainDist <= progressInfo.startPoint &&
                        progressInfo.startPoint - progressInfo.endPoint > 0) {
                    long progress = (progressInfo.startPoint - routeRemainDist) * 100 / (progressInfo.startPoint - progressInfo.endPoint);
                    mScreenView.setProgress2DRoadCross((int) progress);
                    break;
                }
            }
        }
    }

    public void onCrossImageInfo(boolean isShowImage, CrossImageEntity naviImageInfo) {
        if (isShowImage) {
            if (naviImageInfo == null) {
                Logger.e("SceneNaviCrossImageImpl onShowCrossImage, info is null");
                return;
            }
            // 样板间不显示3D路口大图，避免效果与车道级冲突
            Logger.e(TAG, "mIsShowCrossImage= " + mIsShowCrossImage + ", info.type= " + naviImageInfo.getType());
            if (mIsShowCrossImage) {
                return;
            }
            // 首先切换路口大图标记
            mIsShowCrossImage = true;
            mRoadCrossInfo = naviImageInfo;
            // 显示路口大图
            setRoadCrossVisible(true);
            // 近接/混淆路口大图控制（内聚方式）
            mLayerPackage.setCrossImageInfo(mMapTypeId, naviImageInfo);
        } else {
            Logger.d(TAG, "SceneNaviCrossImageImpl onHideCrossImage: type = " + naviImageInfo.getType());
            mIsShowCrossImage = false;
            notifySceneStateChange(false);
            mScreenView.setProgress2DRoadCross(0);
            mLayerPackage.hideCross(mMapTypeId, naviImageInfo.getType());
            mRoadCrossInfo = null;
        }
    }

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
     */
    public boolean setRoadCrossVisible(boolean visible) {
        if (mRoadCrossInfo == null) {
            return false;
        }
        Logger.i(TAG, "mRoadCrossInfo.getType()= " + mRoadCrossInfo.getType() + ",visible= " + visible);
        mIsShowCrossImage = visible;
        if (visible) {
            if (mRoadCrossInfo.getType() != NaviConstant.CrossType.CrossType3D) {
                notifySceneStateChange(true);
                mLayerPackage.showCross(mMapTypeId, mRoadCrossInfo);
            }
        } else {
            notifySceneStateChange(false);
            mLayerPackage.setVisible(mMapTypeId, mRoadCrossInfo.getType(), false);
            mScreenView.setProgress2DRoadCross(0);
        }
        return true;
    }

    /***设置2D路口大图区域可见性***/
    public void notifySceneStateChange(boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_2D_CROSS);
    }
}
