package com.fy.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.ui.navi.SceneNaviTmcView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.ui.action.Action;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviTmcImpl extends BaseSceneModel<SceneNaviTmcView> implements NetWorkUtils.NetworkObserver {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_TMC;
    private boolean mNetworkConnected = true;
    /**
     * 途径点信息
     */
    private NaviEtaInfo mCurNaviInfo;
    /**
     * 光柱图信息
     */
    private NaviTmcInfo.NaviLightBarDetail mLightBarDetail;
    /**
     * 光柱图信息
     */
    private ArrayList<NaviTmcInfo.NaviTmcInfoData> mTmcItemsInTmcBarNew;
    /**
     * < tmcBar累积的总距离，注意与当前路线长度不同，重算后_totalDistance不是新路线的长度，
     * 而是_distanceHasPassed加上新路线长度，避免重算后tmcBar回到起点
     */
    private long mTotalDistance;
    /**
     * tmcBar累积的已走距离,不会清零
     */
    private long mDistanceHasPassed;
    /**
     * 光柱图信息回调时保存上一次已经走过的距离
     */
    private long mLastDistanceHasPassed;
    /**
     * 是否更新光柱图 onUpdateNaviInfo比onUpdateTMCLightBar回调会晚一点，导致途径点和距离数据不同步
     */
    private boolean mUpdateTMCInfoDistance = false;

    public SceneNaviTmcImpl(final SceneNaviTmcView screenView) {
        super(screenView);
    }

    /**
     * 初始化网络监听
     */
    public void registerObserver() {
//        networkConnected = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        mScreenView.initTmcContainer(!mNetworkConnected);
//        NetWorkUtils.Companion.getInstance().registerNetworkObserver(this);
    }

    /**
     * 取消网络监听
     */
    public void unregisterObserver() {
//        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(this);
        // 是否使用光柱图：0-鹰眼，1-光柱图
//        if (mMspUser.getIntValue(MapSharePreference.SharePreferenceKeyEnum.setting, 1) == 1) {
//            hideTMCBar();
//        }
    }

    /**
     * 显示TMC
     */
    public void showTMCBar() {
//        if (!mIsEnterLane) {
        // 不在车道级导航时需要同时显示起点终点线
//            LayerController.getInstance().getRouteResultLayer(super.mSurfaceViewId).setVisible(BizRouteType.BizRouteTypeStartEndLine, true);
//        }
//        if (networkConnected) {
//        mSceneTMCInfo.setVisibleTmc(true);
//        } else {
//            mSceneTMCInfo.setVisibleTMC(false);
//        }
    }

    /**
     * 隐藏TMC
     */
    public void hideTMCBar() {
//        mSceneTMCInfo.setVisibleTmc(false);
//        mHMITripMapElementsRoadModeTmc = null;
//        mHMITripMapElementsRoadModeTmcResources = null;
//        LayerController.getInstance().getRouteResultLayer(super.mSurfaceViewId).setVisible(BizRouteType.BizRouteTypeStartEndLine, false);
    }

//    /**
//     * 更新路线结果数据
//     *
//     * @param routeResultData 路线结果数据
//     */
//    public void updateRouteResultData(RouteCarResultData routeResultData) {
//        mRouteCarResultData = routeResultData;
//        if (!networkConnected) {
//            PathInfo focusPathInfo = RouteCarResultData.getFocusPathInfo(mRouteCarResultData);
//            long focusPathInfoLength = focusPathInfo.getLength();
//            mTotalDistance = focusPathInfoLength + mDistanceHasPassed;
//        }
//    }

    public Action rootViewClick = () -> Logger.i(TAG, "Tmc点击事件拦截");

    /**
     * @param naviInfoBean 导航信息
     */
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        if (naviInfoBean != null) {
            mCurNaviInfo = naviInfoBean;
        }
        innerUpdateNaviInfo();
        if (!mNetworkConnected || mLightBarDetail == null) {
            updateTmcOffline();
        }
    }

    /**
     * @param naviTmcInfo TMC信息
     */
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        if (ConvertUtils.isEmpty(naviTmcInfo)
                        || ConvertUtils.isEmpty(naviTmcInfo.getLightBarInfo())
                        || ConvertUtils.isEmpty(naviTmcInfo.getLightBarDetail())) {
            Logger.e(TAG, "onUpdateTMCLightBar failed, data is empty!");
            return;
        }
        mLightBarDetail = naviTmcInfo.getLightBarDetail();
        innerUpdateTMCLightBar();
    }

    /**
     * 更新途经点数据
     */
    public void innerUpdateNaviInfo() {
//        Logger.i(TAG, "Navi_Tmc_cmpt innerUpdateNaviInfo: viaRemain:" + mCurNaviInfo.viaRemain + ",ChargeStationRemain:" +
//                mCurNaviInfo.ChargeStationRemain);
        if (ConvertUtils.isEmpty(mCurNaviInfo)) {
            return;
        }
        mScreenView.updateTmcVia(mCurNaviInfo.viaRemain, mCurNaviInfo.ChargeStationRemain);
        if (mUpdateTMCInfoDistance) {
            mUpdateTMCInfoDistance = false;
            updateTmcNew(mTmcItemsInTmcBarNew, mDistanceHasPassed, mTotalDistance, false);
        }
    }

    /**
     * 更新离线数据
     */
    private void updateTmcOffline() {
        if (mCurNaviInfo.getRemainDist() < mTotalDistance) {
            final long currentHasPassed = mTotalDistance - mCurNaviInfo.getRemainDist();
            if (currentHasPassed > mDistanceHasPassed) {
                mLastDistanceHasPassed = currentHasPassed - mDistanceHasPassed;
                mDistanceHasPassed += mLastDistanceHasPassed;
            }
            updateTmcNew(null, mDistanceHasPassed, mTotalDistance, false);
        }
    }

    /**
     * 更新柱状图数据
     */
    private void innerUpdateTMCLightBar() {
        if (mLightBarDetail == null || mLightBarDetail.getTmcInfoData() == null ||
                mLightBarDetail.getTmcInfoData().size() <= 0) {
            Logger.e(TAG, "innerUpdateTMCLightBar failed!");
            return;
        }
        /** 光柱图功能，目前与鹰眼图只同时一个,目前显示的是鹰眼图，光柱图暂时关闭，根据产品需要可以开启,当前设置为false,可在GuideConstants开启  */
        //获取当前设置的主路线pathInfo,mRouteCarResultData.getFocusIndex()是当前引导的主路线索引
//        Logger.i(TAG, "Navi_Tmc_cmpt innerUpdateTMCLightBar finishDistance = {?}, restDistance = {?}, totalDistance = {?}",
//                mLightBarDetail.getFinishDistance(), mLightBarDetail.getFinishDistance(), mLightBarDetail.getTotalDistance());
        //计算光柱图数据
        mTmcItemsInTmcBarNew = mLightBarDetail.getTmcInfoData();
        //获取路线长度
        // 刷新光柱图
        if (mTmcItemsInTmcBarNew != null) {
            int totalDistance = 0;
            int finishDistance = 0;
            for (int i = 0; i < mTmcItemsInTmcBarNew.size(); i++) {
                totalDistance += mTmcItemsInTmcBarNew.get(i).getDistance();
                if (mTmcItemsInTmcBarNew.get(i).getStatus() == 10) {
                    finishDistance = mTmcItemsInTmcBarNew.get(i).getDistance();
                }
            }
/*            Logger.i(TAG, "Navi_Tmc_cmpt innerUpdateTMCLightBar finishDistance = {?}, " +
                            "totalDistance = {?}, mTotalDistance={?} , mDistanceHasPassed={?}",
                    finishDistance, totalDistance, mTotalDistance, mDistanceHasPassed);*/
            // 重新规划路线以后，需要保留已走过的距离
            if (totalDistance != (mTotalDistance - mDistanceHasPassed)) {
                mDistanceHasPassed += mLastDistanceHasPassed;
                mTotalDistance = totalDistance + mDistanceHasPassed;
                // 从新规划路线以后，先不更新，等待途径点数据更新后再更新
                mUpdateTMCInfoDistance = true;
            }
            if (!mUpdateTMCInfoDistance) {
                updateTmcNew(mTmcItemsInTmcBarNew, mDistanceHasPassed, mTotalDistance, false);
            }
            mLastDistanceHasPassed = mLightBarDetail.getFinishDistance();
        }
    }

    /**
     * 更新tmc控制栏
     * items柱状图数据
     * totalDistance 总距离
     * restDistance 已走距离
     * @param items           items
     * @param distanceHasPassed distanceHasPassed
     * @param totalDistance   totalDistance
     * @param reRouter reRouter
     */
    public void updateTmcNew(final List<NaviTmcInfo.NaviTmcInfoData> items,
                             final long distanceHasPassed,
                             final long totalDistance, final boolean reRouter) {
        updateSceneVisible(true);
        if (mNetworkConnected && (items == null || items.size() <= 0)) {
            return;
        }
        if (reRouter) {
            Logger.i(TAG, "updateTmcNew reRouter resetView");
            mScreenView.resetView();
        }
        // TODO 临时修复进度条方案，后续重新优化tmc, 存在TMC上的途经点icon频繁闪烁且位置频繁变更问题
        mScreenView.updateTmcContainerNew(items, 0, mLightBarDetail == null ? 0 : mLightBarDetail.getTotalDistance());
        mScreenView.updateTmcAreaNew(items, 0, mLightBarDetail == null ? 0 : mLightBarDetail.getTotalDistance());
//        setNightMode(NightModeGlobal.isNightMode());
    }

    @Override
    public void onNetConnectSuccess() {
        mNetworkConnected = true;
        mScreenView.setOffline(true);
    }

    @Override
    public void onNetDisConnect() {
        mNetworkConnected = false;
        mScreenView.setOffline(false);
    }

    @Override
    public void onNetUnavailable() {

    }

    @Override
    public void onNetBlockedStatusChanged() {

    }

    @Override
    public void onNetLosing() {

    }

    @Override
    public void onNetLinkPropertiesChanged() {

    }

    /**
     * 更新黑夜白天模式
     * @param isNightMode isNightMode
     */
    public void setNightMode(final boolean isNightMode) {
//        if (mHMITripMapElementsRoadModeTmc != null) {
//            mHMITripMapElementsRoadModeTmc.setNightMode(isNightMode);
//            mHMITripMapElementsRoadModeTmcResources.setNightMode(isNightMode);
//        }
    }

    /**
     * @param isVisible isVisible
     */
    private void updateSceneVisible(final boolean isVisible){
        if(mScreenView.isVisible() == isVisible) return;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviTmcImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_TMC);
    }
}
