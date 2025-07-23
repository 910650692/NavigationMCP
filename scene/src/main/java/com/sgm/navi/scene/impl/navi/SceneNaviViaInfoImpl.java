package com.sgm.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.ui.navi.SceneNaviViaInfoView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneManager;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;

public class SceneNaviViaInfoImpl extends BaseSceneModel<SceneNaviViaInfoView> {
    public static final String TAG = MapDefaultFinalTag.NAVI_SCENE_VIA_INFO_IMPL;
    private final RoutePackage mRoutePackage;
    private NaviEtaInfo mNaviEtaInfo;
    private ScheduledFuture mScheduledFuture;
    private int mTimes = NumberUtils.NUM_5;
    private boolean mIsCanUpdateViaInfo = true;

    public SceneNaviViaInfoImpl(final SceneNaviViaInfoView screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        cancelTimer();
    }

    /**
     * @param naviEtaInfo naviEtaInfo
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        mNaviEtaInfo = naviEtaInfo;
        if (mCallBack != null && mCallBack.getIsViaArrived()) {
            if (!ConvertUtils.isEmpty(mNaviEtaInfo.viaRemain) &&
                    mNaviEtaInfo.viaRemain.size() <= 1) {
                return;
            }
        }
        checkWaypointInfo(naviEtaInfo);
    }

    /**
     * 途径点info
     * @param naviEtaInfo naviEtaInfo
     **/
    private void checkWaypointInfo(final NaviEtaInfo naviEtaInfo) {
        if (!mIsCanUpdateViaInfo) {
            return;
        }
        // 没有途经点的情况
        if (ConvertUtils.isEmpty(naviEtaInfo.viaRemain)) {
            updateSceneVisible(false);
            return;
        }
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        String viaName = "";
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            Logger.i(TAG, "checkWaypointInfo =  ", "allPoiParamList.size() = ",
                    allPoiParamList.size());
        } else {
            Logger.i(TAG, "checkWaypointInfo = ", "allPoiParamList is null or empty");
        }
        // 因为要去除掉出发点和终点，所以大于2
        if (allPoiParamList.size() > 2) {
            // 当前的途经点
            updateViaInfo();
            updateSceneVisible(true);
        }
    }

    /**
     * @param isVisible isVisible
     */
    private void updateSceneVisible(final boolean isVisible) {
        if (mScreenView == null) {
            Logger.i(TAG, "updateSceneVisible mScreenView is null");
            return;
        }
        Logger.i(TAG, "updateSceneVisible isVisible = ", isVisible,
                " mScreenView.isVisible() = ", mScreenView.isVisible());
        if (!isVisible) {
            mScreenView.getNaviSceneEvent().notifySceneStateChange(
                    INaviSceneEvent.SceneStateChangeType.SceneCloseState, NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO);
        }
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO);
    }


    /**
     * 更新途经点到达信息
     * @param viaIndex 途径点索引 如果是-1的话是手动点击提前确认的场景
     */
    public void onUpdateViaPass(final long viaIndex) {
        Logger.i(TAG, "onUpdateViaPass viaIndex = ", viaIndex);
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() >= 3) {
            mScreenView.onArriveVia(allPoiParamList.get(1).getName());
            initTimer();
        }
    }

    /**
     * 更新途经点显示信息
     */
    private void updateViaInfo() {
        Logger.i(TAG, "updateViaInfo ");
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() > 2) {
            mScreenView.updateViaInfo(getViaName(),
                    allPoiParamList.size() - NumberUtils.NUM_2);
        } else {
            Logger.i(TAG, "updateViaInfo allPoiParamList is empty or size <= 2");
            updateSceneVisible(false);
        }
    }

    /**
     * @return 途经点名称
     */
    private String getViaName() {
        try {
            final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
            if (allPoiParamList.size() > 2) {
                return allPoiParamList.get(1).getName();
            }
        } catch (Exception e) {
            Logger.e(TAG, "getViaName error", e.getMessage());
        }
        return "";
    }

    /**
     * 隐藏到达弹窗
     */
    private void hideViaArrivedPop() {
        Logger.i(TAG, "SceneNaviViaInfoImpl", false);
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneCloseState, NaviSceneId.NAVI_VIA_ARRIVED_POP);
    }

    public void refreshViaInfo() {
        if (ConvertUtils.isEmpty(mNaviEtaInfo)) {
            Logger.i(TAG, " mNaviEtaInfo is null");
            return;
        }
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = mNaviEtaInfo.viaRemain;
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            if (allPoiParamList.size() > 2) {
                final String viaName = allPoiParamList.get(1).getName();
                mScreenView.updateViaInfo(viaName, viaRemain.size());
            } else {
                updateSceneVisible(false);
            }
        }
    }

    /**
     * 开始倒计时
     */
    public void initTimer() {
        Logger.i(TAG, "initTimer");
        mIsCanUpdateViaInfo = false;
        cancelTimer();
        mTimes = NumberUtils.NUM_5;
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(() -> {
            if (mTimes == NumberUtils.NUM_0) {
                ThreadManager.getInstance().postUi(new Runnable() {
                    @Override
                    public void run() {
                        try {
                            mIsCanUpdateViaInfo = true;
                            updateViaInfo();
                        } catch (Exception e) {
                            Logger.e(TAG, "initTimer error", e.getMessage());
                        }
                    }
                });
            }
            mTimes--;
        }, NumberUtils.NUM_0, NumberUtils.NUM_1);
    }

    /**
     * 取消倒计时
     */
    public void cancelTimer() {
        Logger.i(TAG, "cancelTimer");
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }
}
