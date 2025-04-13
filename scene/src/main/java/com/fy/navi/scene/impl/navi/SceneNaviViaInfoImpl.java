package com.fy.navi.scene.impl.navi;


import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.inter.ISceneCallback;
import com.fy.navi.scene.ui.navi.SceneNaviViaInfoView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviViaInfoImpl extends BaseSceneModel<SceneNaviViaInfoView> {
    public static final String TAG = "SceneNaviViaInfoImpl";
    private final RoutePackage mRoutePackage;

    // 记录当前的途经点
    private long mViaIndex = -1;

    private NaviEtaInfo mNaviEtaInfo;

    // 记录最近的途经点来屏蔽重复操作
    private String mViaName;

    public SceneNaviViaInfoImpl(final SceneNaviViaInfoView screenView) {
        super(screenView);
        mRoutePackage = RoutePackage.getInstance();
        setScreenId(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

    /**
     * @param naviEtaInfo naviEtaInfo
     */
    public void onNaviInfo(final NaviEtaInfo naviEtaInfo) {
        mNaviEtaInfo = naviEtaInfo;
        checkWaypointInfo(naviEtaInfo);
    }

    /**
     * @param sceneCallback sceneCallback
     */
    public void addSceneCallback(final ISceneCallback sceneCallback) {
    }

    /**
     * 途径点info
     * @param naviEtaInfo naviEtaInfo
     **/
    private void checkWaypointInfo(final NaviEtaInfo naviEtaInfo) {
        // 没有途经点的情况
        if (ConvertUtils.isEmpty(naviEtaInfo.viaRemain)) {
            updateSceneVisible(false);
            return;
        }
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        String viaName = "";
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            Logger.i(TAG, "checkWaypointInfo =  ", "allPoiParamList.size() = " +
                    allPoiParamList.size());
        } else {
            Logger.i(TAG, "checkWaypointInfo = ", "allPoiParamList is null or empty");
        }
        // 因为要去除掉出发点和终点，所以大于2
        if (allPoiParamList.size() > 2) {
            // 当前的途经点
            viaName = getViaName();
            final ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = naviEtaInfo.viaRemain;
            Logger.i(TAG, "checkWaypointInfo ", "viaRemain.size() = " + viaRemain.size() +
                    " viaName = " + viaName);
            if (!ConvertUtils.isEmpty(viaRemain)) {
                final NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = viaRemain.get(0);
                final int dist = naviTimeAndDist.dist;
                Logger.i(TAG, "checkWaypointInfo ", "dist = " + dist);
                // 因为viaPass是先更新，途经点名称更新后距离又比较小会导致重复调用，这边距离加上上限
                if (dist <= 500 && dist > 200) {
                    onViaWaypoint(viaName);
                }
            }
            if (!viaName.equals(mViaName)) {
                updateViaInfo();
            }
            updateSceneVisible(true);
        }
        Logger.i(TAG, "ConvertUtils.isEmpty(naviEtaInfo.viaRemain) = " +
                ConvertUtils.isEmpty(naviEtaInfo.viaRemain));
    }

    /**
     * 经过途径点，距离途径点500米时触发
     * @param viaName
     **/
    public void onViaWaypoint(final String viaName) {
        if (viaName.equals(mViaName)) {
            return;
        }
        mViaName = viaName;
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviViaInfoImpl", true);
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneShowState,
                NaviSceneId.NAVI_VIA_ARRIVED_POP);
    }

    /**
     * @param isVisible isVisible
     */
    private void updateSceneVisible(final boolean isVisible) {
        Logger.i(TAG, "updateSceneVisible isVisible = " + isVisible +
                " mScreenView.isVisible() = " + mScreenView.isVisible());
        if(mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_VIA_DETAIL_INFO);
    }


    /**
     * 更新途经点到达信息
     * @param viaIndex 途径点索引 如果是-1的话是手动点击提前确认的场景
     */
    public void onUpdateViaPass(final long viaIndex) {
        Logger.i(TAG, "onUpdateViaPass viaIndex = " + viaIndex);
        hideViaArrivedPop();
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList) && allPoiParamList.size() >= 3) {
            mScreenView.onArriveVia(allPoiParamList.get(1).getName(), 0);
        }
    }

    /**
     * 更新途经点显示信息
     */
    private void updateViaInfo() {
        Logger.i(TAG, "updateViaInfo ");
        final List<RouteParam> allPoiParamList = mRoutePackage.getAllPoiParamList(mMapTypeId);
        if (!ConvertUtils.isEmpty(allPoiParamList)) {
            mScreenView.updateViaInfo(getViaName(),
                    mNaviEtaInfo.viaRemain.size());
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
        Logger.i(MapDefaultFinalTag.NAVI_SCENE_TAG, "SceneNaviViaInfoImpl", false);
        NaviSceneManager.getInstance().notifySceneStateChange(
                INaviSceneEvent.SceneStateChangeType.SceneCloseState, NaviSceneId.NAVI_VIA_ARRIVED_POP);
    }

    /**
     * 开始导航
     */
    public void startNavigation() {
        Logger.i(TAG, "startNavigation");
        mViaIndex = -1;
        mViaName = "";
    }
}
