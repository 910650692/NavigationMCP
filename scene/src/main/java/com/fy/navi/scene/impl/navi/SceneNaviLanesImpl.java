package com.fy.navi.scene.impl.navi;

import android.text.TextUtils;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.ui.navi.SceneNaviLanesView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.TBTLaneInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.ui.base.StackManager;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviLanesImpl extends BaseSceneModel<SceneNaviLanesView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_LANES_IMPL;
    private LaneInfoEntity mLaneInfo;

    public SceneNaviLanesImpl(final SceneNaviLanesView screenView) {
        super(screenView);
        MapPackage.getInstance();
    }

    /**
     * 路线信息回调.
     *
     * @param isShowLane false 导航过程中通知隐藏车道信息 与onShowNaviLaneInfo配对回调
     *                   true  导航过程中传出车道信息
     * @param laneInfo   车道信息
     */
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        this.mLaneInfo = laneInfo;
        Logger.i(TAG, "isShowLane:", isShowLane);
        updateSceneVisible(isShowLane);
        if (laneInfo == null) {
            Logger.w(TAG, "laneInfo == null");
        } else {
            Logger.w(TAG, "laneInfo.getBackLaneType() != null:",
                    (laneInfo.getBackLaneType() != null));
            if (laneInfo.getBackLaneType() != null) {
                Logger.w(TAG, "laneInfo.getBackLaneType()- size:",
                        (laneInfo.getBackLaneType().size()));
            }
        }
        if (ConvertUtils.isEmpty(mScreenView)) {
            return;
        }
        if (!isShowLane) {
            mLaneInfo = null;
        } else {
            if (isOnCruising()) {
                showLaneWhenCarOnCruising(laneInfo);
            } else {
                showLaneWhenCarOnNavigating(laneInfo);
            }
        }
    }

    /***
     * @param laneInfo
     * 导航态下更新车道线
     */
    private void showLaneWhenCarOnNavigating(final LaneInfoEntity laneInfo) {
        Logger.d(TAG, "showLaneWhenCarOnNavigating");
        if (laneInfo != null && laneInfo.getBackLaneType() != null && !laneInfo.getBackLaneType().isEmpty()) {
            // 背景车道类型为空不处理
            final int LaneNum = laneInfo.getBackLane().size();
            // 车道线可见
//                mScreenView.setVisibleLaneInfo(true);
            final List<TBTLaneInfo> listData = new ArrayList<TBTLaneInfo>();
            Logger.i(TAG, "SceneNaviLanesImpl onShowNaviLaneInfo LaneNum=", LaneNum,
                    ",optimalLane=", laneInfo.getOptimalLane(), ",backLane=",
                    laneInfo.getBackLane(), ",frontLane=", laneInfo.getFrontLane(),
                    ",backLaneType=", laneInfo.getBackLaneType(), ",frontLaneType=",
                    laneInfo.getFrontLaneType(), ",extensionLane=",
                    laneInfo.getExtensionLane(), ",backExtenLane=",
                    laneInfo.getBackExtenLane());
            for (int i = 0; i < LaneNum; i++) {
                final TBTLaneInfo info = new TBTLaneInfo();
                // 设置推荐车道
                info.setRecommend(laneInfo.getOptimalLane().get(i) !=
                        NaviConstant.LaneAction.LANE_ACTION_NULL);
                // 设置分时车道
                if (NaviConstant.LANE_TYPE_BUS == laneInfo.getBackLaneType().get(i)) {
                    info.setTimeLane(true);
                    info.setTimeLaneAction(laneInfo.getFrontLane().get(i) ==
                            0xFF ? NaviConstant.LaneActionConstants.BACK_LANE_BUS_NO_WORKABLE :
                            NaviConstant.LaneActionConstants.BACK_LANE_BUS_WORKABLE);
                } else if (NaviConstant.LANE_TYPE_OTHER == laneInfo.getBackLaneType().get(i)) {
                    info.setTimeLane(true);
                    info.setTimeLaneAction(laneInfo.getFrontLane().get(i) ==
                            0xFF ? NaviConstant.LaneActionConstants.BACK_LANE_SPECIAL_NO_WORKABLE :
                            NaviConstant.LaneActionConstants.BACK_LANE_SPECIAL_WORKABLE);
                } else if (NaviConstant.LANE_TYPE_TIDAL == laneInfo.getBackLaneType().get(i)) {
                    info.setTimeLane(true);
                    info.setTimeLaneAction(laneInfo.getFrontLane().get(i) ==
                            0xFF ? NaviConstant.LaneActionConstants.BACK_LANE_TIDAL_NO_WORKABLE :
                            NaviConstant.LaneActionConstants.BACK_LANE_TIDAL_WORKABLE);
                } else if (NaviConstant.LANE_TYPE_VARIABLE == laneInfo.getBackLaneType().get(i)) {
                    info.setTimeLane(true);
                    info.setTimeLaneAction(laneInfo.getFrontLane().get(i) ==
                            0xFF ?
                            NaviConstant.LaneActionConstants.BACK_LANE_REVERSIBLE_NO_WORKABLE :
                            NaviConstant.LaneActionConstants.BACK_LANE_REVERSIBLE_WORKABLE);
                } else {
                    info.setTimeLane(false);
                }
                // 设置车道线图标
                for (int k = 0; k < NaviConstant.LaneActionConstants.NAVI_LANE_MAP.length; k++) {
                    if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][0] == laneInfo.getBackLane().get(i)) {
                        if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][1] == laneInfo.getFrontLane().get(i)) {
                            info.setLaneAction(NaviConstant.
                                    LaneActionConstants.NAVI_LANE_MAP[k][2]);
                            break;
                        }
                    }
                }
                listData.add(info);
            }
            showLaneInfoDefault(listData);
        }
    }

    /***
     * 巡航显示车道线
     * @param laneInfo
     */
    private void showLaneWhenCarOnCruising(final LaneInfoEntity laneInfo) {
        Logger.d(TAG, "showLaneWhenCarOnCruising-laneInfo is null:",
                ConvertUtils.isNull(laneInfo));
        if (laneInfo != null && !ConvertUtils.isEmpty(laneInfo.getBackLane())) {
            // 背景车道类型为空不处理
            final int LaneNum = laneInfo.getBackLane().size();
            final List<TBTLaneInfo> listData = new ArrayList();
            for (int i = 0; i < LaneNum; i++) {
                final TBTLaneInfo info = new TBTLaneInfo();
                // 设置车道线图标
                for (int k = 0; k < NaviConstant.LaneActionConstants.NAVI_LANE_MAP.length; k++) {
                    if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][0] == laneInfo.getBackLane().get(i)) {
                        if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][1] == laneInfo.getFrontLane().get(i)) {
                            info.setLaneAction(NaviConstant.
                                    LaneActionConstants.NAVI_LANE_MAP[k][2]);
                            break;
                        }
                    }
                }
                listData.add(info);
            }
            showLaneInfoDefault(listData);
            Logger.i(TAG, "cruise update lanes success!");
        }
    }

    /**
     * 展示收费站信息.
     *
     * @param tollGateInfo 服务区收费站信息
     */
    public void onShowTollGateLane(final SapaInfoEntity tollGateInfo) {
        if (ConvertUtils.isEmpty(mScreenView)) {
            return;
        }
        if (tollGateInfo == null || tollGateInfo.getLaneTypes() == null || tollGateInfo.getLaneTypes().isEmpty()) {
            Logger.i(TAG, "onShowTollGateLane null");
            if (mLaneInfo == null) {
                updateSceneVisible(false);
            } else {
                onLaneInfo(true, mLaneInfo);
            }
        } else {
            Logger.i(TAG, "onShowTollGateLane ", tollGateInfo.getLaneTypes());
            updateSceneVisible(true);
            final ArrayList<SceneCommonStruct.LaneAction> list = new ArrayList<>();
            for (int i = 0; i < tollGateInfo.getLaneTypes().size(); i++) {
                final Integer integer = tollGateInfo.getLaneTypes().get(i);
                if (integer == 0x02) {
                    list.add(SceneCommonStruct.LaneAction.LaneEtc);
                } else {
                    list.add(SceneCommonStruct.LaneAction.LaneLabor);
                }
            }
            showTollGateLane(list);
        }
    }

    /**
     * 显示默认道线.
     *
     * @param defaultListData 道线信息
     */
    private void showLaneInfoDefault(final List<TBTLaneInfo> defaultListData) {
        if (ConvertUtils.isEmpty(mScreenView)) {
            return;
        }
        mScreenView.sceneLaneInfoDefault();
        for (int index = 0; index < defaultListData.size(); index++) {
            final TBTLaneInfo itemData = defaultListData.get(index);
            mScreenView.setVisibleLaneDefault(index, true);
            // 车道图标的设置：1.推荐分时 2.推荐 3.分时 4.默认
            if (itemData.isRecommend() && itemData.isTimeLane()) {
                mScreenView.setBackgroundLanesDriveRecommendTimeArrow(index, SceneCommonStruct.LaneAction.get(itemData.getLaneAction()));
            } else if (itemData.isRecommend()) {
                mScreenView.setBackgroundLanesDriveRecommendArrow(index, SceneCommonStruct.LaneAction.get(itemData.getLaneAction()));
            } else if (itemData.isTimeLane()) {
                mScreenView.setBackgroundLanesDriveTimeArrow(index, SceneCommonStruct.LaneAction.get(itemData.getLaneAction()));
            } else {
                mScreenView.setBackgroundLanesDriveDefaultArrow(index, SceneCommonStruct.LaneAction.get(itemData.getLaneAction()));
            }

            if (itemData.isTimeLane()) {
                // 显示分时图标
                mScreenView.sceneBottom(index);
                // 分时图标的设置：1.推荐 2.默认
                if (itemData.isRecommend()) {
                    mScreenView.setBackgroundLanesDriveRecommendBottom(index, SceneCommonStruct.TimeLaneBottomAction.get(itemData.getTimeLaneAction()));
                    mScreenView.setVisibleHighlight(index, true);
                } else {
                    mScreenView.setBackgroundLanesDriveDefaultBottom(index, SceneCommonStruct.TimeLaneBottomAction.get(itemData.getTimeLaneAction()));
                    mScreenView.setVisibleHighlight(index, false);
                }
            } else {
                // 隐藏分时图标
                mScreenView.sceneCommonArrow(index);
                if (itemData.isRecommend()) {
                    mScreenView.setVisibleHighlight(index, true);
                } else {
                    mScreenView.setVisibleHighlight(index, false);
                }
            }
        }
    }

    /**
     * @param list 显示收费站道线
     */
    private void showTollGateLane(final List<SceneCommonStruct.LaneAction> list) {
        mScreenView.sceneLaneInfoDefault();
        for (int index = 0; index < list.size(); index++) {
            mScreenView.setVisibleLaneDefault(index, true);
            mScreenView.setBackgroundLanesDriveDefaultArrow(index, list.get(index));
            // 隐藏分时图标和推荐车道背景
            mScreenView.sceneCommonArrow(index);
            mScreenView.setVisibleHighlight(index, false);
        }
    }

    /**
     * @param isVisible 是否可见
     */
    public void updateSceneVisible(final boolean isVisible) {
        Logger.i(TAG, "updateSceneVisible ", isVisible, " mScreenView.isVisible():",
                mScreenView.isVisible());
        if (mScreenView.isVisible() == isVisible) return;
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_LANES);
    }

    private boolean isOnCruising() {
        final String currentStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        final boolean isFragmentStackEmpty = StackManager.getInstance().isFragmentStackNull(mMapTypeId.name());
        Logger.d(TAG, "isOnCruising", "currentStatus:", currentStatus,
                "isFragmentStackEmpty:", isFragmentStackEmpty);
        return TextUtils.equals(currentStatus, NaviStatus.NaviStatusType.CRUISE) || isFragmentStackEmpty;
    }
}
