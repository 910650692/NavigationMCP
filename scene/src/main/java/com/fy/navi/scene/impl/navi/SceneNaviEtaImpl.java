package com.fy.navi.scene.impl.navi;


import android.graphics.Bitmap;
import android.text.TextUtils;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.impl.navi.common.AutoUIDrawable;
import com.fy.navi.scene.impl.navi.common.AutoUIString;
import com.fy.navi.scene.impl.navi.common.NaviUiUtil;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.ui.navi.SceneNaviEtaView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;

import java.util.Objects;

public class SceneNaviEtaImpl extends BaseSceneModel<SceneNaviEtaView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    // 转向图标信息
    private NaviManeuverInfo mManeuverInfo;
    // 是否离线请求本地导航转向图片信息
    private boolean mOfflineManeuverIcon = true;
    // 当前引导数据
    private NaviEtaInfo mCurNaviInfo;
    // 导航信息转向图标bitmap 缓存
    private Bitmap mNextThumDirectionCache;
    // 环岛数
    private int mRoundNum = 0;
    //近阶动作数
    private int mNextThumRoundNum = 0;
    // 进阶动作
    private NaviManeuverInfo mNextThumManeuverInfo = new NaviManeuverInfo();
    // 上一次进阶动作
    private NaviManeuverInfo mPreviousNextThumManeuverInfo = new NaviManeuverInfo();
    //  到达时间
    private String mArriveTime = "";
    // 到达天数
    private String mArriveDay = "";
    //  上一次显示的到达时间
    private String mLastArriveTime = "";
    //  剩余信息
    private String mRemainInfo = "";
    //  上一次显示的剩余信息
    private String mLastRemainInfo = "";
    public ObservableField<Boolean> mDistanceTimeVisible;
    public ObservableField<Boolean> mGroupNextVisible;

    public SceneNaviEtaImpl(final SceneNaviEtaView screenView) {
        super(screenView);
        mDistanceTimeVisible = new ObservableField(false);
        mGroupNextVisible = new ObservableField(true);
    }

    /**
     * @param info 回调 通知更新转向图标信息
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        if (info.getDateType() == NaviConstant.ManeuverDataType.MANEUVER) {
            onShowNaviManeuver(info);
        } else if (info.getDateType() == NaviConstant.ManeuverDataType.MANEUVER_ICON) {
            onObtainManeuverIconData(info);
        }
    }

    /**
     * bl回调 通知更新TBT数据
     * @param naviInfoBean TBT数据
     */
    public void onNaviInfo(final NaviEtaInfo naviInfoBean) {
        mCurNaviInfo = naviInfoBean;
        mRoundNum = mCurNaviInfo.getRingOutCnt();
        // 显示近接动作信息
        if (hasNextThumTip(mCurNaviInfo)) {
            final NaviEtaInfo.NaviCrossNaviInfo nextCrossInfo = mCurNaviInfo.nextCrossInfo.get(0);
            mNextThumRoundNum = nextCrossInfo.outCnt;
            mNextThumManeuverInfo.setManeuverID(nextCrossInfo.crossManeuverID);
            mNextThumManeuverInfo.setSegmentIndex(nextCrossInfo.segIdx);
            mNextThumManeuverInfo.setPathID(mCurNaviInfo.pathID);

            if (isSameNextThumManeuverInfo(mPreviousNextThumManeuverInfo, mNextThumManeuverInfo)) {
                ThreadManager.getInstance().postDelay(new Runnable() {
                    @Override
                    public void run() {
                        updateNaviInfoAndDirection(true);
                    }
                }, 100);
            } else {
                if (null != mPreviousNextThumManeuverInfo) {
                    // 设置离线转向图片
                    if (mOfflineManeuverIcon) {
                        showOfflineNextTurnIcon();
                    }
                    mPreviousNextThumManeuverInfo.setManeuverID(mNextThumManeuverInfo.getManeuverID());
                    mPreviousNextThumManeuverInfo.setPathID(mNextThumManeuverInfo.getPathID());
                    mPreviousNextThumManeuverInfo.setType(mNextThumManeuverInfo.getType());
                    mPreviousNextThumManeuverInfo.setSegmentIndex(mNextThumManeuverInfo.getSegmentIndex());
                }
            }
        }
        innerUpdateNaviInfo();
    }

    /**
     * 导航过程中传出路径上转向图标
     *
     * @param info 转向图标信息
     */
    public void onShowNaviManeuver(final NaviManeuverInfo info) {
        Logger.i(TAG, "onShowNaviManeuver");
        if (info != null) {
            mManeuverInfo = info;
        }
    }

    /**
     * 转向图标回调
     * @param maneuverIconResponseData 转向图标数据
     */
    public void onObtainManeuverIconData(final NaviManeuverInfo maneuverIconResponseData) {
        if (maneuverIconResponseData == null || maneuverIconResponseData.getRequestConfig() == null) {
            Logger.e(TAG, " onObtainManeuverIconData error");
            return;
        }
        final NaviManeuverInfo.NaviManeuverConfig config = maneuverIconResponseData.getRequestConfig();
        // 离线或者在线请求异常，使用本地数据
        if (maneuverIconResponseData.getData() == null || maneuverIconResponseData.getData().length == 0) {
            // 下一路口转向图标
            if (null != config && config.getWidth() == NaviConstant.NEXT_TURN_ICON_SIZE && config.getHeight() == NaviConstant.NEXT_TURN_ICON_SIZE) {
                showOfflineNextTurnIcon();
            }
        } else {
            //当前路口的主动作图标
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    if (null != config && config.getWidth() == NaviConstant.NEXT_TURN_ICON_SIZE && config.getHeight() == NaviConstant.NEXT_TURN_ICON_SIZE) {
                        updateNextThumTurnIcon(maneuverIconResponseData.getData(), config, mNextThumRoundNum);
                    }
                }
            });
        }
        Logger.i(TAG, " onObtainManeuverIconData: requestConfig.width:" + maneuverIconResponseData.getRequestConfig().getWidth());
    }

    /**
     * 显示下一路线本地转向图标
     */
    private void showOfflineNextTurnIcon() {
        setVisibleNaviNext(true);
        final int resId = NaviUiUtil.getOfflineManeuverIconId(mCurNaviInfo.nextCrossInfo.get(0).
                maneuverID, mCurNaviInfo.nextCrossInfo.get(0).outCnt);
        Logger.i(TAG, " showOfflineNextTurnIcon resId:" + resId);
        mScreenView.setBackgroundNaviOfflineNextTurnIcon(SceneCommonStruct.TbtIconAction.get(resId));
    }

    /**
     * 更新近接动作转向图标
     * @param bytes bytes
     * @param config config
     * @param aroundNum aroundNum
     */
    public void updateNextThumTurnIcon(final byte[] bytes,
                                       final NaviManeuverInfo.NaviManeuverConfig config,
                                       final int aroundNum) {
        Logger.i(TAG, " updateNextThumTurnIcon maneuverID:" + config.getManeuverID() + " aroundNum:" + aroundNum + " pathID:" + config.getPathID());
        final int size = NaviConstant.NEXT_TURN_ICON_SIZE;
        mNextThumDirectionCache = NaviUiUtil.getRoadSignBitmap(bytes, size, size, (int) config.getManeuverID(),
                aroundNum, NaviConstant.HUD_RES_PREFIX, NaviConstant.ICON_RES_NAME, NaviConstant.NIGHT);
        if (mNextThumDirectionCache != null && !mNextThumDirectionCache.isRecycled()) {
            Logger.i(TAG, " updateNextThumTurnIcon show nextThumInfo");
            setVisibleNaviNext(true);
            mScreenView.setBackgroundNaviNextTurnIcon(new AutoUIDrawable(mNextThumDirectionCache));
        }
    }

    /**
     * 是否存在近阶动作信息
     * @param naviInfo naviInfo
     * @return boolean
     */
    public static boolean hasNextThumTip(final NaviEtaInfo naviInfo) {
        if (null == naviInfo || ConvertUtils.isEmpty(naviInfo.nextCrossInfo)) {
            return false;
        }
        if (naviInfo.nextCrossInfo.get(0) == null) {
            return false;
        }
        return true;
    }

    /**
     * 是否有进阶路口信息
     * @param previousManeuverInfo previousManeuverInfo
     * @param maneuverInfo maneuverInfo
     * @return boolean
     */
    public static boolean isSameNextThumManeuverInfo(final NaviManeuverInfo previousManeuverInfo,
                                                     final NaviManeuverInfo maneuverInfo) {
        if (null == previousManeuverInfo || null == maneuverInfo) {
            return false;
        }
        return previousManeuverInfo.getManeuverID() == maneuverInfo.getManeuverID()
                && previousManeuverInfo.getType() == maneuverInfo.getType()
                && previousManeuverInfo.getPathID() == maneuverInfo.getPathID()
                && previousManeuverInfo.getSegmentIndex() == maneuverInfo.getSegmentIndex();
    }

    /**
     * 更新引导信息及转向图标
     * @param isNeedUpdateDirection isNeedUpdateDirection
     */
    public void updateNaviInfoAndDirection(final boolean isNeedUpdateDirection) {
        Logger.i(TAG, " updateNaviInfoAndDirection isNeedUpdateDirection: " + isNeedUpdateDirection);
        innerUpdateNaviInfo();
        // 三维实景的转向图标是黑夜模式，所以进入三维场景时需要重新请求图标，故不需要在此处更新转向图标
        if (isNeedUpdateDirection) {
            // 设置离线转向图片
            if (mOfflineManeuverIcon) {
                // TODO
            } else {
                mScreenView.setBackgroundNaviNextTurnIcon(new AutoUIDrawable(mNextThumDirectionCache));
            }
        }
    }

    /**
     * 更新TBT信息
     */
    private void innerUpdateNaviInfo() {
        if (mCurNaviInfo == null || !checkNaviInfoPanelLegal(mCurNaviInfo)) {
            return;
        }
        //更新ETA信息
        updateEta(mCurNaviInfo.getAllDist(), mCurNaviInfo.getAllTime());
        updataNextThumInfo(mCurNaviInfo);
    }

    /**
     * 根据新需求，不需要显示距离，只需显示随后及转向图片
     * 更新二级诱导图
     * @param naviInfo naviInfo
     */
    public void updataNextThumInfo(final NaviEtaInfo naviInfo) {
        final boolean isNextThumTip = isNeedNextThumTip(naviInfo);
        Logger.i(TAG, " updataNextThumInfo isNextThumTip:" + isNextThumTip);
        //清空进阶动作信息
        setVisibleNaviNext(isNextThumTip);
        String txtNextThumText = mScreenView.getContext().getString(R.string.navi_normal_tip);
        if (naviInfo.nextCrossInfo != null && !naviInfo.nextCrossInfo.isEmpty()) {
            final NaviEtaInfo.NaviCrossNaviInfo crossNaviInfo = naviInfo.nextCrossInfo.get(0);
            final int crossDist = crossNaviInfo.curToSegmentDist;
            final String[] distance = ConvertUtils.formatDistanceArray(mScreenView.getContext(), crossDist);
            final AutoUIString autoUIString = new AutoUIString(distance[0] + distance[1]);
            //是否为隧道内外分叉
            switch (crossNaviInfo.tunnelFlag) {
                // TODO: 2024/8/22 UE UI上没有明确要求要区分隧道内外
/*                case CROSS_NAV_TUNNEL_INNER:
                    txtNextThumText = ResUtil.getString(R.string.navi_tunnel_inner_tip);
                    break;
                case CROSS_NAV_TUNNEL_OUT:
                    txtNextThumText = ResUtil.getString(R.string.navi_tunnel_out_tip);
                    break;*/
                default:
                    txtNextThumText += autoUIString.getString(mScreenView.getContext());
                    break;
            }
        }
        mScreenView.setTextNaviNextTurn(new AutoUIString(txtNextThumText));
    }

    /**
     * 根据道路等级检查是否需要近阶动作提醒
     * @param naviInfo naviInfo
     * @return boolean
     */
    public static boolean isNeedNextThumTip(final NaviEtaInfo naviInfo) {
        if (null == naviInfo.nextCrossInfo || naviInfo.nextCrossInfo.isEmpty()) {
            return false;
        }
        if (naviInfo.nextCrossInfo.get(0) == null) {
            return false;
        }
        boolean isNextThumTip = false;
        //检查道路等级限制
        switch (naviInfo.curRoadClass) {
            case NaviConstant.ROAD_CLASS_HIGH_SPEED:
                //高速路
            case NaviConstant.ROAD_CLASS_URBAN_RAPID:
                //城市快速路
                isNextThumTip = naviInfo.NaviInfoData.get(naviInfo.NaviInfoFlag).segmentRemain.dist <= 1000;
                break;
            default:
                isNextThumTip = naviInfo.NaviInfoData.get(naviInfo.NaviInfoFlag).segmentRemain.dist <= 500;
                //其他
                break;
        }
        return isNextThumTip;
    }

    /**
     * 更新ETA信息
     *
     * @param distance
     * @param time
     */
    private void updateEta(final int distance, final int time) {
        if (distance <= 0 && time <= 0) {
            return;
        }
        mArriveDay = TimeUtils.getArriveDay(time);
        mArriveTime = TimeUtils.getArriveTime(mScreenView.getContext(), time);
        mRemainInfo = TimeUtils.getRemainInfo(mScreenView.getContext(), distance, time);
        // 到达或者剩余信息有变化才更新界面
        if (!Objects.equals(mLastArriveTime, mArriveTime) || !Objects.equals(mLastRemainInfo, mRemainInfo)) {
            showArriveInfo();
        }
    }

    /**
     * 预计到达目的剩余信息
     */
    private void showArriveInfo() {
        Logger.i(TAG, " shwoArriveInfo ");
        // 到达时间
        if (!TextUtils.isEmpty(mArriveTime)) {
            mScreenView.setTextNaviEtaRouteArrivalDefault(new AutoUIString(ConvertUtils.digitToBold(mArriveTime)));
        }
        mScreenView.setTextNaviEtaArrivalDay(new AutoUIString(mArriveDay));
        mScreenView.setTextNaviEtaRouteRemainDefault(new AutoUIString(ConvertUtils.digitToBold(mRemainInfo)));
        mLastArriveTime = mArriveTime;
        mLastRemainInfo = mRemainInfo;
    }

    /**
     * 检查NaviInfoPanel是否合法
     * @param naviinfo naviinfo
     * @return boolean
     */
    public static boolean checkNaviInfoPanelLegal(final NaviEtaInfo naviinfo) {
        if (naviinfo == null) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviInfo null");
            return false;
        }
        if (naviinfo.NaviInfoData == null || naviinfo.NaviInfoData.isEmpty()) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoData null");
            return false;
        }
        if (naviinfo.NaviInfoFlag > naviinfo.NaviInfoData.size() - 1) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoFlag length out bound!");
            return false;
        }
        if (naviinfo.NaviInfoData.get(naviinfo.NaviInfoFlag) == null) {
            Logger.d(TAG, "checkNaviInfoPanelLegal naviinfo.NaviInfoData[naviinfo.NaviInfoFlag] null");
            return false;
        }
        return true;
    }

    /**
     * @param isVisible isVisible
     */
    //设置近接动作可见
    private void setVisibleNaviNext(final boolean isVisible) {
        Logger.d(TAG, "setVisibleNaviNext：isVisible：" + isVisible);
        mGroupNextVisible.set(isVisible);
        mDistanceTimeVisible.set(!isVisible);
    }

    /**
     * @param isVisible isVisible
     */
    private void notifxySceneStateChange(final boolean isVisible) {
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_ETA);
    }
}
