package com.fy.navi.scene.impl.navi;


import android.graphics.Bitmap;
import android.text.TextUtils;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.scene.BaseSceneModel;
import com.fy.navi.scene.R;
import com.fy.navi.scene.impl.navi.common.AutoUIDrawable;
import com.fy.navi.scene.impl.navi.common.AutoUIString;
import com.fy.navi.scene.impl.navi.common.SceneCommonStruct;
import com.fy.navi.scene.ui.navi.SceneNaviTbtView;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.AdminCodeBean;
import com.fy.navi.service.define.bean.AreaExtraInfoBean;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.setting.SimpleFavoriteItemBean;
import com.fy.navi.scene.impl.navi.common.NaviUiUtil;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;

import java.util.ArrayList;
import java.util.List;

public class SceneNaviTbtImpl extends BaseSceneModel<SceneNaviTbtView> {
    private static final String TAG = MapDefaultFinalTag.NAVI_HMI_TAG;
    private final NaviPackage mNaviPackage;
    private final MapDataPackage mMapDataPackage;
    private final SettingPackage mSettingPackage;
    private final MapPackage mMapPackage;
    private LayerAdapter mLayerAdapter;
    // 转向图标信息
    private NaviManeuverInfo mManeuverInfo;
    // 出口信息
    private NaviManeuverInfo mExitDirectionInfo;
    // 是否离线请求本地导航转向图片信息
    private boolean mOfflineManeuverIcon = true;
    // 当前引导数据
    private NaviEtaInfo mCurNaviInfo;
    // 导航信息转向图标bitmap 缓存
    protected Bitmap directionCache;
    // 环岛数
    private int mRoundNum = 0;
    //近阶动作数
    private int mNextThumRoundNum = 0;
    // 进阶动作
    private NaviManeuverInfo mNextThumManeuverInfo = new NaviManeuverInfo();
    // 上一次进阶动作
    private NaviManeuverInfo mPreviousNextThumManeuverInfo = new NaviManeuverInfo();
    // 下一道路距离
    private int nDistanceNextRoad;
    private int mCurRoadclass;
    //下一条道路名称
    private String mTvNextRoadNameStr;
    //格式化距离数组
    private String[] mDistanceStrArray;
    public ObservableField<Boolean> exitInfoVisible;
    public ObservableField<Boolean> turnInfoVisible;
    public ObservableField<Boolean> groupDivVisible;

    public SceneNaviTbtImpl(SceneNaviTbtView mScreenView) {
        super(mScreenView);
        mMapDataPackage = MapDataPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mLayerAdapter = LayerAdapter.getInstance();
        groupDivVisible = new ObservableField(true);
        turnInfoVisible = new ObservableField(true);
        exitInfoVisible = new ObservableField(false);
    }

    public void onManeuverInfo(NaviManeuverInfo info) {
        if (info.getDateType() == NaviConstant.ManeuverDataType.Maneuver) {
            onShowNaviManeuver(info);
        } else if (info.getDateType() == NaviConstant.ManeuverDataType.ManeuverIcon) {
            onObtainManeuverIconData(info);
        } else {
            onUpdateExitDirectionInfo(info);
        }
    }

    /**
     * bl回调 通知更新TBT数据
     */
    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        mCurNaviInfo = naviInfoBean;
        mRoundNum = mCurNaviInfo.ringOutCnt;
        // 显示近接动作信息
        if (hasNextThumTip(mCurNaviInfo)) {
            NaviEtaInfo.NaviCrossNaviInfo nextCrossInfo = mCurNaviInfo.nextCrossInfo.get(0);
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
                    mPreviousNextThumManeuverInfo.setManeuverID(mNextThumManeuverInfo.getManeuverID());
                    mPreviousNextThumManeuverInfo.setPathID(mNextThumManeuverInfo.getPathID());
                    mPreviousNextThumManeuverInfo.setType(mNextThumManeuverInfo.getType());
                    mPreviousNextThumManeuverInfo.setSegmentIndex(mNextThumManeuverInfo.getSegmentIndex());
                }
            }
        }
        // 经过出入口路牌到下一个导航段时和路线变化时，需要清除保留的出入口信息
        if (mCurNaviInfo != null && mManeuverInfo != null && (mCurNaviInfo.curSegIdx != mManeuverInfo.getSegmentIndex()
                || mCurNaviInfo.pathID != mManeuverInfo.getPathID())) {
            Logger.i(TAG, "SceneNaviTbtImpl 进入下一段路出口信息置为null，不显示出入口信息");
            onUpdateExitDirectionInfo(null);
        } else {
            Logger.i(TAG, "SceneNaviTbtImpl 更新出入口信息");
            updateExitDirectionInfo(mExitDirectionInfo);
        }
        innerUpdateNaviInfo();
        // 设置离线转向图片
        if (mOfflineManeuverIcon) {
            showOfflineTurnIcon();
        }
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
     */
    public void onObtainManeuverIconData(final NaviManeuverInfo maneuverIconResponseData) {
        if (maneuverIconResponseData == null || maneuverIconResponseData.getRequestConfig() == null) {
            Logger.e(TAG, "SceneNaviTbtImpl onObtainManeuverIconData error");
            return;
        }
        NaviManeuverInfo.NaviManeuverConfig config = maneuverIconResponseData.getRequestConfig();
        // 离线或者在线请求异常，使用本地数据
        if (maneuverIconResponseData.getData() == null || maneuverIconResponseData.getData().length == 0) {
            // 下一路口转向图标
            if (null != config && config.getWidth() == NaviConstant.nextTurnIconSize && config.getHeight() == NaviConstant.nextTurnIconSize) {
            } else {
                showOfflineTurnIcon();
            }
        } else {
            //当前路口的主动作图标
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    if (null != config && config.getWidth() == NaviConstant.nextTurnIconSize && config.getHeight() == NaviConstant.nextTurnIconSize) {
                    } else {
                        updateTurnIcon(maneuverIconResponseData.getData(), config, mRoundNum);
                    }
                }
            });
        }
        Logger.i(TAG, "SceneNaviTbtImpl onObtainManeuverIconData: requestConfig.width:" + maneuverIconResponseData.getRequestConfig().getWidth());
    }

    /**
     * 传出出口编号和出口方向信息
     *
     * @details 导航过程中传出出口编号和出口方向信息
     * @param[in] boardInfo        当前导航信息数组
     * @remark 自车在高速和城市快速路并满足一定距离的情况下回调
     * @note thread mutil
     */
    public void onUpdateExitDirectionInfo(NaviManeuverInfo exitDirectionInfo) {
        Logger.i(TAG, "onUpdateExitDirectionInfo");
        if (null == exitDirectionInfo
                || exitDirectionInfo.getDirectionInfo() == null
                || exitDirectionInfo.getDirectionInfo().isEmpty()) {
            mExitDirectionInfo = null;
        } else {
            mExitDirectionInfo = exitDirectionInfo;
        }
        updateExitDirectionInfo(exitDirectionInfo);
    }


    public void onNaviArrive(long traceId, int naviType) {
        mLayerAdapter.clearRouteLine(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mNaviPackage.stopNavigation();
//        StackManager.getInstance().getCurrentFragment(mMapTypeId.name()).closeFragment(true);
    }

    /**
     * 更新出口信息
     */
    public void updateExitDirectionInfo(NaviManeuverInfo guideBoardInfo) {
        Logger.i(TAG, "updateExitDirectionInfo");
        if (mCurNaviInfo == null
                || mCurNaviInfo.NaviInfoData == null
                || mCurNaviInfo.NaviInfoData.isEmpty()
                || mCurNaviInfo.NaviInfoFlag >= mCurNaviInfo.NaviInfoData.size()
                || mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).segmentRemain == null) {
            Logger.i(TAG, "SceneNaviTbtImpl updateExitDirectionInfo null");
            return;
        }
        // 距离出口大于2000米不提示
        int dist = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).segmentRemain.dist;
        if (guideBoardInfo != null
                && guideBoardInfo.getDirectionInfo() != null
                && !guideBoardInfo.getDirectionInfo().isEmpty()
                && !TextUtils.isEmpty(guideBoardInfo.getEntranceExit())
                && dist < NaviConstant.MIN_EXIT_DIST) {
            exitInfoVisible.set(true);
            turnInfoVisible.set(false);
            String directString = guideBoardInfo.getEntranceExit();
            String tvExitNum;
            // 编号个数
            int eixtNum = 0;
            if (guideBoardInfo.getExitNameInfo() != null && !guideBoardInfo.getExitNameInfo().isEmpty()) {
                eixtNum = guideBoardInfo.getExitNameInfo().size();
                List<String> exitInfos = guideBoardInfo.getExitNameInfo();
                if (eixtNum == 1 && exitInfos.size() > 0 && exitInfos.get(0).length() <= NaviConstant.EXIT_STRING_MAX_NUMBER) {
                    // 有出入口编号时，展示”出口“+出口信息或”入口“加入口信息
                    Logger.i(TAG, "SceneNaviTbtImpl updateExitDirectionInfo updateExitInfo: ：出入口编号={?}", exitInfos.get(0));
                    tvExitNum = directString + exitInfos.get(0);
                } else {
                    // 无出入口编号时，仅展示”出口“或”入口“字样
                    // 出入口编号超过一个时，或出入口编号大于5个字符时，不展示出入口编号
                    tvExitNum = directString;
                }
            } else {
                // 无出入口编号时，仅展示”出口“或”入口“字样
                tvExitNum = directString;
            }
            // 方向
            List<String> directionInfos = guideBoardInfo.getDirectionInfo();
            if (directionInfos != null && directionInfos.size() > 0) {
                StringBuilder contentBuilder = new StringBuilder();
                for (int i = 0; i < directionInfos.size(); i++) {
                    if (!TextUtils.isEmpty(directionInfos.get(i).trim())) {
                        //此处空格不能删掉，显示效果需要;
                        contentBuilder.append(directionInfos.get(i)).append(" ");
                    }
                }
            }
            mScreenView.setTextNaviExit(new AutoUIString(tvExitNum));
            Logger.i(TAG, "SceneNaviTbtImpl updateExitDirectionInfo directString={?},eixtNum={?},tvExitNum={?}", directString, eixtNum, tvExitNum);
        } else {
            exitInfoVisible.set(false);
            turnInfoVisible.set(true);
            Logger.i(TAG, "SceneNaviTbtImpl updateExitDirectionInfo 隐藏出入口信息");
        }
    }


    /**
     * 显示当前路线本地转向图标
     */
    private void showOfflineTurnIcon() {
        if (mCurNaviInfo != null && mCurNaviInfo.NaviInfoData != null) {
            // 当前路口转向图标
            int resId = NaviUiUtil.GetOfflineManeuverIconId(mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).maneuverID, mCurNaviInfo.ringOutCnt);
            Logger.i(TAG, "SceneNaviTbtImpl showOfflineTurnIcon resId:" + resId);
            mScreenView.setBackgroundNaviOfflineCommonTurnIcon(SceneCommonStruct.TbtIconAction.get(resId));
            mScreenView.setBackgroundNaviOfflineExitTurnIcon(SceneCommonStruct.TbtExitIconAction.get(resId));
        } else {
            Logger.i(TAG, "showOfflineTurnIcon mCurNaviInfo/NaviInfoData null:");
        }
    }

    /**
     * 更新路口转向图标
     */
    public void updateTurnIcon(final byte[] bytes, final NaviManeuverInfo.NaviManeuverConfig config, final int aroundNum) {
        if (config == null) {
            return;
        }
        Logger.i(TAG, "SceneNaviTbtImpl updateTurnIcon config.width={?},config.height={?},config.maneuverID={?},aroundNum={?},config.pathID={?}",
                config.getWidth(), config.getHeight(), config.getManeuverID(), aroundNum, config.getPathID());
        directionCache = NaviUiUtil.getRoadSignBitmap(bytes, config.getWidth(), config.getHeight(), (int) config.getManeuverID(), aroundNum, NaviConstant.hudResPrefix, NaviConstant.iconResName, NaviConstant.night);
        if (directionCache != null && !directionCache.isRecycled()) {
            Logger.i(TAG, "SceneNaviTbtImpl updateTurnIcon directionCache isRecycled false");
            mScreenView.setBackgroundNaviCommonTurnIcon(new AutoUIDrawable(directionCache));
            mScreenView.setBackgroundNaviExitTurnIcon(new AutoUIDrawable(directionCache));
        }
    }

    /**
     * 是否存在近阶动作信息
     */
    public static boolean hasNextThumTip(NaviEtaInfo naviInfo) {
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
     */
    public static boolean isSameNextThumManeuverInfo(NaviManeuverInfo previousManeuverInfo, NaviManeuverInfo maneuverInfo) {
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
     */
    public void updateNaviInfoAndDirection(boolean isNeedUpdateDirection) {
        Logger.i(TAG, "SceneNaviTbtImpl updateNaviInfoAndDirection isNeedUpdateDirection: " + isNeedUpdateDirection);
        innerUpdateNaviInfo();
        updateExitDirectionInfo(mExitDirectionInfo);
        // 三维实景的转向图标是黑夜模式，所以进入三维场景时需要重新请求图标，故不需要在此处更新转向图标
        if (isNeedUpdateDirection) {
            // 设置离线转向图片
            if (mOfflineManeuverIcon) {
                showOfflineTurnIcon();
            } else {
                mScreenView.setBackgroundNaviCommonTurnIcon(new AutoUIDrawable(directionCache));
                mScreenView.setBackgroundNaviExitTurnIcon(new AutoUIDrawable(directionCache));
            }
        }
    }

    /**
     * 更新TBT信息
     */
    private void innerUpdateNaviInfo() {
        if (mCurNaviInfo == null) {
            return;
        }
        // 显示当前道路
        AdminCodeBean adminCode = new AdminCodeBean();
        adminCode.nAdCode = mCurNaviInfo.cityCode;
        // 二、三级道路名称
        AreaExtraInfoBean areaExtraInfo = mMapDataPackage.getAreaExtraInfo(adminCode);
        ArrayList<SimpleFavoriteItemBean> home = mSettingPackage.getSimpleFavoriteList(
                1, false);
        String extraRoadName = "";
        int adCode = -1;
        // 异地城市加上行政区域名称（和家所在城市对比）
        if (home != null && !home.isEmpty()) {
            GeoPoint homeCoord = mMapPackage.mapToLonLat(MapTypeId.MAIN_SCREEN_MAIN_MAP, home.get(0).point_x, home.get(0).point_y);
            adCode = mMapDataPackage.getAdCodeByLonLat(homeCoord.lon, homeCoord.lat);
        }
        if (areaExtraInfo != null && adCode > 0 && areaExtraInfo.stAdCode != null && areaExtraInfo.stAdCode.nCityAdCode != adCode) {
            Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo cityName= " + areaExtraInfo.cityName + ",townName=" + areaExtraInfo.townName);
            if (!TextUtils.isEmpty(areaExtraInfo.cityName)) {
                String tempCityName = areaExtraInfo.cityName.endsWith(mScreenView.getContext().getString(R.string.navi_city_name_postfix))
                        ? areaExtraInfo.cityName.substring(0, areaExtraInfo.cityName.length() - 1) : areaExtraInfo.cityName;
                extraRoadName += tempCityName;
            }
            if (!TextUtils.isEmpty(areaExtraInfo.townName)) {
                String tempTownName = areaExtraInfo.townName.endsWith(mScreenView.getContext().getString(R.string.navi_town_name_postfix))
                        ? areaExtraInfo.townName.substring(0, areaExtraInfo.townName.length() - 1) : areaExtraInfo.townName;
                if (!TextUtils.isEmpty(extraRoadName)) {
                    extraRoadName = extraRoadName + mScreenView.getContext().getString(R.string.auto_navi_text_residue_diving) + tempTownName;
                } else {
                    extraRoadName = tempTownName;
                }
            }
            // 添加一个空格作为间隔
            extraRoadName = " " + extraRoadName;
        }
        Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo adCode0={?},NaviTbtCurrentRoadName={?} ", adCode, mCurNaviInfo.curRouteName);
        if (mCurNaviInfo == null || !checkNaviInfoPanelLegal(mCurNaviInfo)) {
            return;
        }
        // 到下一道路距离
        nDistanceNextRoad = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).segmentRemain.dist;
        mCurRoadclass = mCurNaviInfo.curRoadClass;
        if (nDistanceNextRoad >= 0) {
            updateNextRoadDistance();
        }
        // 下一条道路名称
        mTvNextRoadNameStr = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).nextRouteName;
        Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo nDistanceNextRoad={?},mCurRoadclass={?},mTVNextRoadNameStr={?}", nDistanceNextRoad,
                mCurRoadclass, mTvNextRoadNameStr);
        updateNextRoadNameView(mTvNextRoadNameStr);
    }

    /**
     * 更新下一道路名称view
     */
    public void updateNextRoadNameView(String tvNextRoadNameStr) {
        if (!TextUtils.isEmpty(tvNextRoadNameStr)) {
            mScreenView.setTextNaviInfoDistanceNextRoadName(new AutoUIString(tvNextRoadNameStr));
        }
    }

    /**
     * 检查NaviInfoPanel是否合法
     */
    public static boolean checkNaviInfoPanelLegal(NaviEtaInfo naviinfo) {
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
     * 更新到下个道路信息
     */
    public void updateNextRoadDistance() {
        mDistanceStrArray = ConvertUtils.formatDistanceArray(mScreenView.getContext(), nDistanceNextRoad);
        // 距路口小于10，显示现在
        if (nDistanceNextRoad <= 10) {
            Logger.d(TAG, "sceneNowNextRoad：");
            groupDivVisible.set(false);
        } else {
            // 显示距离路口实际距离和单位
            Logger.d(TAG, "sceneDistanceNextRoad：");
            groupDivVisible.set(true);
            mScreenView.setTextNaviInfoDistanceNextRoad(new AutoUIString(mDistanceStrArray[0] + ""));
            mScreenView.setTextNaviInfoDistanceNextRoadUnit(new AutoUIString(mDistanceStrArray[1] + ""));
        }
    }
    private void updateSceneVisible(boolean isVisible){
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ? INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneHideState), NaviSceneId.NAVI_SCENE_TBT);
    }
}
