package com.sgm.navi.scene.impl.navi;


import android.graphics.Bitmap;
import android.text.TextUtils;

import androidx.databinding.ObservableField;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.scene.BaseSceneModel;
import com.sgm.navi.scene.R;
import com.sgm.navi.scene.impl.navi.common.AutoUIDrawable;
import com.sgm.navi.scene.impl.navi.common.AutoUIString;
import com.sgm.navi.scene.impl.navi.common.SceneCommonStruct;
import com.sgm.navi.scene.ui.navi.SceneNaviTbtView;
import com.sgm.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.sgm.navi.scene.ui.navi.manager.NaviSceneId;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.AdminCodeBean;
import com.sgm.navi.service.define.bean.AreaExtraInfoBean;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.scene.impl.navi.common.NaviUiUtil;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;

import java.util.List;

public class SceneNaviTbtImpl extends BaseSceneModel<SceneNaviTbtView> implements IPositionPackageCallback {
    private static final String TAG = MapDefaultFinalTag.NAVI_SCENE_TBT_IMPL;
    private final NaviPackage mNaviPackage;
    private final MapDataPackage mMapDataPackage;
    private final BehaviorPackage mBehaviorPackage;
    private final MapPackage mMapPackage;
    private LayerPackage mLayerPackage;
    // 转向图标信息
    private NaviManeuverInfo mManeuverInfo;
    // 出口信息
    private NaviManeuverInfo mExitDirectionInfo;
    // 是否离线请求本地导航转向图片信息
    private boolean mOfflineManeuverIcon = true;
    // 当前引导数据
    private NaviEtaInfo mCurNaviInfo;
    // 导航信息转向图标bitmap 缓存
    private Bitmap mDirectionCache;
    // 环岛数
    private int mRoundNum = 0;
    //近阶动作数
    private int mNextThumRoundNum = 0;
    // 进阶动作
    private NaviManeuverInfo mNextThumManeuverInfo = new NaviManeuverInfo();
    // 上一次进阶动作
    private NaviManeuverInfo mPreviousNextThumManeuverInfo = new NaviManeuverInfo();
    // 下一道路距离
    private int mDistanceNextRoad;
    private int mCurRoadclass;
    //下一条道路名称
    private String mTvNextRoadNameStr;
    //格式化距离数组
    private String[] mDistanceStrArray;
    public ObservableField<Boolean> mExitInfoVisible;
    public ObservableField<Boolean> mTurnInfoVisible;
    public ObservableField<Boolean> mGroupDivVisible;
    private final PositionPackage mPositionPackage;

    public SceneNaviTbtImpl(final SceneNaviTbtView screenView) {
        super(screenView);
        mMapDataPackage = MapDataPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mBehaviorPackage = BehaviorPackage.getInstance();
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mPositionPackage = PositionPackage.getInstance();
        mGroupDivVisible = new ObservableField(true);
        mTurnInfoVisible = new ObservableField(true);
        mExitInfoVisible = new ObservableField(false);
    }

    @Override
    protected void onCreate() {
        super.onCreate();
        if (mPositionPackage != null) {
            mPositionPackage.registerCallBack(this);
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (mPositionPackage != null) {
            mPositionPackage.unregisterCallBack(this);
        }
    }

    /**
     * @param info 回调 通知更新转向图标信息
     */
    public void onManeuverInfo(final NaviManeuverInfo info) {
        if (info.getDateType() == NaviConstant.ManeuverDataType.MANEUVER) {
            onShowNaviManeuver(info);
        } else if (info.getDateType() == NaviConstant.ManeuverDataType.MANEUVER_ICON) {
            onObtainManeuverIconData(info);
        } else {
            onUpdateExitDirectionInfo(info);
        }
    }

    /**
     * bl回调 通知更新TBT数据
     *
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
     *
     * @param maneuverIconResponseData 转向图标数据
     */
    public void onObtainManeuverIconData(final NaviManeuverInfo maneuverIconResponseData) {
        if (maneuverIconResponseData == null || maneuverIconResponseData.getRequestConfig() == null) {
            Logger.e(TAG, "SceneNaviTbtImpl onObtainManeuverIconData error");
            return;
        }
        final NaviManeuverInfo.NaviManeuverConfig config = maneuverIconResponseData.getRequestConfig();
        // 离线或者在线请求异常，使用本地数据
        if (maneuverIconResponseData.getData() == null || maneuverIconResponseData.getData().length == 0) {
            // 下一路口转向图标
            if (null != config && config.getWidth() == NaviConstant.NEXT_TURN_ICON_SIZE &&
                    config.getHeight() == NaviConstant.NEXT_TURN_ICON_SIZE) {
                // TODO
            } else {
                showOfflineTurnIcon();
            }
        } else {
            //当前路口的主动作图标
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    if (null != config && config.getWidth() == NaviConstant.NEXT_TURN_ICON_SIZE &&
                            config.getHeight() == NaviConstant.NEXT_TURN_ICON_SIZE) {
                        // TODO
                    } else {
                        updateTurnIcon(maneuverIconResponseData.getData(), config, mRoundNum);
                    }
                }
            });
        }
        Logger.i(TAG, "SceneNaviTbtImpl onObtainManeuverIconData: requestConfig.width:",
                maneuverIconResponseData.getRequestConfig().getWidth());
    }

    /**
     * 传出出口编号和出口方向信息
     *
     * @param exitDirectionInfo 出口编号和出口方向信息
     */
    public void onUpdateExitDirectionInfo(final NaviManeuverInfo exitDirectionInfo) {
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


    /**
     * @param traceId  traceId
     * @param naviType 导航类型
     */
    public void onNaviArrive(final long traceId, final int naviType) {
    }

    /**
     * 更新出口信息
     *
     * @param guideBoardInfo 导航信息
     */
    public void updateExitDirectionInfo(final NaviManeuverInfo guideBoardInfo) {
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
        final int dist = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).segmentRemain.dist;
        if (guideBoardInfo != null
                && guideBoardInfo.getDirectionInfo() != null
                && !guideBoardInfo.getDirectionInfo().isEmpty()
                && !TextUtils.isEmpty(guideBoardInfo.getEntranceExit())
                && dist < NaviConstant.MIN_EXIT_DIST) {
            mExitInfoVisible.set(true);
            mTurnInfoVisible.set(false);
            final String directString = guideBoardInfo.getEntranceExit();
            final String tvExitNum;
            // 编号个数
            int eixtNum = 0;
            if (guideBoardInfo.getExitNameInfo() != null && !guideBoardInfo.getExitNameInfo().isEmpty()) {
                eixtNum = guideBoardInfo.getExitNameInfo().size();
                final List<String> exitInfos = guideBoardInfo.getExitNameInfo();
                if (eixtNum == 1 && !exitInfos.isEmpty() && exitInfos.get(0).length() <=
                        NaviConstant.EXIT_STRING_MAX_NUMBER) {
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
            final List<String> directionInfos = guideBoardInfo.getDirectionInfo();
            if (directionInfos != null && !directionInfos.isEmpty()) {
                final StringBuilder contentBuilder = new StringBuilder();
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
            mExitInfoVisible.set(false);
            mTurnInfoVisible.set(true);
            Logger.i(TAG, "SceneNaviTbtImpl updateExitDirectionInfo 隐藏出入口信息");
        }
    }


    /**
     * 显示当前路线本地转向图标
     */
    private void showOfflineTurnIcon() {
        if (mCurNaviInfo != null && mCurNaviInfo.NaviInfoData != null) {
            // 当前路口转向图标
            final int resId = NaviUiUtil.getOfflineManeuverIconId(
                    mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).maneuverID,
                    mCurNaviInfo.getRingOutCnt());
            Logger.i(TAG, "SceneNaviTbtImpl showOfflineTurnIcon resId:", resId);
            mScreenView.setBackgroundNaviOfflineCommonTurnIcon(SceneCommonStruct.TbtExitIconAction.get(resId));
            mScreenView.setBackgroundNaviOfflineExitTurnIcon(SceneCommonStruct.TbtExitIconAction.get(resId));
        } else {
            Logger.i(TAG, "showOfflineTurnIcon mCurNaviInfo/NaviInfoData null");
        }
    }

    /**
     * 更新路口转向图标
     *
     * @param bytes     bytes
     * @param config    config
     * @param aroundNum aroundNum
     */
    public void updateTurnIcon(final byte[] bytes, final NaviManeuverInfo.NaviManeuverConfig config, final int aroundNum) {
        if (config == null) {
            return;
        }
        Logger.i(TAG, "SceneNaviTbtImpl updateTurnIcon config.width={?},config.height={?},config.maneuverID={?},aroundNum={?},config.pathID={?}",
                config.getWidth(), config.getHeight(), config.getManeuverID(), aroundNum, config.getPathID());
        mDirectionCache = NaviUiUtil.getRoadSignBitmap(bytes, config.getWidth(), config.getHeight(),
                (int) config.getManeuverID(), aroundNum, NaviConstant.HUD_RES_PREFIX,
                NaviConstant.ICON_RES_NAME, NaviConstant.NIGHT);
        if (mDirectionCache != null && !mDirectionCache.isRecycled()) {
            Logger.i(TAG, "SceneNaviTbtImpl updateTurnIcon directionCache isRecycled false");
            mScreenView.setBackgroundNaviCommonTurnIcon(new AutoUIDrawable(mDirectionCache));
            mScreenView.setBackgroundNaviExitTurnIcon(new AutoUIDrawable(mDirectionCache));
        }
    }

    /**
     * 是否存在近阶动作信息
     *
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
     *
     * @param previousManeuverInfo previousManeuverInfo
     * @param maneuverInfo         maneuverInfo
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
     *
     * @param isNeedUpdateDirection isNeedUpdateDirection
     */
    public void updateNaviInfoAndDirection(final boolean isNeedUpdateDirection) {
        Logger.i(TAG, "SceneNaviTbtImpl updateNaviInfoAndDirection isNeedUpdateDirection: ",
                isNeedUpdateDirection);
        innerUpdateNaviInfo();
        updateExitDirectionInfo(mExitDirectionInfo);
        // 三维实景的转向图标是黑夜模式，所以进入三维场景时需要重新请求图标，故不需要在此处更新转向图标
        if (isNeedUpdateDirection) {
            // 设置离线转向图片
            if (mOfflineManeuverIcon) {
                showOfflineTurnIcon();
            } else {
                mScreenView.setBackgroundNaviCommonTurnIcon(new AutoUIDrawable(mDirectionCache));
                mScreenView.setBackgroundNaviExitTurnIcon(new AutoUIDrawable(mDirectionCache));
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
        final AdminCodeBean adminCode = new AdminCodeBean();
        adminCode.setnAdCode(mCurNaviInfo.cityCode);
        // 二、三级道路名称
        final AreaExtraInfoBean areaExtraInfo = mMapDataPackage.getAreaExtraInfo(adminCode);
        final PoiInfoEntity home = mBehaviorPackage.getHomeFavoriteInfo();
        Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo home.getPoint().getLon()=", home);
        String extraRoadName = "";
        int adCode = -1;
        // 异地城市加上行政区域名称（和家所在城市对比）
        if (home != null && home.getPoint() != null) {
            final GeoPoint homeCoord = home.getPoint();
            adCode = mMapDataPackage.getAdCodeByLonLat(homeCoord.getLon(), homeCoord.getLat());
        }
        if (areaExtraInfo != null && adCode > 0 && areaExtraInfo.getStAdCode() != null && areaExtraInfo.getStAdCode().getnCityAdCode() != adCode) {
            Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo cityName= ",
                    areaExtraInfo.getCityName(), ",townName=", areaExtraInfo.getTownName());
            if (!TextUtils.isEmpty(areaExtraInfo.getCityName())) {
                final String tempCityName = areaExtraInfo.getCityName().endsWith(mScreenView.getContext().getString(R.string.navi_city_name_postfix))
                        ? areaExtraInfo.getCityName().substring(0, areaExtraInfo.getCityName().length() - 1) : areaExtraInfo.getCityName();
                extraRoadName += tempCityName;
            }
            if (!TextUtils.isEmpty(areaExtraInfo.getTownName())) {
                final String tempTownName = areaExtraInfo.getTownName().endsWith(mScreenView.getContext().getString(R.string.navi_town_name_postfix))
                        ? areaExtraInfo.getTownName().substring(0, areaExtraInfo.getTownName().length() - 1) : areaExtraInfo.getTownName();
                if (!TextUtils.isEmpty(extraRoadName)) {
                    extraRoadName = extraRoadName + mScreenView.getContext().getString(R.string.auto_navi_text_residue_diving) + tempTownName;
                } else {
                    extraRoadName = tempTownName;
                }
            }
            // 添加一个空格作为间隔
            extraRoadName = " " + extraRoadName;
        }
        Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo adCode0={?},NaviTbtCurrentRoadName={?} ", adCode, mCurNaviInfo.getCurRouteName());
        if (mCurNaviInfo == null || !checkNaviInfoPanelLegal(mCurNaviInfo)) {
            return;
        }
        // 到下一道路距离
        mDistanceNextRoad = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).segmentRemain.dist;
        mCurRoadclass = mCurNaviInfo.curRoadClass;
        if (mDistanceNextRoad >= 0) {
            updateNextRoadDistance();
        }
        // 下一条道路名称
        mTvNextRoadNameStr = mCurNaviInfo.NaviInfoData.get(mCurNaviInfo.NaviInfoFlag).nextRouteName;
        Logger.i(TAG, "SceneNaviTbtImpl innerUpdateNaviInfo nDistanceNextRoad={?},mCurRoadclass={?},mTVNextRoadNameStr={?}", mDistanceNextRoad,
                mCurRoadclass, mTvNextRoadNameStr);
        updateNextRoadNameView(mTvNextRoadNameStr);
    }

    /**
     * 更新下一道路名称view
     *
     * @param tvNextRoadNameStr 名称
     */
    public void updateNextRoadNameView(final String tvNextRoadNameStr) {
        if (!TextUtils.isEmpty(tvNextRoadNameStr)) {
            mScreenView.setTextNaviInfoDistanceNextRoadName(new AutoUIString(tvNextRoadNameStr));
        }
    }

    /**
     * 检查NaviInfoPanel是否合法
     *
     * @param naviinfo naviInfo
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
     * 更新到下个道路信息
     */
    public void updateNextRoadDistance() {
        mDistanceStrArray = ConvertUtils.formatDistanceArray(mScreenView.getContext(), mDistanceNextRoad);
        // 距路口小于10，显示现在
        if (mDistanceNextRoad <= 10) {
            Logger.d(TAG, "sceneNowNextRoad：", mDistanceNextRoad);
            //mGroupDivVisible.set(false);
            mScreenView.setTextNaviInfoDistanceNextRoad(new AutoUIString(mScreenView.getContext().getString(com.android.utils.R.string.now)));
            mScreenView.setTextNaviInfoDistanceNextRoadUnit(new AutoUIString(""));
        } else {
            // 显示距离路口实际距离和单位
            Logger.d(TAG, "sceneDistanceNextRoad：", mDistanceNextRoad);
            mGroupDivVisible.set(true);
            mScreenView.setTextNaviInfoDistanceNextRoad(new AutoUIString(mDistanceStrArray[0] + ""));
            mScreenView.setTextNaviInfoDistanceNextRoadUnit(new AutoUIString(mDistanceStrArray[1] + ""));
        }
    }

    /**
     * @param isVisible 是否可见
     */
    private void updateSceneVisible(final boolean isVisible) {
        if (mScreenView.isVisible() == isVisible) return;
        Logger.i(TAG, "SceneNaviTbtImpl", isVisible);
        mScreenView.getNaviSceneEvent().notifySceneStateChange((isVisible ?
                INaviSceneEvent.SceneStateChangeType.SceneShowState :
                INaviSceneEvent.SceneStateChangeType.SceneCloseState), NaviSceneId.NAVI_SCENE_TBT);
    }

    @Override
    public void onSatelliteNum(int num) {
        if (num == 0) {
            mScreenView.onUpdateGpsStrength(NaviConstant.GpsStrengthState.GPS_NONE);
        } else if (num > 0 && num <= 4) {
            mScreenView.onUpdateGpsStrength(NaviConstant.GpsStrengthState.GPS_WEAK);
        } else if (num > 4 && num <= 7) {
            mScreenView.onUpdateGpsStrength(NaviConstant.GpsStrengthState.GPS_MEDIUM);
        } else if (num > 7 && num <= 24) {
            mScreenView.onUpdateGpsStrength(NaviConstant.GpsStrengthState.GPS_STRONG);
        }
    }

    @Override
    public void onGpsSatellitesChanged(boolean isLocSuccess) {
        if (!isLocSuccess) {
            mScreenView.onUpdateGpsStrength(NaviConstant.GpsStrengthState.GPS_NONE);
        }
    }
}
