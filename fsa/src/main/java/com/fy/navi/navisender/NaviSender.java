package com.fy.navi.navisender;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviCongestionDetailInfoEntity;
import com.fy.navi.service.define.navi.NaviCongestionInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.signal.RoadConditionGroupFirst;
import com.fy.navi.service.define.signal.RoadConditionGroupSecond;
import com.fy.navi.service.define.signal.SdNavigationStatusGroup;
import com.fy.navi.service.logicpaket.l2.L2Package;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.concurrent.TimeUnit;

public class NaviSender {
    private static final String TAG = NaviSender.class.getSimpleName();

    private RoadConditionGroupFirst mRoadConditionGroupFirst = new RoadConditionGroupFirst();
    private RoadConditionGroupSecond mRoadConditionGroupSecond = new RoadConditionGroupSecond();
    private int mRemainDistanceToChargingStation;
    private int mRemainTimeToChargingStationy;
    private SdNavigationStatusGroup mSdNavigationStatusGroup = new SdNavigationStatusGroup();

    private int mDistanceToTrafficJamRoad;
    private boolean mDistanceToTrafficJamRoadAvailability;
    private int mDistanceOnTrafficJamRoad;
    private boolean mDistanceOnTrafficJamRoadAvailability;
    private int mTrafficJamRoadAverageSpeed;
    private boolean mTrafficJamRoadAverageSpeedAvailability;

    private List<RouteLineInfo> mRouteLineInfos = new ArrayList<>();

    public static NaviSender getInstance() {
        return NaviSender.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final NaviSender INSTANCE = new NaviSender();
    }

    private NaviSender() {
    }

    public void init() {
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);

        ThreadManager.getInstance().asyncAtFixDelay(new Runnable() {
            @Override
            public void run() {
                /**
                 * 4.3.2.14
                 * 如上信息应在导航状态下透出
                 */
                if (!Objects.equals(NaviStatus.NaviStatusType.NAVING, NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
                    return;
                }
                //长度单位m 时间单位s
                SignalPackage.getInstance().setRoadConditionGroupFirst(mRoadConditionGroupFirst);
                SignalPackage.getInstance().setRoadConditionGroupSecond(mRoadConditionGroupSecond);

                SignalPackage.getInstance().setRemainDistanceToChargingStation(mRemainDistanceToChargingStation); // 距离充电站剩余里程
                SignalPackage.getInstance().setRemainTimeToChargingStationy(mRemainTimeToChargingStationy); // 距离充电站的剩余时长
                if (mRouteLineInfos != null) {
                    Map<MapType, Integer> selectRouteIndex = RoutePackage.getInstance().getSelectRouteIndex();
                    if (selectRouteIndex != null) {
                        Integer index = selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP);
                        if (index != null && index < mRouteLineInfos.size()) {
                            RouteLineInfo routeLineInfo = mRouteLineInfos.get(index);
                            if (routeLineInfo != null) {
                                SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation((int) routeLineInfo.getMDistance()); // 导航总距离
                                SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation((int) routeLineInfo.getMTotalTime()); // 导航预计时长
                            }
                        }
                    }
                }
            }
        }, 0, 250, TimeUnit.MILLISECONDS);

        ThreadManager.getInstance().asyncAtFixDelay(new Runnable() {
            @Override
            public void run() {
                /**
                 * 4.3.2.1
                 * 导航状态下，LBS系统需要透出前方2km的交通拥堵信息；巡航状态下，LBS系统需要透出前方1km的交通拥堵信息。
                 */
                if (!Objects.equals(NaviStatus.NaviStatusType.NAVING, NaviStatusPackage.getInstance().getCurrentNaviStatus())) {
                    return;
                }
                SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup); // 导航状态
                SignalPackage.getInstance().setDistanceToTrafficJamRoad(mDistanceToTrafficJamRoad); // 距离拥堵路段的行驶距离 单位m
                SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(mDistanceToTrafficJamRoadAvailability ? 1 : 0);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoad(mDistanceOnTrafficJamRoad); // 拥堵路段的长度 单位km
                SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(mDistanceOnTrafficJamRoadAvailability ? 1 : 0);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(mTrafficJamRoadAverageSpeed); // 拥堵路段的平均车速 单位km/h
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(mTrafficJamRoadAverageSpeedAvailability ? 1 : 0);
            }
        }, 0, 1, TimeUnit.SECONDS);
    }

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                mRoadConditionGroupSecond.setDataInv(0);
                mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(0);
                mSdNavigationStatusGroup.setNaviStatCrntRdLvl_Inv(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);
                return;
            }
            mRoadConditionGroupSecond.setDataInv(1);
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl(naviETAInfo.getCurRoadClass());
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl_Inv(1);
            mSdNavigationStatusGroup.setNaviStatRmnDist(naviETAInfo.getAllDist() / 5);
            mRoadConditionGroupSecond.setEstimRemnDistn(naviETAInfo.getAllDist());
            mRoadConditionGroupSecond.setEstimRemnTim(naviETAInfo.getAllTime());
            mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(1);
            try {
                ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = naviETAInfo.getViaRemain();
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = viaRemain.get(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(naviTimeAndDist.dist / 5);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(1);
            } catch (NullPointerException e) {
                Logger.w(TAG, "途径点距离", e);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);
            }
            try {
                ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain = naviETAInfo.getChargeStationRemain();
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = chargeStationRemain.get(0);
                mRemainDistanceToChargingStation = naviTimeAndDist.dist;
                mRemainTimeToChargingStationy = naviTimeAndDist.time;
            } catch (NullPointerException e) {
                Logger.w(TAG, "充电站距离", e);
            }
        }

        @Override
        public void onUpdateTMCCongestionInfo(NaviCongestionInfoEntity naviCongestionInfoEntity) {
            if (naviCongestionInfoEntity == null) {
                mDistanceToTrafficJamRoad = 0;
                mDistanceToTrafficJamRoadAvailability = false;
                mDistanceOnTrafficJamRoad = 0;
                mDistanceOnTrafficJamRoadAvailability = false;
                mTrafficJamRoadAverageSpeed = 0;
                mTrafficJamRoadAverageSpeedAvailability = false;
                return;
            }
            ArrayList<NaviCongestionDetailInfoEntity> congestionInfos = naviCongestionInfoEntity.getCongestionInfos();
            if (congestionInfos == null || congestionInfos.isEmpty()) {
                mDistanceToTrafficJamRoad = 0;
                mDistanceToTrafficJamRoadAvailability = false;
                mDistanceOnTrafficJamRoad = 0;
                mDistanceOnTrafficJamRoadAvailability = false;
                mTrafficJamRoadAverageSpeed = 0;
                mTrafficJamRoadAverageSpeedAvailability = false;
                return;
            }
            NaviCongestionDetailInfoEntity entity = naviCongestionInfoEntity.getCongestionInfos().get(0);
            mDistanceToTrafficJamRoad = L2Package.getInstance().getTrafficLightDis(entity.getBeginSegmentIndex(), entity.getBeginLinkIndex());
            mDistanceToTrafficJamRoadAvailability = true;
            mDistanceOnTrafficJamRoad = entity.getRemainDist();
            mDistanceOnTrafficJamRoadAvailability = true;
            mTrafficJamRoadAverageSpeed = (entity.getRemainDist() / 1000) / (entity.getTimeOfSeconds() / 3600) * 10;
            mTrafficJamRoadAverageSpeedAvailability = true;
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            mRouteLineInfos = requestRouteResult.getMRouteLineInfos();
        }
    };

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
//            mSdNavigationStatusGroup.setNaviStat();
        }
    };

}
