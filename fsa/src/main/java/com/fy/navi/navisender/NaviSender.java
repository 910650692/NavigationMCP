package com.fy.navi.navisender;

import com.android.utils.log.Logger;
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

public class NaviSender {
    private static final String TAG = NaviSender.class.getSimpleName();

    private final RoadConditionGroupSecond mRoadConditionGroupSecond = new RoadConditionGroupSecond();
    private final SdNavigationStatusGroup mSdNavigationStatusGroup = new SdNavigationStatusGroup();

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
    }

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                Logger.w(TAG, "引导面板回调: naviETAInfo == null");
                return;
            }
            ArrayList<String> logs = new ArrayList<>();
            logs.add("roadClass= " + naviETAInfo.getCurRoadClass());
            int curRoadClass = switch (naviETAInfo.getCurRoadClass()) {
                case -1 -> 15;
                case 0 -> 0;
                case 1 -> 2;
                case 2 -> 3;
                case 3 -> 4;
                case 4 -> 5;
                case 6 -> 1;
                default -> 6;
            };
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl(curRoadClass);
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl_Inv(1);
            mSdNavigationStatusGroup.setNaviStatRmnDist(naviETAInfo.getRemainDist() / 5);
            logs.add("remainDist= " + naviETAInfo.getRemainDist());
            mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(1);
            ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = naviETAInfo.getViaRemain();
            if (viaRemain != null && !viaRemain.isEmpty()) {
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = viaRemain.get(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(naviTimeAndDist.dist / 5);
                logs.add("viaDist= " + naviTimeAndDist.dist);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(1);
            } else {
                logs.add("via= null");
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);
            }
            SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup);

            mRoadConditionGroupSecond.setDataInv(1);
            mRoadConditionGroupSecond.setEstimRemnDistn(naviETAInfo.getRemainDist() / 1000);
            mRoadConditionGroupSecond.setEstimRemnTim(naviETAInfo.getRemainTime());
            logs.add("remainTime= " + naviETAInfo.getRemainTime());
            SignalPackage.getInstance().setRoadConditionGroupSecond(mRoadConditionGroupSecond);

            ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain = naviETAInfo.getChargeStationRemain();
            if (chargeStationRemain == null || chargeStationRemain.isEmpty() || chargeStationRemain.get(0) == null) {
                SignalPackage.getInstance().setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                SignalPackage.getInstance().setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长
                logs.add("chargeStation= null");
            } else {
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = chargeStationRemain.get(0);
                SignalPackage.getInstance().setRemainDistanceToChargingStation(naviTimeAndDist.dist / 1000); // 距离充电站剩余里程
                logs.add("chargeStationDist= " + naviTimeAndDist.dist);
                SignalPackage.getInstance().setRemainTimeToChargingStationy(naviTimeAndDist.time); // 距离充电站的剩余时长
                logs.add("chargeStationTime= " + naviTimeAndDist.time);
            }
            Logger.d(TAG, "引导面板回调: " + logs);
        }

        @Override
        public void onUpdateTMCCongestionInfo(NaviCongestionInfoEntity naviCongestionInfoEntity) {
            ArrayList<String> logs = new ArrayList<>();
            if (naviCongestionInfoEntity == null ||
                    naviCongestionInfoEntity.getCongestionInfos() == null ||
                    naviCongestionInfoEntity.getCongestionInfos().isEmpty() ||
                    naviCongestionInfoEntity.getCongestionInfos().get(0) == null) {
                SignalPackage.getInstance().setDistanceToTrafficJamRoad(0);
                SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoad(0);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(0);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(1);
                logs.add("null");
            } else {
                NaviCongestionDetailInfoEntity entity = naviCongestionInfoEntity.getCongestionInfos().get(0);
                int distanceToTrafficJamRoad = L2Package.getInstance().getLinkDist(entity.getBeginSegmentIndex(), entity.getBeginLinkIndex());
                logs.add("distance= " + distanceToTrafficJamRoad);
                int distanceOnTrafficJamRoad = entity.getRemainDist();
                logs.add("length= " + distanceOnTrafficJamRoad);
                int trafficJamRoadAverageSpeed = 0;
                if  (entity.getRemainDist() != 0) {
                    trafficJamRoadAverageSpeed = (int) (entity.getRemainDist() / entity.getTimeOfSeconds() * 3.6);
                }
                logs.add("time= " + entity.getTimeOfSeconds());
                SignalPackage.getInstance().setDistanceToTrafficJamRoad(distanceToTrafficJamRoad / 10); // 距离拥堵路段的行驶距离 单位m
                SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoad(distanceOnTrafficJamRoad / 10); // 拥堵路段的长度 单位km
                SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(trafficJamRoadAverageSpeed); // 拥堵路段的平均车速 单位km/h
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(1);
            }
            Logger.d(TAG, "拥堵信息回调: " + logs);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            Logger.d(TAG, "道路限速回调: " + speed);
            if (speed <= 0 || speed == 0xFF) {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
            } else {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(speed);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(1);
            }
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            Logger.d(TAG, "onRouteResult: ");
            mRouteLineInfos = requestRouteResult.getMRouteLineInfos();
        }
    };

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            ArrayList<String> logs = new ArrayList<>();
            logs.add("naviStatus= " + naviStatus);
            /**
             * 1-路径规划中；2-导航中；3-巡航；4-偏航；5-重新规划中；6-非导航（除1和5之外的其他状态）；7-未授权
             */
            switch (naviStatus) {
                case NaviStatus.NaviStatusType.ROUTING:
                    mSdNavigationStatusGroup.setNaviStat(1);
                    break;
                case NaviStatus.NaviStatusType.NAVING:
                case NaviStatus.NaviStatusType.LIGHT_NAVING:
                    mSdNavigationStatusGroup.setNaviStat(2);
                    break;
                case NaviStatus.NaviStatusType.CRUISE:
                    mSdNavigationStatusGroup.setNaviStat(3);
                    break;
                default:
                    mSdNavigationStatusGroup.setNaviStat(6);
            }
            SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup);

            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(naviStatus)) {
                if (mRouteLineInfos == null) {
                    mRoadConditionGroupSecond.setLngthDynInfmAryOfNavRut(0); // 导航路段信息数组长度
                    SignalPackage.getInstance().setRoadConditionGroupSecond(mRoadConditionGroupSecond);
                } else {
                    mRoadConditionGroupSecond.setLngthDynInfmAryOfNavRut(mRouteLineInfos.size()); // 导航路段信息数组长度
                    logs.add("routeListSize= " + mRouteLineInfos.size());
                    SignalPackage.getInstance().setRoadConditionGroupSecond(mRoadConditionGroupSecond);
                }
                Map<MapType, Integer> selectRouteIndex = RoutePackage.getInstance().getSelectRouteIndex();
                if (mRouteLineInfos == null || mRouteLineInfos.isEmpty() ||
                        selectRouteIndex == null || selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP) == null) {
                    SignalPackage.getInstance().setRoadConditionGroupFirst(new RoadConditionGroupFirst());
                    SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                    SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                } else {
                    Integer index = selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP);
                    RouteLineInfo routeLineInfo = mRouteLineInfos.get(index);
                    long distance = routeLineInfo.getMDistance();

                    RoadConditionGroupFirst roadConditionGroupFirst = new RoadConditionGroupFirst();
                    roadConditionGroupFirst.setIndxOfDynmInftAryNavRut(index); // 导航路段信息索引号
                    logs.add("roadIndex= " + index);
                    roadConditionGroupFirst.setEstimDistnCorpToIndxRut((int) distance); // 对应索引号的预估路段长度
                    roadConditionGroupFirst.setEstimTimCorpToIndxRut((int) routeLineInfo.getMTotalTime()); // 通过路段的预估时长
//                    roadConditionGroupFirst.setEstimRodCndtnCorpToIndxRut(); // 对应索引号的道路状况
                    SignalPackage.getInstance().setRoadConditionGroupFirst(roadConditionGroupFirst);

                    SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation((int) distance / 1000); // 导航总距离
                    logs.add("roadLength= " + distance);
                    SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation((int) routeLineInfo.getMTotalTime()); // 导航预计时长
                    logs.add("roadTime= " + routeLineInfo.getMTotalTime());
                }
                Logger.d(TAG, "算路信息: " + logs);
            } else {
                mSdNavigationStatusGroup.setNaviStatCrntRdLvl(15);
                mSdNavigationStatusGroup.setNaviStatCrntRdLvl_Inv(0);
                mSdNavigationStatusGroup.setNaviStatRmnDist(0);
                mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);

                SignalPackage.getInstance().setDistanceToTrafficJamRoad(0);
                SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoad(0);
                SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(1);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(0);
                SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(1);

                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);

                SignalPackage.getInstance().setRoadConditionGroupFirst(new RoadConditionGroupFirst());
                SignalPackage.getInstance().setRoadConditionGroupSecond(new RoadConditionGroupSecond());

                SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                SignalPackage.getInstance().setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                SignalPackage.getInstance().setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长
                Logger.d(TAG, "导航状态: " + logs);
            }
        }
    };

}
