package com.fy.navi.navisender;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.signal.RoadConditionGroupFirst;
import com.fy.navi.service.define.signal.RoadConditionGroupSecond;
import com.fy.navi.service.define.signal.SdNavigationStatusGroup;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class NaviSender {
    private static final String TAG = NaviSender.class.getSimpleName();
    private static final String PREFIX = "cleacan";

    private final RoadConditionGroupSecond mRoadConditionGroupSecond = new RoadConditionGroupSecond();
    private final SdNavigationStatusGroup mSdNavigationStatusGroup = new SdNavigationStatusGroup();

    private List<RouteLineInfo> mRouteLineInfos = new ArrayList<>();
    private ArrayList<RoadGroupData> mRoadGroupDatas = new ArrayList<>();
    private int sendIndex = 0;
    private ScheduledFuture mScheduledFuture;
    private int mRoadSpeed = 0;
    private int mCameraSpeed = 0;

    public static NaviSender getInstance() {
        return NaviSender.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final NaviSender INSTANCE = new NaviSender();
    }

    private NaviSender() {
    }

    public void init() {
        if (CalibrationPackage.getInstance().architecture() != 0) {
            Logger.i(TAG, PREFIX , "init: not CLEA calibration");
            return;
        }
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);
        Logger.i(TAG, PREFIX , "init success");
    }

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null || cruiseInfoEntity.getSpeed() == null) {
                Logger.i(TAG, PREFIX , "巡航电子眼: null");
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            short speed = 0;
            // 获取第一个有效值
            for (Short i : cruiseInfoEntity.getSpeed()) {
                if (i != 0 && i != 0xFF) {
                    speed = i;
                    break;
                }
            }
            if (speed == 0) {
                Logger.i(TAG, PREFIX , "speed == 0");
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(speed);
            SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(1);
            Logger.i(TAG, PREFIX , "巡航电子眼: " , speed);
        }
    };

    private final IPositionPackageCallback mIPositionPackageCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            if (locationInfo == null) {
                Logger.w(TAG, PREFIX , "定位回调: locationInfo == null");
                return;
            }
            int onGuideRoad = locationInfo.getOnGuideRoad();
            mSdNavigationStatusGroup.setNaviStatCrntRdMpConf(onGuideRoad);
            SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup);
        }
    };

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                Logger.w(TAG, PREFIX + "引导面板回调: naviETAInfo == null");
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
            Logger.d(TAG, PREFIX , "引导面板回调: " , logs);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            Logger.d(TAG, PREFIX , "道路限速: " , speed);
            mRoadSpeed = speed;
            int sendSpeed = Math.max(mRoadSpeed, mCameraSpeed);
            if (sendSpeed <= 0 || sendSpeed == 0xFF) {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
            } else {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(sendSpeed);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(1);
            }
        }

        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            mCameraSpeed = cameraInfo.getSpeed();
            Logger.d(TAG, PREFIX , "电子眼限速: " , cameraInfo.getSpeed());
            int sendSpeed = Math.max(mRoadSpeed, mCameraSpeed);
            if (sendSpeed <= 0 || sendSpeed == 0xFF) {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(255);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(0);
            } else {
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResults(sendSpeed);
                SignalPackage.getInstance().setVcuSpeedLimitArbitrationResultsAssured(1);
            }
        }

        @Override
        public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
            if (naviTmcInfo == null) {
                Logger.d(TAG, PREFIX , "光柱图: null");
                return;
            }
            NaviTmcInfo.NaviLightBarDetail lightBarDetail = naviTmcInfo.getLightBarDetail();
            if (lightBarDetail != null) {
                ArrayList<NaviTmcInfo.NaviTmcInfoData> tmcInfoData = lightBarDetail.getTmcInfoData();
                if (tmcInfoData != null) {
                    int distance = 0;
                    for (int i = 0; i < tmcInfoData.size(); i++) {
                        NaviTmcInfo.NaviTmcInfoData naviTmcInfoData = tmcInfoData.get(i);
                        if (naviTmcInfoData == null) {
                            continue;
                        }
                        Logger.d(TAG, PREFIX , "拥堵路段原始: " , naviTmcInfoData);
                        int status = naviTmcInfoData.getStatus();
                        if (status == 10) {
                            continue;
                        }
                        if (status == 1 || status == 5) {
                            distance += naviTmcInfoData.getDistance();
                            continue;
                        }
                        if (distance > 20 * 1000) {
                            sendTrafficJamRoadInvalid();
                            break;
                        }
                        SignalPackage.getInstance().setDistanceToTrafficJamRoad(distance / 10); // 距离拥堵路段的行驶距离 单位m
                        SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(1);
                        SignalPackage.getInstance().setDistanceOnTrafficJamRoad(naviTmcInfoData.getDistance() / 10); // 拥堵路段的长度 单位km
                        SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(1);
                        int travelTime = naviTmcInfoData.getTravelTime();
                        if (travelTime > 0) {
                            int speed = (int) (naviTmcInfoData.getDistance() / travelTime * 3.6);
                            SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(speed); // 拥堵路段的平均车速 单位km/h
                            Logger.d(TAG, PREFIX + "拥堵路段发送: ", distance, naviTmcInfoData.getDistance(), speed);
                        }
                        SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(1);
                        break;
                    }
                } else {
                    sendTrafficJamRoadInvalid();
                }
            } else {
                sendTrafficJamRoadInvalid();
            }
            if (!mRoadGroupDatas.isEmpty()) {
                Logger.d(TAG, PREFIX , "光柱图 发送中");
                return;
            }
            ArrayList<NaviTmcInfo.NaviLightBarInfo> lightBarInfo = naviTmcInfo.getLightBarInfo();
            if (lightBarInfo == null || lightBarInfo.isEmpty()) {
                Logger.d(TAG, PREFIX , "光柱图: null 1");
                return;
            }
            NaviTmcInfo.NaviLightBarInfo naviLightBarInfo = lightBarInfo.get(0);
            if (naviLightBarInfo == null || naviLightBarInfo.getItemList() == null) {
                Logger.d(TAG, PREFIX , "光柱图: null 2");
                return;
            }
            ArrayList<NaviTmcInfo.NaviLightBarItem> itemList = naviLightBarInfo.getItemList();
            if (itemList.isEmpty()) {
                Logger.d(TAG, PREFIX , "光柱图: null 3");
                return;
            }
            for (int i = 0; i < itemList.size(); i++) {
                NaviTmcInfo.NaviLightBarItem naviLightBarItem = itemList.get(i);
                if (naviLightBarItem == null) {
                    continue;
                }
                int status = naviLightBarItem.getStatus();
                // -1: 无交通数据, 0: 未知状态 (蓝色), 1: 畅通（绿色）, 2: 缓行（黄色）, 3: 拥堵（红色）, 4: 严重拥堵（深红色）, 5: 极度畅通（深绿）, 10: 行驶过的路段（灰色）
                if (status == 5) {
                    status = 0;
                } else if (status == 0 || status == -1 || status == 10) {
                    status = 15;
                }
                int distance = naviLightBarItem.getLength();
                if (distance > 65535) {
                    int lengthNum = distance / 65535;
                    int lengthMod = distance % 65535;
                    for (int j = 0; j < lengthNum; j++) {
                        RoadGroupData roadGroupData = new RoadGroupData();
                        roadGroupData.setStatus(status);
                        roadGroupData.setRoadTime((int) (naviLightBarItem.getTimeOfSeconds() * ((float) 65535 / distance)));
                        roadGroupData.setRoadLength(65535);
                        mRoadGroupDatas.add(roadGroupData);
                    }
                    if (lengthMod != 0) {
                        RoadGroupData roadGroupData = new RoadGroupData();
                        roadGroupData.setStatus(status);
                        roadGroupData.setRoadTime((int) (naviLightBarItem.getTimeOfSeconds() * ((float) lengthMod / distance)));
                        roadGroupData.setRoadLength(lengthMod);
                        mRoadGroupDatas.add(roadGroupData);
                    }
                } else {
                    RoadGroupData roadGroupData = new RoadGroupData();
                    roadGroupData.setStatus(status);
                    roadGroupData.setRoadTime((int) naviLightBarItem.getTimeOfSeconds());
                    roadGroupData.setRoadLength(naviLightBarItem.getLength());
                    mRoadGroupDatas.add(roadGroupData);
                }
            }
            Logger.d(TAG, PREFIX , "光柱图解析: ", mRoadGroupDatas);
            mScheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(new Runnable() {
                @Override
                public void run() {
                    if (sendIndex >= mRoadGroupDatas.size()) {
                        sendIndex = 0;
                        mRoadGroupDatas.clear();
                        mScheduledFuture.cancel(true);
                        return;
                    }
                    ArrayList<String> logs = new ArrayList<>();
                    logs.add("size= " + mRoadGroupDatas.size());
                    RoadGroupData roadGroupData = mRoadGroupDatas.get(sendIndex);
                    RoadConditionGroupFirst roadConditionGroupFirst = new RoadConditionGroupFirst();
                    roadConditionGroupFirst.setIndxOfDynmInftAryNavRut(sendIndex + 1); // 导航路段信息索引号
                    logs.add("index= " + (sendIndex + 1));
                    roadConditionGroupFirst.setEstimDistnCorpToIndxRut(roadGroupData.getRoadLength()); // 对应索引号的预估路段长度
                    logs.add("length= " + roadGroupData.getRoadLength());
                    roadConditionGroupFirst.setEstimTimCorpToIndxRut(roadGroupData.getRoadTime()); // 通过路段的预估时长
                    logs.add("time= " + roadGroupData.getRoadTime());
                    roadConditionGroupFirst.setEstimRodCndtnCorpToIndxRut(roadGroupData.getStatus()); // 对应索引号的道路状况
                    logs.add("status= " + roadGroupData.getStatus());
                    logs.add("remnDist= " + mRoadConditionGroupSecond.getEstimRemnDistn());
                    logs.add("remnTime= " + mRoadConditionGroupSecond.getEstimRemnTim());
                    mRoadConditionGroupSecond.setLngthDynInfmAryOfNavRut(mRoadGroupDatas.size());
                    mRoadConditionGroupSecond.setDataInv(1);
                    SignalPackage.getInstance().setRoadConditionGroupSecond(mRoadConditionGroupSecond);
                    Logger.d(TAG, PREFIX , "拥堵信息", logs);
                    sendIndex++;
                }
            }, 0, 250, TimeUnit.MILLISECONDS);
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            Logger.d(TAG, PREFIX , "onRouteResult: ");
            mRouteLineInfos = requestRouteResult.getMRouteLineInfos();
            String naviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(naviStatus)) {
                mSdNavigationStatusGroup.setNaviStat(2);
            }
        }

        @Override
        public void onReroute() {
            Logger.d(TAG, PREFIX , "偏航: ");
            mSdNavigationStatusGroup.setNaviStat(4);
            SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup);
            mSdNavigationStatusGroup.setNaviStat(5);
            SignalPackage.getInstance().setSdNavigationStatus(mSdNavigationStatusGroup);

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
                Map<MapType, Integer> selectRouteIndex = RoutePackage.getInstance().getSelectRouteIndex();
                if (mRouteLineInfos == null || mRouteLineInfos.isEmpty() ||
                        selectRouteIndex == null || selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP) == null) {
                    SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                    SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                } else {
                    Integer index = selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP);
                    RouteLineInfo routeLineInfo = mRouteLineInfos.get(index);
                    long distance = routeLineInfo.getMDistance();
                    SignalPackage.getInstance().setTotalDistanceFromStartToDestinationOnNavigation((int) distance / 1000); // 导航总距离
                    logs.add("roadLength= " + distance);
                    SignalPackage.getInstance().setTotalPredictedTimeFromStartToDestinationOnNavigation((int) routeLineInfo.getMTotalTime()); // 导航预计时长
                    logs.add("roadTime= " + routeLineInfo.getMTotalTime());
                }
                Logger.d(TAG, PREFIX , "算路信息: " , logs);
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
                Logger.d(TAG, PREFIX , "导航状态: " , logs);
            }
        }
    };

    private void sendTrafficJamRoadInvalid() {
        SignalPackage.getInstance().setDistanceToTrafficJamRoad(0);
        SignalPackage.getInstance().setDistanceToTrafficJamRoadAvailability(1);
        SignalPackage.getInstance().setDistanceOnTrafficJamRoad(0);
        SignalPackage.getInstance().setDistanceOnTrafficJamRoadAvailability(1);
        SignalPackage.getInstance().setTrafficJamRoadAverageSpeed(0);
        SignalPackage.getInstance().setTrafficJamRoadAverageSpeedAvailability(1);
    }
}
