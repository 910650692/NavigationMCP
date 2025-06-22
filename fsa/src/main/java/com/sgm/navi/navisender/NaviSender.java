package com.sgm.navi.navisender;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteLineInfo;
import com.sgm.navi.service.define.signal.RoadConditionGroup;
import com.sgm.navi.service.define.signal.SdNavigationStatusGroup;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class NaviSender {
    private static final String TAG = NaviSender.class.getSimpleName();
    private static final String PREFIX = "cleacan";

    private final SignalPackage mSignalPackage;
    private final RoadConditionGroup mRoadConditionGroup = new RoadConditionGroup();
    private final SdNavigationStatusGroup mSdNavigationStatusGroup = new SdNavigationStatusGroup();

    private List<RouteLineInfo> mRouteLineInfos = new ArrayList<>();
    private ArrayList<RoadGroupData> mRoadGroupDatas = new ArrayList<>();
    private int sendIndex = 0;
    private ScheduledFuture mScheduledFuture;
    private int mRoadSpeed = 0;
    private int mCameraSpeed = 0;

    //region INSTANCE
    public static NaviSender getInstance() {
        return NaviSender.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final NaviSender INSTANCE = new NaviSender();
    }

    private NaviSender() {
        mSignalPackage = SignalPackage.getInstance();
    }
    //endregion

    //region 初始化
    public void start() {
        if (CalibrationPackage.getInstance().architecture() != 0) {
            Logger.i(TAG, PREFIX , "init: not CLEA calibration");
            return;
        }
        Logger.d(TAG, "start");
        StartService.getInstance().registerSdkCallback(TAG, mISdkInitCallback);
        final int engineInit = StartService.getInstance().getSdkActivation();
        if (engineInit == -1) {
            Logger.i(TAG, "engine not initialized");
            mSdNavigationStatusGroup.setNaviStat(7);
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
            return;
        }
        if (engineInit == 1) {
            Logger.i(TAG, "engine initialization");
            return;
        }
        // 地图引擎已初始化
        init();
    }

    private void init() {
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);

        String naviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        setNaviState(naviStatus);
        mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
        Logger.i(TAG, PREFIX , "init success");
    }

    private final StartService.ISdkInitCallback mISdkInitCallback = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            init();
        }

        @Override
        public void onSdkInitFail(int initSdkResult, String msg) {
            mSdNavigationStatusGroup.setNaviStat(7);
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
        }
    };
    //endregion

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null || cruiseInfoEntity.getSpeed() == null) {
                Logger.i(TAG, PREFIX , "巡航电子眼: null");
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
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
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            mSignalPackage.setVcuSpeedLimitArbitrationResults(speed);
            mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(1);
            Logger.i(TAG, PREFIX , "巡航电子眼: " + speed);
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
            int curRoadClass = switch (locationInfo.getRoadClass()) {
                case -1 -> 15;
                case 0 -> 0;
                case 1 -> 2;
                case 2 -> 3;
                case 3 -> 4;
                case 4 -> 5;
                case 6 -> 1;
                default -> 6;
            };
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl_Inv(1);
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl(curRoadClass);
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
        }
    };

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                Logger.w(TAG, PREFIX , "引导面板回调: naviETAInfo == null");
                return;
            }
            ArrayList<String> logs = new ArrayList<>();
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
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);

            mRoadConditionGroup.setRemainDistance(naviETAInfo.getRemainDist() / 1000);
            mRoadConditionGroup.setRemainTime(naviETAInfo.getRemainTime());
            logs.add("remainTime= " + naviETAInfo.getRemainTime());

            ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain = naviETAInfo.getChargeStationRemain();
            if (chargeStationRemain == null || chargeStationRemain.isEmpty() || chargeStationRemain.get(0) == null) {
                mSignalPackage.setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                mSignalPackage.setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长
                logs.add("chargeStation= null");
            } else {
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = chargeStationRemain.get(0);
                mSignalPackage.setRemainDistanceToChargingStation(naviTimeAndDist.dist / 1000); // 距离充电站剩余里程
                logs.add("chargeStationDist= " + naviTimeAndDist.dist);
                mSignalPackage.setRemainTimeToChargingStationy(naviTimeAndDist.time); // 距离充电站的剩余时长
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
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
            } else {
                mSignalPackage.setVcuSpeedLimitArbitrationResults(sendSpeed);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(1);
            }
        }

        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            mCameraSpeed = cameraInfo.getSpeed();
            Logger.d(TAG, PREFIX , "电子眼限速: " , cameraInfo.getSpeed());
            int sendSpeed = Math.max(mRoadSpeed, mCameraSpeed);
            if (sendSpeed <= 0 || sendSpeed == 0xFF) {
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
            } else {
                mSignalPackage.setVcuSpeedLimitArbitrationResults(sendSpeed);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(1);
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
                        int travelTime = naviTmcInfoData.getTravelTime();
                        mSignalPackage.setDistanceToTrafficJamRoad(distance / 10); // 距离拥堵路段的行驶距离 单位m
                        mSignalPackage.setDistanceToTrafficJamRoadAvailability(1);
                        mSignalPackage.setDistanceOnTrafficJamRoad(naviTmcInfoData.getDistance() / 10); // 拥堵路段的长度 单位km
                        mSignalPackage.setDistanceOnTrafficJamRoadAvailability(1);
                        int speed = travelTime == 0 ? 0 : (int) (naviTmcInfoData.getDistance() / travelTime * 3.6);
                        mSignalPackage.setTrafficJamRoadAverageSpeed(speed); // 拥堵路段的平均车速 单位km/h
                        mSignalPackage.setTrafficJamRoadAverageSpeedAvailability(1);
                        Logger.d(TAG, PREFIX , "拥堵路段发送: ", distance, naviTmcInfoData.getDistance(), speed);
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
                    RoadGroupData roadGroupData = mRoadGroupDatas.get(sendIndex);
                    mRoadConditionGroup.setRoadSegmentIndex(sendIndex + 1);
                    mRoadConditionGroup.setSegmentLength(roadGroupData.getRoadLength());
                    mRoadConditionGroup.setSegmentTime(roadGroupData.getRoadTime());
                    mRoadConditionGroup.setSegmentCondition(roadGroupData.getStatus());
                    mRoadConditionGroup.setRoadSegmentCount(mRoadGroupDatas.size());
                    mRoadConditionGroup.setDataInvalid(1);
                    mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
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
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
            mSdNavigationStatusGroup.setNaviStat(5);
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);

        }
    };

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            ArrayList<String> logs = new ArrayList<>();
            logs.add("naviStatus= " + naviStatus);
            setNaviState(naviStatus);
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);

            if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus) || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(naviStatus)) {
                Map<MapType, Integer> selectRouteIndex = RoutePackage.getInstance().getSelectRouteIndex();
                if (mRouteLineInfos == null || mRouteLineInfos.isEmpty() ||
                        selectRouteIndex == null || selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP) == null) {
                    mSignalPackage.setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                    mSignalPackage.setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                } else {
                    Integer index = selectRouteIndex.get(MapType.MAIN_SCREEN_MAIN_MAP);
                    if (index == null || index > mRouteLineInfos.size() -1) {
                        Logger.e(TAG, PREFIX , "算路信息 select error: " + logs);
                        return;
                    }
                    RouteLineInfo routeLineInfo = mRouteLineInfos.get(index);
                    long distance = routeLineInfo.getMDistance();
                    mSignalPackage.setTotalDistanceFromStartToDestinationOnNavigation((int) distance / 1000); // 导航总距离
                    logs.add("roadLength= " + distance);
                    mSignalPackage.setTotalPredictedTimeFromStartToDestinationOnNavigation((int) routeLineInfo.getMTotalTime()); // 导航预计时长
                    logs.add("roadTime= " + routeLineInfo.getMTotalTime());
                }
                Logger.d(TAG, PREFIX , "导航状态: " , logs);
            } else {
                mSdNavigationStatusGroup.setNaviStatRmnDist(0);
                mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);

                mSignalPackage.setDistanceToTrafficJamRoad(0);
                mSignalPackage.setDistanceToTrafficJamRoadAvailability(1);
                mSignalPackage.setDistanceOnTrafficJamRoad(0);
                mSignalPackage.setDistanceOnTrafficJamRoadAvailability(1);
                mSignalPackage.setTrafficJamRoadAverageSpeed(0);
                mSignalPackage.setTrafficJamRoadAverageSpeedAvailability(1);

                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);

                mSignalPackage.setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                mSignalPackage.setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                mSignalPackage.setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                mSignalPackage.setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长

                mRoadConditionGroup.setRoadSegmentIndex(0);
                mRoadConditionGroup.setSegmentLength(0);
                mRoadConditionGroup.setSegmentTime(0);
                mRoadConditionGroup.setSegmentCondition(15);
                mRoadConditionGroup.setRoadSegmentCount(0);
                mRoadConditionGroup.setRemainDistance(0);
                mRoadConditionGroup.setRemainTime(0);
                mRoadConditionGroup.setDataInvalid(0);
                mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
                Logger.d(TAG, PREFIX , "算路状态: " + logs);
            }
        }
    };

    private void setNaviState(String naviStatus) {
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
    }

    private void sendTrafficJamRoadInvalid() {
        mSignalPackage.setDistanceToTrafficJamRoad(0);
        mSignalPackage.setDistanceToTrafficJamRoadAvailability(1);
        mSignalPackage.setDistanceOnTrafficJamRoad(0);
        mSignalPackage.setDistanceOnTrafficJamRoadAvailability(1);
        mSignalPackage.setTrafficJamRoadAverageSpeed(0);
        mSignalPackage.setTrafficJamRoadAverageSpeedAvailability(1);
    }
}
