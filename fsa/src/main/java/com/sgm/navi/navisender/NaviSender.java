package com.sgm.navi.navisender;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.l2.CleaCanData;
import com.sgm.navi.service.adapter.l2.RoadGroupData;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.signal.RoadConditionGroup;
import com.sgm.navi.service.define.signal.SdNavigationStatusGroup;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.l2.L2Package;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public class NaviSender {
    private static final String TAG = NaviSender.class.getSimpleName();
    private static final String PREFIX = "cleacan";

    private final SignalPackage mSignalPackage;
    private final RoadConditionGroup mRoadConditionGroup = new RoadConditionGroup();
    private final SdNavigationStatusGroup mSdNavigationStatusGroup = new SdNavigationStatusGroup();

    private int sendIndex = 0;
    private ScheduledFuture mScheduledFuture;
    private int mRoadSpeed = 0;
    private int mCameraSpeed = 0;
    private boolean isInit = false;
    private long mPathID = 0;

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
            Logger.i(TAG, PREFIX, "init: not CLEA calibration");
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
        if (isInit) {
            Logger.i(TAG, PREFIX, "inited");
            return;
        }
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);

        String naviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        mSdNavigationStatusGroup.setNaviStat(getNaviState(naviStatus));
        mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
        isInit = true;
        Logger.i(TAG, PREFIX, "init success");
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
                Logger.i(TAG, PREFIX, "巡航电子眼 null");
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
                Logger.i(TAG, PREFIX, "巡航电子眼 speed == 0");
                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
                return;
            }
            mSignalPackage.setVcuSpeedLimitArbitrationResults(speed);
            mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(1);
            Logger.i(TAG, PREFIX, "巡航电子眼", speed);
        }
    };

    private final IPositionPackageCallback mIPositionPackageCallback = new IPositionPackageCallback() {
        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            if (locationInfo == null) {
                Logger.w(TAG, PREFIX, "定位回调: locationInfo == null");
                return;
            }
            int onGuideRoad = locationInfo.getOnGuideRoad();
            mSdNavigationStatusGroup.setNaviStatCrntRdMpConf(onGuideRoad); // 导航绑路状态
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
            mSdNavigationStatusGroup.setNaviStatCrntRdLvl(curRoadClass); // 当前道路类型
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
            if (Logger.isDebugLevel())
                Logger.d(TAG, PREFIX, "定位回调", locationInfo.getOnGuideRoad(), locationInfo.getRoadClass());
        }
    };

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                Logger.w(TAG, PREFIX, "引导面板回调: naviETAInfo == null");
                return;
            }
            mSdNavigationStatusGroup.setNaviStatRmnDist(naviETAInfo.getRemainDist() / 5); // 到导航路线终点的距离
            mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(1);

            int viaRemain = 0;
            ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemainList = naviETAInfo.getViaRemain();
            if (viaRemainList != null && !viaRemainList.isEmpty()) {
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = viaRemainList.get(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(naviTimeAndDist.dist / 5); // 到导航最近途径点距离
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(1);
                viaRemain = naviTimeAndDist.dist;
            } else {
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(0); // 到导航最近途径点距离
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);
            }
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);

            mRoadConditionGroup.setRemainDistance(naviETAInfo.getRemainDist() * 50 / 1000);
            mRoadConditionGroup.setRemainTime(naviETAInfo.getRemainTime());
            checkPathUpdate(naviETAInfo.getPathID());
            if (mScheduledFuture == null) {
                mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
            }

            int chargeStationRemainDist = 0;
            int chargeStationRemainTime = 0;
            ArrayList<NaviEtaInfo.NaviTimeAndDist> chargeStationRemain = naviETAInfo.getChargeStationRemain();
            if (chargeStationRemain == null || chargeStationRemain.isEmpty() || chargeStationRemain.get(0) == null) {
                mSignalPackage.setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                mSignalPackage.setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长
            } else {
                NaviEtaInfo.NaviTimeAndDist naviTimeAndDist = chargeStationRemain.get(0);
                mSignalPackage.setRemainDistanceToChargingStation(naviTimeAndDist.dist / 1000); // 距离充电站剩余里程
                mSignalPackage.setRemainTimeToChargingStationy(naviTimeAndDist.time); // 距离充电站的剩余时长
                chargeStationRemainDist = naviTimeAndDist.dist;
                chargeStationRemainTime = naviTimeAndDist.time;
            }
            if (Logger.isDebugLevel())
                Logger.d(TAG, PREFIX, "引导面板回调", naviETAInfo.getRemainDist(), naviETAInfo.getRemainTime(), viaRemain, chargeStationRemainDist, chargeStationRemainTime);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            Logger.d(TAG, PREFIX, "道路限速", speed);
            if (speed < 0 || speed == 0xFF) { // 道路限速为无效值
                mRoadSpeed = -1;
            } else { // 道路限速为有效值
                mRoadSpeed = speed;
            }
            sendMinSpeedLimit();
        }

        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            int speed = -1;
            if (cameraInfo != null) {
                speed = cameraInfo.getSpeed();
            }
            Logger.d(TAG, PREFIX, "导航电子眼限速", speed);
            if (speed < 0 || speed == 0xFF) { // 电子眼限速为无效值
                mCameraSpeed = -1;
            } else { // 电子眼限速为有效值
                mCameraSpeed = speed;
            }
            sendMinSpeedLimit();
        }

        @Override
        public void onUpdateTMCLightBar(NaviTmcInfo naviTmcInfo) {
            if (naviTmcInfo == null) {
                Logger.d(TAG, PREFIX, "光柱图: null");
                sendTrafficJamRoadInvalid();
                setRoadConditionGroupInvalid();
                return;
            }
            NaviTmcInfo.NaviLightBarDetail lightBarDetail = naviTmcInfo.getLightBarDetail();
            if (lightBarDetail != null) {
                ArrayList<NaviTmcInfo.NaviTmcInfoData> tmcInfoData = lightBarDetail.getTmcInfoData();
                // 此list中的路段信息，已行驶的路段会被标注为status == 10
                if (tmcInfoData != null && !tmcInfoData.isEmpty()) {
                    int distance = 0;
                    for (int i = 0; i < tmcInfoData.size(); i++) {
                        NaviTmcInfo.NaviTmcInfoData naviTmcInfoData = tmcInfoData.get(i);
                        Logger.d(TAG, PREFIX, "拥堵路段原始", naviTmcInfoData);
                        if (naviTmcInfoData == null) {
                            continue;
                        }
                        int status = naviTmcInfoData.getStatus();
                        // -1: 无交通数据, 0: 未知状态 (蓝色), 1: 畅通（绿色）, 2: 缓行（黄色）, 3: 拥堵（红色）, 4: 严重拥堵（深红色）, 5: 极度畅通（深绿）, 10: 行驶过的路段（灰色）
                        if (status == 10) {
                            continue;
                        }
                        if (status == 1 || status == 5 || status == -1 || status == 0) {
                            distance += naviTmcInfoData.getDistance();
                            continue;
                        }
                        if (distance > 20 * 1000) { // 距离超过20km
                            Logger.d(TAG, PREFIX, "拥堵路段发送Invalid: over 20km");
                            sendTrafficJamRoadInvalid();
                            break;
                        }
                        int travelTime = naviTmcInfoData.getTravelTime();
                        mSignalPackage.setDistanceToTrafficJamRoad(distance / 10); // 车辆距离拥堵路段的长度 单位m
                        mSignalPackage.setDistanceToTrafficJamRoadAvailability(0);
                        mSignalPackage.setDistanceOnTrafficJamRoad((float) naviTmcInfoData.getDistance() * 10 / 1000); // 导航中前方拥堵路段的长度 单位km
                        mSignalPackage.setDistanceOnTrafficJamRoadAvailability(0);
                        int speed = travelTime == 0 ? 0 : (int) (naviTmcInfoData.getDistance() / travelTime * 3.6);
                        mSignalPackage.setTrafficJamRoadAverageSpeed(speed); // 导航中形式经过拥堵路段的平均速度 单位km/h
                        mSignalPackage.setTrafficJamRoadAverageSpeedAvailability(0);
                        if (Logger.isDebugLevel())
                            Logger.d(TAG, PREFIX, "拥堵路段发送", distance, naviTmcInfoData.getDistance(), speed);
                        break;
                    }
                } else {
                    Logger.d(TAG, PREFIX, "拥堵路段发送Invalid: null 1");
                    sendTrafficJamRoadInvalid();
                }
            } else {
                Logger.d(TAG, PREFIX, "拥堵路段发送Invalid: null 2");
                sendTrafficJamRoadInvalid();
            }
        }
    };

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            Logger.d(TAG, PREFIX, "导航状态变更", naviStatus);
            mSdNavigationStatusGroup.setNaviStat(getNaviState(naviStatus));
            mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
            if (!NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
                // 仅导航中有效的参数，非导航态需设置默认值
                mSdNavigationStatusGroup.setNaviStatRmnDist(0);
                mSdNavigationStatusGroup.setNaviStatRmnDist_Inv(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint(0);
                mSdNavigationStatusGroup.setNaviStatDistToViaPoint_Inv(0);

                sendTrafficJamRoadInvalid();

                mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
                mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);

                mSignalPackage.setTotalDistanceFromStartToDestinationOnNavigation(0); // 导航总距离
                mSignalPackage.setTotalPredictedTimeFromStartToDestinationOnNavigation(0); // 导航预计时长
                mSignalPackage.setRemainDistanceToChargingStation(0); // 距离充电站剩余里程
                mSignalPackage.setRemainTimeToChargingStationy(0); // 距离充电站的剩余时长

                mPathID = 0;
                if (mScheduledFuture != null) {
                    mScheduledFuture.cancel(true);
                    mScheduledFuture = null;
                }
                mRoadConditionGroup.setRoadSegmentIndex(0);
                mRoadConditionGroup.setSegmentLength(0);
                mRoadConditionGroup.setSegmentTime(0);
                mRoadConditionGroup.setSegmentCondition(0);
                mRoadConditionGroup.setRoadSegmentCount(0);
                mRoadConditionGroup.setRemainDistance(0);
                mRoadConditionGroup.setRemainTime(0);
                mRoadConditionGroup.setDataInvalid(0);
                mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
            }
        }
    };

    private int getNaviState(String naviStatus) {
        /**
         * 1-路径规划中；2-导航中；3-巡航；4-偏航；5-重新规划中；6-非导航（除1和5之外的其他状态）；7-未授权
         */
        switch (naviStatus) {
            case NaviStatus.NaviStatusType.SELECT_ROUTE:
            case NaviStatus.NaviStatusType.ROUTING:
                return 1;
            case NaviStatus.NaviStatusType.NAVING:
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                return 2;
            case NaviStatus.NaviStatusType.CRUISE:
                return 3;
            default:
                return 6;
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

    private void setRoadConditionGroupInvalid() {
        mRoadConditionGroup.setRoadSegmentIndex(0);
        mRoadConditionGroup.setSegmentLength(0);
        mRoadConditionGroup.setSegmentTime(0);
        mRoadConditionGroup.setSegmentCondition(0);
        mRoadConditionGroup.setRoadSegmentCount(0);
    }

    private void sendMinSpeedLimit() {
        int sendSpeed = 0;
        if (mRoadSpeed > 0 && mCameraSpeed > 0) {
            sendSpeed = Math.min(mRoadSpeed, mCameraSpeed);
        } else if (mRoadSpeed >= 0) {
            sendSpeed = mRoadSpeed;
        } else if (mCameraSpeed >= 0) {
            sendSpeed = mCameraSpeed;
        } else {
            // 两个值都无效，发送限速无效标识
            mSignalPackage.setVcuSpeedLimitArbitrationResults(255);
            mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(0);
            return;
        }
        mSignalPackage.setVcuSpeedLimitArbitrationResults(sendSpeed);
        mSignalPackage.setVcuSpeedLimitArbitrationResultsAssured(1);
    }

    private void checkPathUpdate(long pathID) {
        if (pathID == mPathID) {
            return;
        }
        CleaCanData roadGroupData = L2Package.getInstance().getRoadGroupData(pathID);
        if (roadGroupData == null) {
            Logger.e(TAG, PREFIX, "获取当前路线异常", pathID);
            return;
        }
        Logger.d(TAG, PREFIX, "更新路线信息", pathID);
        startRoadConditionGroup(roadGroupData);
        mPathID = pathID;
    }

    private void startRoadConditionGroup(CleaCanData cleaCanData) {
        if (mScheduledFuture != null) {
            Logger.d(TAG, PREFIX, "道路状况发送被打断");
            mScheduledFuture.cancel(true);
        }
        Logger.d(TAG, PREFIX, "道路状况开始发送");
        if (mPathID == 0) {
            sendIndex = -2;
        } else {
            sendIndex = -3;
        }
        mScheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> {
            List<RoadGroupData> roadGroupDatas = cleaCanData.getRoadGroupDatas();
            if (sendIndex >= roadGroupDatas.size()) {
                setRoadConditionGroupInvalid();
                mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
                mScheduledFuture.cancel(true);
                mScheduledFuture = null;
                Logger.d(TAG, PREFIX, "道路状况结束发送");
                return;
            }
            if (sendIndex == -3) { // 1.路线重规划
                mSdNavigationStatusGroup.setNaviStat(5);
                mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
                setRoadConditionGroupInvalid();
                mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
                sendIndex++;
                return;
            }
            if (sendIndex == -2) { // 2.算路完成
                mSdNavigationStatusGroup.setNaviStat(getNaviState(NaviStatusPackage.getInstance().getCurrentNaviStatus()));
                mSignalPackage.setSdNavigationStatus(mSdNavigationStatusGroup);
                sendIndex++;
                return;
            }
            if (sendIndex == -1) { // 3.总距离时间
                mSignalPackage.setTotalDistanceFromStartToDestinationOnNavigation((int) cleaCanData.getTotalLength() * 50 / 1000); // 导航总距离
                mSignalPackage.setTotalPredictedTimeFromStartToDestinationOnNavigation((int) cleaCanData.getTotalTime()); // 导航预计时长
                Logger.d(TAG, PREFIX, "发送路线信息", cleaCanData.getTotalLength(), cleaCanData.getTotalTime());
                sendIndex++;
                return;
            }
            RoadGroupData roadGroupData = roadGroupDatas.get(sendIndex);
            mRoadConditionGroup.setRoadSegmentIndex(sendIndex + 1);
            mRoadConditionGroup.setSegmentLength(roadGroupData.getRoadLength());
            mRoadConditionGroup.setSegmentTime(roadGroupData.getRoadTime());
            // -1: 无交通数据, 0: 未知状态 (蓝色), 1: 畅通（绿色）, 2: 缓行（黄色）, 3: 拥堵（红色）, 4: 严重拥堵（深红色）, 5: 极度畅通（深绿）, 10: 行驶过的路段（灰色）
            int status = roadGroupData.getStatus();
            if (status == 5) {
                status = 1;
            } else if (status == 0 || status == -1 || status == 10) {
                status = 1;
            } else {
                status++;
            }
            mRoadConditionGroup.setSegmentCondition(status);
            mRoadConditionGroup.setRoadSegmentCount(roadGroupDatas.size());
            mRoadConditionGroup.setDataInvalid(1);
            mSignalPackage.setRoadConditionGroup(mRoadConditionGroup);
            sendIndex++;
        }, 0, 250, TimeUnit.MILLISECONDS);
    }
}
