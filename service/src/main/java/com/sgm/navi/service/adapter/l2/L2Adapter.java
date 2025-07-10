package com.sgm.navi.service.adapter.l2;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.model.Formway;
import com.autonavi.gbl.common.path.model.LinkType;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.autonavi.gbl.guide.model.NaviFacilityType;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.adapter.cruise.CruiseAdapter;
import com.sgm.navi.service.adapter.cruise.CruiseObserver;
import com.sgm.navi.service.adapter.navi.GuidanceObserver;
import com.sgm.navi.service.adapter.navi.NaviAdapter;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.adapter.navistatus.INaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.adapter.position.IPositionAdapterCallback;
import com.sgm.navi.service.adapter.position.PositionAdapter;
import com.sgm.navi.service.adapter.route.RouteAdapter;
import com.sgm.navi.service.define.cruise.CruiseFacilityEntity;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.L2NaviBean;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviMixForkInfo;
import com.sgm.navi.service.define.navi.NaviRoadFacilityEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.position.LocParallelInfoEntity;
import com.sgm.navi.service.define.route.RouteCurrentPathParam;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Adapter {
    private static final String TAG = L2Adapter.class.getSimpleName();
    private static final String PREFIX = "l2++";

    private L2DriveObserver l2DriveObserver;
    private volatile L2NaviBean l2NaviBean;
    private String mNaviStatus;
    private ScheduledExecutorService mScheduler;
    private NaviEtaInfo mNaviEtaInfo;
    private int mTaskId;
    private L2NaviBean.EndParkingInfo mEndParkingInfo = new L2NaviBean.EndParkingInfo();

    //region INSTANCE
    public static L2Adapter getInstance() {
        return L2Adapter.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final L2Adapter INSTANCE = new L2Adapter();
    }

    private L2Adapter() {
    }
    //endregion

    public void init() {
        if (l2NaviBean != null) {
            return;
        }
        l2NaviBean = new L2NaviBean();
        l2NaviBean.setCrossInfoData(new L2NaviBean.CrossInfoDataBean()); // 组合
        l2NaviBean.setVehiclePosition(new L2NaviBean.VehiclePositionBean()); // 组合
        l2NaviBean.setIntervalCameraData(new L2NaviBean.IntervalCameraDataBean());
        l2NaviBean.setLimitCameraData(new L2NaviBean.LimitCameraDataBean());
        l2NaviBean.setTunnelInfo(new L2NaviBean.TunnelInfoBean()); // 隧道
        l2NaviBean.setGuidePointInfo(new L2NaviBean.GuidePointInfoBean());
        l2NaviBean.setWarningFacility(new L2NaviBean.WarningFacilityBean()); // 警示牌
        l2NaviBean.setAheadIntersections(new ArrayList<>());
        l2NaviBean.setMixForks(new ArrayList<>());
        registerAdapterCallback();
        mNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        mScheduler = Executors.newScheduledThreadPool(1);
        mScheduler.scheduleWithFixedDelay(mTask, 0, 1, TimeUnit.SECONDS);
    }

    /**
     * 向其他模块注册数据回到接口.
     */
    private void registerAdapterCallback() {
        NaviAdapter.getInstance().registerObserver(getClass().getSimpleName(), mGuidanceObserver);
        NavistatusAdapter.getInstance().registerCallback(mINaviStatusCallback);
        PositionAdapter.getInstance().registerCallback(mIPositionAdapterCallback);
        CruiseAdapter.getInstance().registerObserver(TAG, mCruiseObserver);
    }

    /**
     * 向其他模块反注册数据回到接口.
     */
    private void unRegisterAdapterCallback() {
        NaviAdapter.getInstance().unregisterObserver(getClass().getSimpleName());
        NavistatusAdapter.getInstance().unRegisterCallback(mINaviStatusCallback);
        PositionAdapter.getInstance().unregisterCallback(mIPositionAdapterCallback);
    }

    private final GuidanceObserver mGuidanceObserver = new GuidanceObserver() {
        @Override
        public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
            mNaviEtaInfo = naviEtaInfo;
            if (naviEtaInfo == null) {
                Logger.i(TAG, PREFIX , "引导面板 null");
                return;
            }
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();

            vehiclePosition.setCurPathID(naviEtaInfo.pathID); // 当前选择得路线下标
            vehiclePosition.setDistToDestination(naviEtaInfo.getRemainDist()); // 当前位置到目的地距离
            long locationLinkIndex = getLocationLinkIndex(naviEtaInfo.curSegIdx, naviEtaInfo.curLinkIdx);
            vehiclePosition.setLocationLinkIndex(locationLinkIndex); //自车当前位置的link索引，跟全局路线中的link索引对应
            int locationLinkOffset = getLinkLength(naviEtaInfo.curSegIdx, naviEtaInfo.curLinkIdx) - naviEtaInfo.linkRemainDist;
            vehiclePosition.setLocationLinkOffset(Math.max(locationLinkOffset, 0));

            int curRoadClass = naviEtaInfo.curRoadClass;
            if (curRoadClass == 0xFF) {
                vehiclePosition.setRoadClass(-1); // 当前自车所在道路等级
            } else {
                vehiclePosition.setRoadClass(curRoadClass); // 当前自车所在道路等级
            }

            L2NaviBean.GuidePointInfoBean guidePointInfo = l2NaviBean.getGuidePointInfo();
            int dist = naviEtaInfo.NaviInfoData.get(naviEtaInfo.NaviInfoFlag).segmentRemain.dist;
            guidePointInfo.setNextGuideDist(dist); // 到下一个引导点的剩余距离（导航：2000米以内发出）
            int maneuverId = naviEtaInfo.getNextManeuverID();
            if (maneuverId == -1) {
                maneuverId = 0;
            }
            guidePointInfo.setNextGuideType(maneuverId); // 引导点动作高德原始接口（导航：2000米以内发出）.

            L2NaviBean.TunnelInfoBean tunnelInfo = getTunnelInfo(naviEtaInfo);
            l2NaviBean.setMTunnelInfo(tunnelInfo);
            int rampDist = getRampDist(naviEtaInfo);
            l2NaviBean.setRampDist(rampDist);
            if (BuildConfig.DEBUG) Logger.i(TAG, PREFIX , "引导面板: ", "pathID= " + naviEtaInfo.pathID, "remainDist= " + naviEtaInfo.getRemainDist(), "locationLinkIndex= " + locationLinkIndex, "locationLinkOffset= " + locationLinkOffset, "roadClass= " + curRoadClass, "guideDist= " + dist, "roadClass= " + maneuverId, "tunnelDist= " + tunnelInfo.getTunnelDist(), "tunnelLength= " + tunnelInfo.getTunnelLength(), "rampDist= " + rampDist);
        }

        @Override
        public void onPlayTTS(SoundInfoEntity pInfo) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            String text = pInfo.getText();
            vehiclePosition.setTtsText(text); // 语音播报对应的文本（导航状态，前方一个）
            Logger.i(TAG, PREFIX , "语音播报" , text);
        }

        @Override
        public void onPlayRing(int type) {

        }

        @Override
        public void onLaneInfoReceived(ArrayList<LaneInfoEntity> laneInfoList) {
            if (laneInfoList == null || laneInfoList.isEmpty()) {
                Logger.i(TAG, PREFIX , "后续车道信息 null");
                List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
                aheadIntersections.clear();
                return;
            }
            List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
            aheadIntersections.clear();
            for (int i = 0; i < laneInfoList.size(); i++) {
                LaneInfoEntity laneInfoEntity = laneInfoList.get(i);
                long linkIndexDist = getLinkIndexDist(laneInfoEntity.getSegmentIdx(), laneInfoEntity.getLinkIdx());
                if (linkIndexDist < 0) {
                    continue;
                }
                L2NaviBean.AheadIntersectionsBean aheadIntersectionsBean = new L2NaviBean.AheadIntersectionsBean();
                aheadIntersectionsBean.setLaneNum(laneInfoEntity.getBackLane().size()); // 下个路口车道数
                aheadIntersectionsBean.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
                aheadIntersectionsBean.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
                aheadIntersectionsBean.setTimestamp(System.currentTimeMillis());
                aheadIntersectionsBean.setLaneTypes(laneInfoEntity.getBackLane()); // 下个路口所有车道通行方向
                aheadIntersectionsBean.setHighLightLaneTypes(laneInfoEntity.getFrontLane()); // 表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
                aheadIntersectionsBean.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
                aheadIntersectionsBean.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
                aheadIntersections.add(aheadIntersectionsBean);
                if (aheadIntersections.size() > 5) {
                    break;
                }
            }
            if (BuildConfig.DEBUG) Logger.i(TAG, PREFIX , "后续车道信息", laneInfoList);
        }

        @Override
        public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            if (!isShowLane) {
                Logger.i(TAG, PREFIX , "车道隐藏");
                crossInfoDataBean.setHighLightLanes(new ArrayList<>());
                crossInfoDataBean.setHighLightLaneTypes(new ArrayList<>());
                crossInfoDataBean.setBackLaneType(new ArrayList<>());
                crossInfoDataBean.setFrontLaneType(new ArrayList<>());
                crossInfoDataBean.setSegmentIndex(-1);
                crossInfoDataBean.setLinkIndex(-1);
                crossInfoDataBean.setTimestamp(System.currentTimeMillis());
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>());
                l2NaviBean.setHasTidalLane(0);
                return;
            }
            if (ConvertUtils.isEmpty(laneInfoEntity)) {
                Logger.i(TAG, PREFIX , "车道 null");
                return;
            }
            boolean hasTidalLane = false;
            ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
            if (ConvertUtils.isEmpty(backLanes)) return;
            int LaneNum = laneInfoEntity.getBackLane().size();
            List<Integer> highLightLanes = new ArrayList<>();
            for (int i = 0; i < LaneNum; i++) {
                int recommend = laneInfoEntity.getFrontLane().get(i) != NaviConstant.LaneAction.LANE_ACTION_NULL ? 1 : 0;
                highLightLanes.add(recommend);
                if (NaviConstant.LANE_TYPE_TIDAL == laneInfoEntity.getBackLaneType().get(i)) {
                    hasTidalLane = true;
                }
            }
            if (hasTidalLane) {
                l2NaviBean.setHasTidalLane(1); // 有潮汐车道
            } else {
                l2NaviBean.setHasTidalLane(0); // 没有潮汐车道
            }
            crossInfoDataBean.setLaneNum(laneInfoEntity.getBackLane().size());
            crossInfoDataBean.setLaneTypes(laneInfoEntity.getBackLane());
            crossInfoDataBean.setHighLightLanes(highLightLanes); // 设置推荐车道
            crossInfoDataBean.setHighLightLaneTypes(laneInfoEntity.getFrontLane()); // 设置表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
            crossInfoDataBean.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
            crossInfoDataBean.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
            crossInfoDataBean.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
            crossInfoDataBean.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
            crossInfoDataBean.setTimestamp(System.currentTimeMillis());

            Logger.i(TAG, PREFIX , "车道--------------------------------------");
            Logger.i(TAG, PREFIX , "车道   高亮", highLightLanes, hasTidalLane);
            Logger.i(TAG, PREFIX , "车道   前景", laneInfoEntity.getFrontLane());
            Logger.i(TAG, PREFIX , "车道   背景", laneInfoEntity.getBackLane());
            Logger.i(TAG, PREFIX , "车道分时前景", laneInfoEntity.getFrontLaneType());
            Logger.i(TAG, PREFIX , "车道分时背景", laneInfoEntity.getBackLaneType());
        }

        @Override
        public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdownEntity> list) {
//            if (ConvertUtils.isEmpty(list)) {
//                Logger.i(TAG, "红绿灯 null 1");
//                l2NaviBean.getCrossInfoData().setTrafficLightPosition(0xFFFF);
//                l2NaviBean.getCrossInfoData().setHasTrafficLight(0);
//                return;
//            }
//            TrafficLightCountdownEntity lightCountdown = list.get(0);
//            if (lightCountdown == null) {
//                Logger.i(TAG, "红绿灯 null 2");
//                l2NaviBean.getCrossInfoData().setTrafficLightPosition(0xFFFF);
//                l2NaviBean.getCrossInfoData().setHasTrafficLight(0);
//                return;
//            }
//            int trafficLightDis = getTrafficLightDis(lightCountdown.getMSegmentIndex(), lightCountdown.getMLinkIndex());
//            l2NaviBean.getCrossInfoData().setTrafficLightPosition(trafficLightDis);
//            l2NaviBean.getCrossInfoData().setHasTrafficLight(1);
//            Logger.i(TAG, "红绿灯", trafficLightDis);
        }

        @Override
        public void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list) {
            if (ConvertUtils.isEmpty(list)) {
                Logger.i(TAG, PREFIX + "混淆路口 null");
                l2NaviBean.setMixForks(new ArrayList<>());
                return;
            }
            List<L2NaviBean.MixForksBean> mixForksBeanList = new ArrayList<>(); // 混淆路口信息列表
            for (int i = 0; i < list.size(); i++) {
                NaviMixForkInfo mixForkInfo = list.get(i);
                if (ConvertUtils.isEmpty(mixForkInfo)) continue;
                L2NaviBean.MixForksBean mixForksBean = new L2NaviBean.MixForksBean();
                mixForksBean.setDistance(mixForkInfo.getMDist());
                mixForksBean.setRoadClass(mixForkInfo.getMRoadclass());
                mixForksBean.setSegmentIndex(mixForkInfo.getMSegmentIndex());
                mixForksBean.getPosition().setX(mixForkInfo.getMPos().getLat());
                mixForksBean.getPosition().setY(mixForkInfo.getPos().getLon());
                mixForksBeanList.add(mixForksBean);
            }
            l2NaviBean.setMixForks(mixForksBeanList);
            Logger.i(TAG, PREFIX + "混淆路口", mixForksBeanList);
        }

        /**
         * 最近的限速电子眼
         *
         * @param cameraInfo 电子眼实体
         */
        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            L2NaviBean.LimitCameraDataBean limitCameraData = l2NaviBean.getLimitCameraData();
            if (ConvertUtils.isEmpty(cameraInfo)) {
                Logger.i(TAG, PREFIX + "电子眼 cameraInfo null");
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getDistance() > 2000) { // 超过2km无需返回
                Logger.i(TAG, PREFIX + "电子眼 over 2km");
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getSpeed() == 0) {
                Logger.i(TAG, PREFIX + "电子眼 speed 0");
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            limitCameraData.setSpdLmtEleEyeDist(cameraInfo.getDistance());
            limitCameraData.setSpdLmtEleEyeSpeedValue(cameraInfo.getSpeed());
            Logger.i(TAG, PREFIX + "电子眼", limitCameraData);
        }

        /**
         * 区间测速
         *
         * @param speedEntity 车速实体
         */
        @Override
        public void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
            if (ConvertUtils.isEmpty(speedEntity)) {
                Logger.i(TAG, PREFIX + "区间测速 speedEntity null");
                l2NaviBean.setIntervalCameraData(new L2NaviBean.IntervalCameraDataBean());
                return;
            }
            L2NaviBean.IntervalCameraDataBean intervalCameraDataBean = l2NaviBean.getIntervalCameraData();
            // 区间测速总距离 - 区间测速剩余距离 = 区间测速起点距离
            intervalCameraDataBean.setIntervalCameraStartPointDist(speedEntity.getDistance() - speedEntity.getRemainDistance());
            intervalCameraDataBean.setIntervalCameraEndPointDist(speedEntity.getRemainDistance());
            ArrayList<Short> speedList = speedEntity.getLimitSpeedList();
            short speed = 0;
            // 获取第一个有效值
            for (Short i : speedList) {
                if (i != 0 && i != 0xFF) {
                    speed = i;
                    break;
                }
            }
            intervalCameraDataBean.setIntervalCameraSpeedValue(speed);
            l2NaviBean.setIntervalCameraData(intervalCameraDataBean);
            Logger.i(TAG, PREFIX + "区间测速", intervalCameraDataBean);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            vehiclePosition.setCurrentSpeedLimit(speed);

            L2NaviBean.WarningFacilityBean warningFacility = l2NaviBean.getWarningFacility();
            if (warningFacility.getBoardSignType() == 10) {
                warningFacility.setLimitSpeed(speed);
            } else {
                warningFacility.setLimitSpeed(0);
            }
            Logger.i(TAG, PREFIX + "道路限速", speed);
        }

        @Override
        public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
            Logger.i(TAG, sapaInfoEntity);
            if (ConvertUtils.isEmpty(sapaInfoEntity)) {
                Logger.i(TAG, PREFIX + "收费站距离 null");
                l2NaviBean.setTollStationDist(0xFFFF);
                return;
            }
            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = sapaInfoEntity.getList();
            if (ConvertUtils.isEmpty(sapaItems)) {
                Logger.i(TAG, PREFIX + "收费站距离 null");
                l2NaviBean.setTollStationDist(0xFFFF); // 集合为空，代表没有收费站信息
                return;
            } else {
                int tollStationDist = Integer.MAX_VALUE;
                for (int i = 0; i < sapaItems.size(); i++) {
                    SapaInfoEntity.SAPAItem sapaItem = sapaItems.get(i);
                    if (sapaItem == null) {
                        continue;
                    }
                    if (sapaItem.getType() != NaviFacilityType.NaviFacilityTypeTollGate) {
                        continue;
                    }
                    int distance = sapaItem.getRemainDist();
                    if (distance != 0 && distance < 2000 && distance < tollStationDist) {
                        tollStationDist = distance;
                    }
                }
                if (tollStationDist != Integer.MAX_VALUE) {
                    l2NaviBean.setTollStationDist(tollStationDist); // 收费站距离
                } else {
                    l2NaviBean.setTollStationDist(0xFFFF); // 与前方收费站剩余距离 0xFFFF表示没有收费站
                }
            }
            Logger.i(TAG, PREFIX + "收费站距离", l2NaviBean.getTollStationDist());
        }

        @Override
        public void onNaviStop() {

        }

        @Override
        public void onShowNaviFacility(ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntitys) {
            L2NaviBean.WarningFacilityBean warningFacility = l2NaviBean.getWarningFacility();
            if (naviRoadFacilityEntitys == null || naviRoadFacilityEntitys.isEmpty()) {
                Logger.i(TAG, PREFIX + "道路设施", "null");
                warningFacility.setBoardSignType(0);
                warningFacility.setBoardSignDist(0xFFFF);
                return;
            }
            NaviRoadFacilityEntity naviRoadFacilityEntity = naviRoadFacilityEntitys.get(0);
            warningFacility.setBoardSignType(naviRoadFacilityEntity.getType());
            warningFacility.setBoardSignDist(naviRoadFacilityEntity.getDistance());
            Logger.i(TAG, PREFIX + "道路设施", naviRoadFacilityEntity);
        }
    };

    private final INaviStatusCallback mINaviStatusCallback = new INaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            if (naviStatus == null) {
                Logger.i(TAG, PREFIX + "引导状态 null");
                return;
            }
            if (mNaviStatus != null){
                if (mNaviStatus.equals(naviStatus)) {
                    return;
                }
                mNaviStatus = naviStatus;
                l2NaviBean.clear();
                mNaviEtaInfo = null;
                Logger.i(TAG, PREFIX + "引导状态", naviStatus);
            }
        }
    };

    private final IPositionAdapterCallback mIPositionAdapterCallback = new IPositionAdapterCallback() {
        @Override
        public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
            L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
            if (ConvertUtils.isEmpty(entity)) {
                Logger.i(TAG, PREFIX + "主辅路: null");
                vpb.setMainSideRots(0);
                return;
            }
            vpb.setMainSideRots(entity.getFlag());
            Logger.i(TAG, PREFIX + "主辅路", entity.getFlag());
        }

        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            if (locationInfo == null) {
                Logger.i(TAG, PREFIX + "位置信息: null");
            }
            ArrayList<String> logs = new ArrayList<>();
            L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
            vpb.setLocationLongitude(locationInfo.getLongitude());
            logs.add("Longitude= " + locationInfo.getLongitude());
            vpb.setLocationLatitude(locationInfo.getLatitude());
            logs.add("Longitude= " + locationInfo.getLatitude());
            vpb.setFormWay(locationInfo.getFormway());
            logs.add("Formway= " + locationInfo.getFormway());
            vpb.setLinkType(locationInfo.getLinkType());
            logs.add("LinkType= " + locationInfo.getLinkType());
            int ownership = locationInfo.getOwnership();
            logs.add("Ownership= " + ownership);
            vpb.setRoadOwnership(ownership == -1 ? 0 : ownership);
            Logger.i(TAG, PREFIX + "位置信息", logs);
        }
    };

    private final CruiseObserver mCruiseObserver = new CruiseObserver() {
        @Override
        public void onCruiseLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            if (!isShowLane) {
                Logger.i(TAG, "lane hide");
                crossInfoDataBean.setTimestamp(System.currentTimeMillis());
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>());
                return;
            }
            if (ConvertUtils.isEmpty(laneInfoEntity)) {
                Logger.i(TAG, "laneInfoEntity null");
                crossInfoDataBean.setTimestamp(System.currentTimeMillis());
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>());
                return;
            }
            ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
            if (ConvertUtils.isEmpty(backLanes)) return;
            int LaneNum = backLanes.size();
            crossInfoDataBean.setTimestamp(System.currentTimeMillis());
            crossInfoDataBean.setLaneNum(LaneNum);
            crossInfoDataBean.setLaneTypes(backLanes);
            Logger.i(TAG, "巡航车道线", LaneNum, backLanes);
        }

        @Override
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            L2NaviBean.LimitCameraDataBean limitCameraData = l2NaviBean.getLimitCameraData();
            if (cruiseInfoEntity == null) {
                Logger.i(TAG, "cruiseInfoEntity null");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                return;
            }
            if (cruiseInfoEntity.getSpeed() == null) {
                Logger.i(TAG, "speed null");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
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
                Logger.i(TAG, "speed == 0");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                return;
            }
            if (cruiseInfoEntity.distance > 2000) { // 超过2km无需返回
                Logger.i(TAG, "over 2km");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                return;
            }
            limitCameraData.setSpdLmtEleEyeSpeedValue(speed);
            limitCameraData.setSpdLmtEleEyeDist(cruiseInfoEntity.distance);
            Logger.i(TAG, "巡航电子眼" + limitCameraData);
        }

        @Override
        public void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            if (cruiseInfoEntity == null) {
                Logger.i(TAG, "cruiseInfoEntity null");
                vehiclePosition.setRoadClass(-1);
                return;
            }
            vehiclePosition.setRoadClass(cruiseInfoEntity.roadClass);
            Logger.i(TAG, "巡航道路等级", cruiseInfoEntity.roadClass);
        }

        @Override
        public void setConfigKeyRoadWarn(boolean roadWarn) {

        }

        @Override
        public void setConfigKeySafeBroadcast(boolean safeBroadcast) {

        }

        @Override
        public void setConfigKeyDriveWarn(boolean driveWarn) {

        }

        @Override
        public void onUpdateCruiseFacility(CruiseFacilityEntity cruiseFacilityEntity) {
            L2NaviBean.WarningFacilityBean warningFacility = l2NaviBean.getWarningFacility();
            if (cruiseFacilityEntity == null) {
                Logger.i(TAG, "道路设施", "null");
                warningFacility.setBoardSignType(0);
                warningFacility.setBoardSignDist(0xFFFF);
                warningFacility.setLimitSpeed(0);
                return;
            }
            warningFacility.setBoardSignDist(cruiseFacilityEntity.getDistance());
            warningFacility.setBoardSignType(cruiseFacilityEntity.getType());
            if (warningFacility.getBoardSignType() == 10) {
                warningFacility.setLimitSpeed(cruiseFacilityEntity.getLimitSpeed());
            } else {
                warningFacility.setLimitSpeed(0);
            }
            Logger.i(TAG, "道路设施", warningFacility);
        }

        @Override
        public void onPlayTTS(SoundInfoEntity info) {

        }

        @Override
        public void onPlayRing(int type) {

        }

        @Override
        public void onNaviStop() {

        }
    };

    private final Runnable mTask = new Runnable() {
        @Override
        public void run() {
            try {
                sendMessage();
            } catch (Exception e) {
                Logger.e(TAG, "sendMessage: ", e);
            }
        }
    };

    public void registerCallback(L2DriveObserver driveObserver) {
        this.l2DriveObserver = driveObserver;
    }

    public void unregisterCallback(String packageName) {
        this.l2DriveObserver = null;
    }


    private void setNaviStatus(String naviStatus) {
        switch (naviStatus) {
            case NaviStatus.NaviStatusType.ROUTING:
            case NaviStatus.NaviStatusType.SELECT_ROUTE:
                l2NaviBean.getVehiclePosition().setNaviStatus(2);
                break;
            case NaviStatus.NaviStatusType.NAVING:
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                l2NaviBean.getVehiclePosition().setNaviStatus(1);
                break;
            case NaviStatus.NaviStatusType.CRUISE:
                l2NaviBean.getVehiclePosition().setNaviStatus(3);
                break;
            case NaviStatus.NaviStatusType.NO_STATUS:
            default:
                l2NaviBean.getVehiclePosition().setNaviStatus(0); //无效值或默认状态
                break;
        }
    }

    /**
     * 执行动作.
     */
    private void sendMessage() {
        if (ConvertUtils.isEmpty(l2DriveObserver)) {
            Logger.v(TAG, "l2++回调接口未注册");
        } else {
            if (mNaviEtaInfo != null) {
                NaviAdapter.getInstance().queryAppointLanesInfo(mNaviEtaInfo.curSegIdx, mNaviEtaInfo.curLinkIdx);
            }
            switch (mNaviStatus) {
                case NaviStatus.NaviStatusType.NAVING:
                case NaviStatus.NaviStatusType.LIGHT_NAVING:
                    PathInfo pathInfo = getPathInfo();
                    if (pathInfo != null) {
                        boolean online = pathInfo.isOnline();
                        l2NaviBean.getVehiclePosition().setNaviStatus(online ? 0x1 : 0x9);
                    } else {
                        l2NaviBean.getVehiclePosition().setNaviStatus(0x9);
                    }
                    break;
                default:
                    if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                        l2NaviBean.getVehiclePosition().setNaviStatus(0x9);
                    } else {
                        setNaviStatus(mNaviStatus);
                    }
            }
            Logger.d(TAG, "l2++导航状态", l2NaviBean.getVehiclePosition().getNaviStatus());

            LinkInfo linkInfo = getCurLinkInfo();
            if (linkInfo != null) {
                l2NaviBean.setIsServiceAreaRoad(linkInfo.isAtService() ? 1 : 0);
                int trafficLightDis = getTrafficLightDis(mNaviEtaInfo);
                l2NaviBean.getCrossInfoData().setHasTrafficLight(trafficLightDis == 0xFFFF ? 0 : 1);
                l2NaviBean.getCrossInfoData().setTrafficLightPosition(trafficLightDis);
                Logger.i(TAG, PREFIX + "红绿灯距离: " + trafficLightDis); // 0xFFFF表示无红绿灯, 0表示红绿灯就在附近
            }
            // 为防止并发问题，发送消息时暂停接口回调，防止修改l2NaviBean对象
            // unRegisterAdapterCallback();
            l2DriveObserver.onSdTbtDataChange(l2NaviBean);
            // registerAdapterCallback();
        }
    }

    private PathInfo getPathInfo() {
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath == null) {
            return null;
        }
        return (PathInfo) currentPath.getMPathInfo();
    }

    private LinkInfo getLinkInfo(int curSegIdx, int curLinkIdx) {
        PathInfo pathInfo = getPathInfo();
        if (pathInfo == null) {
            return null;
        }
        SegmentInfo segmentInfo = pathInfo.getSegmentInfo(curSegIdx);
        if (segmentInfo == null) {
            return null;
        }
        return segmentInfo.getLinkInfo(curLinkIdx);
    }

    private LinkInfo getCurLinkInfo() {
        if (mNaviEtaInfo == null) {
            return null;
        }
        int curSegIdx = mNaviEtaInfo.getCurSegIdx();
        int curLinkIdx = mNaviEtaInfo.getCurLinkIdx();
        return getLinkInfo(curSegIdx, curLinkIdx);
    }

    private long getLocationLinkIndex(int curSegIdx, int curLinkIdx) {
        long locationLinkIndex = 0;
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath != null) {
            PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
            if (pathInfo != null) {
                for (int i = 0; i < curSegIdx; i++) {
                    SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
                    if (segmentInfo != null) {
                        long linkCount = segmentInfo.getLinkCount();
                        locationLinkIndex += linkCount;
                    }
                }
            }
        }
        locationLinkIndex += curLinkIdx;
        return locationLinkIndex;
    }

    private int getLinkLength(int curSegIdx, int curLinkIdx) {
        LinkInfo linkInfo = getLinkInfo(curSegIdx, curLinkIdx);
        if (linkInfo == null) {
            return -1;
        }
        return linkInfo.getLength();
    }

    private int getRampDist(@NonNull NaviEtaInfo naviEtaInfo) {
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath == null) {
            return 0xFFFF;
        }
        PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
        if (pathInfo == null) {
            return 0xFFFF;
        }
        int curSegIdx = naviEtaInfo.getCurSegIdx();
        int curLinkIdx = naviEtaInfo.getCurLinkIdx();
        int rampDist = 0;
        for (int i = curSegIdx; i < pathInfo.getSegmentCount(); i++) {
            SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
            if (segmentInfo == null) {
                continue;
            }
            if (i != curSegIdx) {
                curLinkIdx = 0;
            }
            for (int j = curLinkIdx; j < segmentInfo.getLinkCount(); j++) {
                LinkInfo linkInfo = segmentInfo.getLinkInfo(j);
                if (linkInfo == null) {
                    continue;
                }
                if (i == curSegIdx && j == curLinkIdx) {
                    // 起始link
                    int formway = linkInfo.getFormway();
                    if (formway == Formway.FormwayJCT || formway == Formway.FormwaySlipRoad || formway == Formway.FormwaySlipJCT || formway == Formway.FormwayEntranceLink) {
                        // 匝道
                        return 0xFFFF;
                    } else {
                        // 目前未处于匝道
                        rampDist += naviEtaInfo.linkRemainDist;
                        if (rampDist > 2000) {
                            return 0xFFFF;
                        }
                    }
                } else {
                    // 非起始link
                    int formway = linkInfo.getFormway();
                    if (formway == Formway.FormwayJCT || formway == Formway.FormwaySlipRoad || formway == Formway.FormwaySlipJCT || formway == Formway.FormwayEntranceLink) {
                        // 遍历到匝道
                        return rampDist;
                    } else {
                        rampDist += linkInfo.getLength();
                        if (rampDist > 2000) {
                            return 0xFFFF;
                        }
                    }
                }
            }
        }
        return 0xFFFF;
    }

    private int getTrafficLightDis(@NonNull NaviEtaInfo naviEtaInfo) {
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath == null) {
            return 0xFFFF;
        }
        PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
        if (pathInfo == null) {
            return 0xFFFF;
        }
        int curSegIdx = naviEtaInfo.getCurSegIdx();
        int curLinkIdx = naviEtaInfo.getCurLinkIdx();
        int rampDist = 0;
        for (int i = curSegIdx; i < pathInfo.getSegmentCount(); i++) {
            SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
            if (segmentInfo == null) {
                continue;
            }
            if (i != curSegIdx) {
                curLinkIdx = 0;
            }
            for (int j = curLinkIdx; j < segmentInfo.getLinkCount(); j++) {
                LinkInfo linkInfo = segmentInfo.getLinkInfo(j);
                if (linkInfo == null) {
                    continue;
                }
                if (i == curSegIdx && j == curLinkIdx) {
                    // 起始link
                    rampDist = naviEtaInfo.linkRemainDist;
                } else {
                    // 非起始link
                    rampDist += linkInfo.getLength();
                }
                if (rampDist > 2000) {
                    return 0xFFFF;
                }
                // 遍历到红绿灯
                if (linkInfo.hasTrafficLight()) {
                    return rampDist;
                }
            }
        }
        return 0xFFFF;
    }

    private L2NaviBean.TunnelInfoBean getTunnelInfo(@NonNull NaviEtaInfo naviEtaInfo) {
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath == null) {
            return new L2NaviBean.TunnelInfoBean(0xFFFF, 0);
        }
        PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
        if (pathInfo == null) {
            return new L2NaviBean.TunnelInfoBean(0xFFFF, 0);
        }
        int curSegIdx = naviEtaInfo.getCurSegIdx();
        int curLinkIdx = naviEtaInfo.getCurLinkIdx();
        int tunnelLength = 0;
        int tunnelDistance = 0;
        int prevLinkType = -1;
        for (int i = curSegIdx; i < pathInfo.getSegmentCount(); i++) {
            SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
            if (segmentInfo == null) {
                continue;
            }
            if (i != curSegIdx) {
                curLinkIdx = 0;
            }
            for (int j = curLinkIdx; j < segmentInfo.getLinkCount(); j++) {
                LinkInfo linkInfo = segmentInfo.getLinkInfo(j);
                if (linkInfo == null) {
                    continue;
                }
                if (i == curSegIdx && j == curLinkIdx) {
                    // 起始link
                    if (linkInfo.getLinkType() == LinkType.LinkTypeTunnel) {
                        // 目前处于隧道
                        tunnelLength += naviEtaInfo.linkRemainDist;
                    } else {
                        // 目前未处于隧道
                        tunnelDistance += naviEtaInfo.linkRemainDist;
                        if (tunnelDistance > 2000) {
                            tunnelDistance = 0xFFFF;
                            tunnelLength = 0;
                            return new L2NaviBean.TunnelInfoBean(tunnelDistance, tunnelLength);
                        }
                    }
                } else {
                    // 非起始link
                    if (linkInfo.getLinkType() == LinkType.LinkTypeTunnel) {
                        // 遍历到隧道
                        tunnelLength += linkInfo.getLength();
                    } else {
                        if (prevLinkType == LinkType.LinkTypeTunnel) {
                            // 遍历到隧道
                            tunnelLength += linkInfo.getLength();
                            if (tunnelDistance == 0) {
                                tunnelDistance = 0xFFFF;
                            }
                            return new L2NaviBean.TunnelInfoBean(tunnelDistance, tunnelLength);
                        } else {
                            // 未遍历到隧道
                            tunnelDistance += linkInfo.getLength();
                            if (tunnelDistance > 2000) {
                                tunnelDistance = 0xFFFF;
                                tunnelLength = 0;
                                return new L2NaviBean.TunnelInfoBean(tunnelDistance, tunnelLength);
                            }
                        }
                    }
                }
                prevLinkType = linkInfo.getLinkType();
            }
        }
        return new L2NaviBean.TunnelInfoBean(0xFFFF, 0);
    }

    public int getLinkDist(long segIdx, long linkIdx) {
        if (mNaviEtaInfo == null) {
            return 0;
        }
        if (mNaviEtaInfo.curSegIdx > segIdx) {
            return 0;
        } else if (mNaviEtaInfo.curSegIdx == segIdx) {
            if (mNaviEtaInfo.curLinkIdx > linkIdx) {
                return 0;
            } else if (mNaviEtaInfo.curLinkIdx == linkIdx) {
                return mNaviEtaInfo.linkRemainDist;
            }
        }
        int trafficLightDis = 0;
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath != null) {
            PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
            if (pathInfo != null) {
                for (int i = mNaviEtaInfo.curSegIdx; i < pathInfo.getSegmentCount(); i++) {
                    SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
                    long linkCount = segmentInfo.getLinkCount();
                    int startLinkIdx = mNaviEtaInfo.curLinkIdx;
                    if (mNaviEtaInfo.curSegIdx != i) {
                        startLinkIdx = 0;
                    }
                    for (int j = startLinkIdx; j < linkCount; j++) {
                        LinkInfo infoLinkInfo = segmentInfo.getLinkInfo(j);
                        if (i > segIdx) {
                            return trafficLightDis;
                        } else if (i == segIdx) {
                            if (j > linkIdx) {
                                return trafficLightDis;
                            }
                        }
                        trafficLightDis += infoLinkInfo.getLength();

                    }
                }
            }
        }
        return trafficLightDis;
    }

    private long getLinkIndexDist(int curSegIdx, int curLinkIdx) {
        if (mNaviEtaInfo == null) {
            return -1;
        }
        if (mNaviEtaInfo.getCurSegIdx() == curSegIdx) {
            if (mNaviEtaInfo.getCurLinkIdx() == curLinkIdx) {
                return mNaviEtaInfo.linkRemainDist;
            }
            if (mNaviEtaInfo.getCurSegIdx() > curSegIdx) {
                return -1;
            }
        }
        if (mNaviEtaInfo.getCurSegIdx() > curSegIdx) {
            return -1;
        }
        long dist = 0;
        RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
        if (currentPath != null) {
            PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
            if (pathInfo != null) {
                for (int i = mNaviEtaInfo.getCurSegIdx(); i < pathInfo.getSegmentCount(); i++) {
                    SegmentInfo segmentInfo = pathInfo.getSegmentInfo(i);
                    long linkCount = segmentInfo.getLinkCount();
                    int startLinkIdx = mNaviEtaInfo.getCurLinkIdx();
                    if (mNaviEtaInfo.getCurSegIdx() != i) {
                        startLinkIdx = 0;
                    }
                    for (int j = startLinkIdx; j < linkCount; j++) {
                        if (i == mNaviEtaInfo.getCurSegIdx() && j == mNaviEtaInfo.getCurLinkIdx()) {
                            dist += mNaviEtaInfo.linkRemainDist;
                        } else {
                            if (segmentInfo.getLinkInfo(j) == null) {
                                continue;
                            }
                            dist += segmentInfo.getLinkInfo(j).getLength();
                        }
                        if (i == curSegIdx && j == curLinkIdx) {
                            if (dist > 2000) {
                                return -1;
                            } else {
                                return dist;
                            }
                        }
                        if (dist > 2000) {
                            return -1;
                        }
                    }
                }
            }
        }
        return -1;
    }
}
