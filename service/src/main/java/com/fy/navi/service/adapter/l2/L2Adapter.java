package com.fy.navi.service.adapter.l2;

import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.path.model.LinkType;
import com.autonavi.gbl.common.path.option.LinkInfo;
import com.autonavi.gbl.common.path.option.PathInfo;
import com.autonavi.gbl.common.path.option.SegmentInfo;
import com.autonavi.gbl.guide.model.ManeuverIconID;
import com.autonavi.gbl.layer.model.BizLayerUtil;
import com.fy.navi.service.adapter.cruise.CruiseAdapter;
import com.fy.navi.service.adapter.cruise.CruiseObserver;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.adapter.route.RouteAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.logicpaket.position.PositionPackage;

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

    private L2DriveObserver l2DriveObserver;
    private volatile L2NaviBean l2NaviBean;
    private String mNaviStatus;
    private ScheduledExecutorService mScheduler;
    private NaviEtaInfo mNaviEtaInfo;

    private L2Adapter() {
        l2NaviBean = new L2NaviBean();
        l2NaviBean.setCrossInfoData(new L2NaviBean.CrossInfoDataBean()); // 组合
        l2NaviBean.setVehiclePosition(new L2NaviBean.VehiclePositionBean()); // 组合
        l2NaviBean.setIntervalCameraData(new L2NaviBean.IntervalCameraDataBean());
        l2NaviBean.setLimitCameraData(new L2NaviBean.LimitCameraDataBean());
        l2NaviBean.setTunnelInfo(new L2NaviBean.TunnelInfoBean()); // 隧道，暂缺少隧道长度
        l2NaviBean.setGuidePointInfo(new L2NaviBean.GuidePointInfoBean());
        l2NaviBean.setWarningFacility(new L2NaviBean.WarningFacilityBean()); // 警示牌，
        l2NaviBean.setAheadIntersections(new ArrayList<>());
        l2NaviBean.setMixForks(new ArrayList<>());
//        l2NaviBean.setEndParkingInfo(new L2NaviBean.EndParkingInfo());
        registerAdapterCallback();
        mScheduler = Executors.newScheduledThreadPool(1);
        mScheduler.scheduleWithFixedDelay(mTask, 0, 1, TimeUnit.SECONDS);
    }

    public static L2Adapter getInstance() {
        return Helper.la;
    }

    private static final class Helper {
        private static final L2Adapter la = new L2Adapter();
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
                Logger.i(TAG, "naviEtaInfo null");
                return;
            }
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();

            vehiclePosition.setCurPathID(naviEtaInfo.pathID); // 当前选择得路线下标
            vehiclePosition.setDistToDestination(naviEtaInfo.getAllDist()); // 当前位置到目的地距离
            vehiclePosition.setLocationLinkIndex(naviEtaInfo.curLinkIdx); //自车当前位置的link索引，跟全局路线中的link索引对应

            //vehiclePosition.setCurrentSpeedLimit(naviEtaInfo.curLinkSpeed); // 当前自车所在道路限速
            vehiclePosition.setRoadClass(naviEtaInfo.curRoadClass); // 当前自车所在道路等级

            L2NaviBean.GuidePointInfoBean guidePointInfo = l2NaviBean.getGuidePointInfo();
            guidePointInfo.setNextGuideDist(naviEtaInfo.getNextDist()); // 到下一个引导点的剩余距离（导航：2000米以内发出）
            int maneuverId = naviEtaInfo.getNextManeuverID();
            if (maneuverId == -1) {
                maneuverId = 0;
            }
            guidePointInfo.setNextGuideType(maneuverId); // 引导点动作高德原始接口（导航：2000米以内发出）.

            L2NaviBean.TunnelInfoBean tunnelInfoBean = l2NaviBean.getTunnelInfo();
            if (ManeuverIconID.ManeuverIconTunnel == maneuverId) {
                tunnelInfoBean.setTunnelDist(naviEtaInfo.getNextDist());
                tunnelInfoBean.setTunnelLength(0);
                LinkInfo linkInfo = getCurLinkInfo();
                if (linkInfo != null) {
                    if (linkInfo.getLinkType() == LinkType.LinkTypeTunnel) {
                        tunnelInfoBean.setTunnelLength(linkInfo.getLength());
                    }
                }
            } else {
                tunnelInfoBean.setTunnelDist(0xFFFF);
                tunnelInfoBean.setTunnelLength(0);
            }

//            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
//            crossInfoDataBean.setTrafficLightPosition(naviEtaInfo.getNextDist());

            ArrayList<NaviEtaInfo.NaviCrossNaviInfo> naviCrossNaviInfos = naviEtaInfo.nextCrossInfo;
            if (!ConvertUtils.isEmpty(naviCrossNaviInfos)) {
                NaviEtaInfo.NaviCrossNaviInfo nextNaviInfo = naviCrossNaviInfos.get(0);
                Logger.i(TAG, "交叉路口信息", nextNaviInfo);
                /*** 发起下个路口的车道线信息得请求{@link L2Adapter#onLaneInfoReceived} **/
                NaviAdapter.getInstance().queryAppointLanesInfo(nextNaviInfo.segmentIndex, nextNaviInfo.linkIndex);
            }
            Logger.i(TAG, "引导信息", naviEtaInfo.getNextDist(), guidePointInfo, tunnelInfoBean, vehiclePosition);
        }

        @Override
        public void onPlayTTS(SoundInfoEntity pInfo) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            String text = pInfo.getText();
            vehiclePosition.setTtsText(text); // 语音播报对应的文本（导航状态，前方一个）
            Logger.i(TAG, "语音播报", text);
        }

        @Override
        public void onLaneInfoReceived(LaneInfoEntity laneInfoEntity) {
            if (laneInfoEntity == null || laneInfoEntity.getBackLane() == null) {
                Logger.i(TAG, "lane null");
                L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>()); // 下个路口所有车道通行方向

                List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
                if (aheadIntersections.isEmpty()) {
                    aheadIntersections.add(new L2NaviBean.AheadIntersectionsBean());
                }
                L2NaviBean.AheadIntersectionsBean aheadIntersection = aheadIntersections.get(0);
                aheadIntersection.setLaneNum(0); // 下个路口车道数
                aheadIntersection.setLaneTypes(new ArrayList<>());
                return;
            }
            Logger.i(TAG, "下一个路口车道信息", laneInfoEntity);
            ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            crossInfoDataBean.setLaneNum(backLanes.size());
            crossInfoDataBean.setLaneTypes(backLanes); // 下个路口所有车道通行方向

            List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
            if (aheadIntersections.isEmpty()) {
                aheadIntersections.add(new L2NaviBean.AheadIntersectionsBean());
            }
            L2NaviBean.AheadIntersectionsBean aheadIntersection = aheadIntersections.get(0);
            aheadIntersection.setLaneNum(backLanes.size()); // 下个路口车道数
            aheadIntersection.setLaneTypes(laneInfoEntity.getBackLane()); // 下个路口所有车道通行方向
        }

        @Override
        public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            if (!isShowLane) {
                Logger.i(TAG, "lane hide");
                crossInfoDataBean.setHighLightLanes(null);
                crossInfoDataBean.setHighLightLaneTypes(null);
                crossInfoDataBean.setBackLaneType(null);
                crossInfoDataBean.setFrontLaneType(null);
                crossInfoDataBean.setSegmentIndex(-1);
                crossInfoDataBean.setLinkIndex(-1);
                crossInfoDataBean.setTimestamp(System.currentTimeMillis());
                l2NaviBean.setHasTidalLane(0);
                l2NaviBean.setAheadIntersections(new ArrayList<>());
                return;
            }
            if (ConvertUtils.isEmpty(laneInfoEntity)) {
                Logger.i(TAG, "laneInfoEntity null");
                return;
            }
            boolean hasTidalLane = false;
            ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
            if (ConvertUtils.isEmpty(backLanes)) return;
            int LaneNum = laneInfoEntity.getBackLane().size();
            List<Integer> highLightLanes = new ArrayList<>();
            List<Integer> highLightLaneTypes = new ArrayList<>();
            for (int i = 0; i < LaneNum; i++) {
                int recommend = laneInfoEntity.getFrontLane().get(i) != NaviConstant.LaneAction.LANE_ACTION_NULL ? 1 : 0;
                highLightLanes.add(recommend);
                if (NaviConstant.LANE_TYPE_TIDAL == laneInfoEntity.getBackLaneType().get(i)) {
                    hasTidalLane = true;
                }
                for (int k = 0; k < NaviConstant.LaneActionConstants.NAVI_LANE_MAP.length; k++) {
                    if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][0] != laneInfoEntity.getBackLane().get(i))
                        continue;
                    if (NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][1] != laneInfoEntity.getFrontLane().get(i))
                        continue;
                    highLightLaneTypes.add(NaviConstant.LaneActionConstants.NAVI_LANE_MAP[k][2]);
                }
            }
            if (hasTidalLane) {
                l2NaviBean.setHasTidalLane(1); // 有潮汐车道
            } else {
                l2NaviBean.setHasTidalLane(0); // 没有潮汐车道
            }
            crossInfoDataBean.setHighLightLanes(highLightLanes); // 设置推荐车道
            crossInfoDataBean.setHighLightLaneTypes(highLightLaneTypes); // 设置表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
            crossInfoDataBean.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
            crossInfoDataBean.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
            crossInfoDataBean.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
            crossInfoDataBean.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
            crossInfoDataBean.setTimestamp(System.currentTimeMillis());
            // 前方推荐车道信息列表
            List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
            if (aheadIntersections.isEmpty()) {
                aheadIntersections.add(new L2NaviBean.AheadIntersectionsBean());
            }
            L2NaviBean.AheadIntersectionsBean aheadIntersection = aheadIntersections.get(0);
            aheadIntersection.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
            aheadIntersection.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
            aheadIntersection.setHighLightLaneTypes(highLightLaneTypes); // 表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
            aheadIntersection.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
            aheadIntersection.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
            aheadIntersection.setTimestamp(System.currentTimeMillis());
//            aheadIntersections.add(aheadIntersection);


            Logger.i(TAG, "车道线1", l2NaviBean.getHasTidalLane(), l2NaviBean.getCrossInfoData());
            Logger.i(TAG, "车道线2", l2NaviBean.getAheadIntersections());
            Logger.i(TAG, "车道线3", l2NaviBean.getCrossInfoData().getHighLightLanes());
        }

        @Override
        public void onUpdateTrafficLightCountdown(int isHaveTrafficLight, GeoPoint geoPoint) {
            l2NaviBean.getCrossInfoData().setHasTrafficLight(isHaveTrafficLight == 0 ? 0 : 1); // 路口是否有红绿灯
            if (isHaveTrafficLight == 0) {
                l2NaviBean.getCrossInfoData().setTrafficLightPosition(0);
            } else {
                GeoPoint currentGeo = PositionPackage.getInstance().currentGeo;
                double distance = BizLayerUtil.calcDistanceBetweenPoints(new Coord2DDouble(currentGeo.getLon(), currentGeo.getLat()), new Coord2DDouble(geoPoint.getLon(), geoPoint.getLat()));
                l2NaviBean.getCrossInfoData().setTrafficLightPosition((int) distance);
            }

            Logger.i(TAG, "红绿灯", isHaveTrafficLight);
        }

        @Override
        public void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list) {
            if (ConvertUtils.isEmpty(list)) {
                Logger.i(TAG, "list null");
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
            Logger.i(TAG, "混淆路口", mixForksBeanList);
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
                Logger.i(TAG, "cameraInfo null");
                limitCameraData.setSpdLmtEleEyeDist(0);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getDistance() > 2000) { // 超过2km无需返回
                Logger.i(TAG, "over 2km");
                limitCameraData.setSpdLmtEleEyeDist(0);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getSpeed() == 0) {
                Logger.i(TAG, "speed 0");
                limitCameraData.setSpdLmtEleEyeDist(0);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            limitCameraData.setSpdLmtEleEyeDist(cameraInfo.getDistance());
            limitCameraData.setSpdLmtEleEyeSpeedValue(cameraInfo.getSpeed());
            Logger.i(TAG, "电子眼", limitCameraData);
        }

        /**
         * 区间测速
         *
         * @param speedEntity 车速实体
         */
        @Override
        public void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
            if (ConvertUtils.isEmpty(speedEntity)) {
                Logger.i(TAG, "speedEntity null");
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
            Logger.i(TAG, "区间测速", intervalCameraDataBean);
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            vehiclePosition.setCurrentSpeedLimit(speed);
            L2NaviBean.WarningFacilityBean warningFacilityBean = l2NaviBean.getWarningFacility();
            warningFacilityBean.setLimitSpeed(speed); // 警示牌限速值
            warningFacilityBean.setBoardSignType(10);
//            warningFacilityBean.setBoardSignDist();
            Logger.i(TAG, "当前限速", speed);
        }

        @Override
        public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
            if (ConvertUtils.isEmpty(sapaInfoEntity)) {
                Logger.i(TAG, "sapaInfoEntity null");
                l2NaviBean.setTollStationDist(0xFFFF);
                return;
            }
            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = sapaInfoEntity.getList();
            if (ConvertUtils.isEmpty(sapaItems)) {
                l2NaviBean.setTollStationDist(0xFFFF); // 集合为空，代表没有收费站信息
            } else {
                int tollStationDist = -1;
                for (int i = 0; i < sapaItems.size(); i++) {
                    SapaInfoEntity.SAPAItem sapaItem = sapaItems.get(i);
                    int distance = sapaItem.getRemainDist();
                    // 隐含条件是按距离最近排序
                    if (NaviConstant.SapaItemsType.SPAS_LIST == sapaItem.getType() && tollStationDist == -1) {
                        tollStationDist = distance;
                    }
                }
                if (tollStationDist != -1) {
                    l2NaviBean.setTollStationDist(tollStationDist); // 收费站距离
                } else {
                    l2NaviBean.setTollStationDist(0xFFFF); // 与前方收费站剩余距离 0xFFFF表示没有收费站
                }
            }
            Logger.i(TAG, "收费站距离", l2NaviBean.getTollStationDist());
        }

        @Override
        public void onManeuverInfo(NaviManeuverInfo respData) {
            if (ConvertUtils.isEmpty(respData)) {
                l2NaviBean.setRampDist(0xFFFF);
            } else {
                int disToCurrentPos = respData.getDisToCurrentPos();
                if (disToCurrentPos == 0) {
                    l2NaviBean.setRampDist(0xFFFF);
                } else if (disToCurrentPos < 2000) {
                    l2NaviBean.setRampDist(disToCurrentPos); // 到前方匝道口距离
                } else {
                    l2NaviBean.setRampDist(0xFFFF);
                }
            }
            Logger.i(TAG, "匝道信息", l2NaviBean.getRampDist());
        }

        @Override
        public void onNaviStop() {

        }
    };

    private final INaviStatusCallback mINaviStatusCallback = new INaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            if (naviStatus == null) {
                Logger.i(TAG, "naviStatus null");
                return;
            }
            if (mNaviStatus == null) {
                mNaviStatus = naviStatus;
            } else {
                if (mNaviStatus.equals(naviStatus)) {
                    return;
                }
            }
            l2NaviBean.clear();
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            switch (naviStatus) {
                case NaviStatus.NaviStatusType.NO_STATUS:
                    vehiclePosition.setNaviStatus(0); //无效值或默认状态
                    break;
                case NaviStatus.NaviStatusType.ROUTING:
                    vehiclePosition.setNaviStatus(2);
                    break;
                case NaviStatus.NaviStatusType.NAVING:
                    vehiclePosition.setNaviStatus(1);
                    break;
                case NaviStatus.NaviStatusType.CRUISE:
                    vehiclePosition.setNaviStatus(3);
                    break;
            }
            Logger.i(TAG, "引导状态回调", naviStatus);
        }
    };

    private final IPositionAdapterCallback mIPositionAdapterCallback = new IPositionAdapterCallback() {
        @Override
        public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
            L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
            if (ConvertUtils.isEmpty(entity)) {
                Logger.i(TAG, "entity null");
                vpb.setMainSideRots(0);
                return;
            }
            vpb.setMainSideRots(entity.getFlag());
            Logger.i(TAG, "主辅路", vpb.getMainSideRots());
        }
    };

    private final CruiseObserver mCruiseObserver = new CruiseObserver() {
        @Override
        public void onCruiseLaneInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        }

        @Override
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            L2NaviBean.LimitCameraDataBean limitCameraData = l2NaviBean.getLimitCameraData();
            if (cruiseInfoEntity == null) {
                Logger.i(TAG, "cruiseInfoEntity null");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(-1);
                return;
            }
            if (cruiseInfoEntity.getSpeed() == null) {
                Logger.i(TAG, "speed null");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(-1);
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
                limitCameraData.setSpdLmtEleEyeDist(-1);
                return;
            }
            if (cruiseInfoEntity.distance > 2000) { // 超过2km无需返回
                Logger.i(TAG, "over 2km");
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                limitCameraData.setSpdLmtEleEyeDist(-1);
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
                vehiclePosition.setRoadClass(0xFF);
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
        public void onPlayTTS(SoundInfoEntity info) {

        }

        @Override
        public void onNaviStop() {

        }
    };

    /*** 道路属性 **/
    public void graspRouteResult(L2NaviBean.VehiclePositionBean value) {
        L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
        vpb.setLocationLinkOffset(value.getLocationLinkOffset()); // 自车绑路后的link上offset，距离link起点的距离
        vpb.setLocationLongitude(value.getLocationLongitude()); // 自车经度坐标（在sd route上的）
        vpb.setLocationLatitude(value.getLocationLatitude()); // 自车纬度坐标（在sd route上的
        vpb.setRoadOwnership(value.getRoadOwnership()); // 自车所在道路所有权
        Logger.i(TAG, "道路属性", vpb.getLocationLinkOffset(), vpb.getLocationLongitude(), vpb.getLocationLatitude(), vpb.getRoadOwnership());
    }

    /**
     * 设置停车场信息.
     *
     * @param enterX 停车场入口的坐标X
     * @param enterY 停车场入口的坐标Y
     * @param exitX  停车场出口的坐标X
     * @param exitY  停车场出口的坐标Y
     */
    public void setEndParkInfo(double enterX, double enterY, double exitX, double exitY) {
        L2NaviBean.EndParkingInfo endParkingInfo = new L2NaviBean.EndParkingInfo(enterX, enterY, exitX, exitY);
//        l2NaviBean.setEndParkingInfo(endParkingInfo);
        Logger.i(TAG, "设置停车场信息", endParkingInfo);
    }

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

    /**
     * 执行动作.
     */
    private void sendMessage() {
        if (ConvertUtils.isEmpty(l2DriveObserver)) {
            Logger.v(TAG, "l2++回调接口未注册");
        } else {
            LinkInfo linkInfo = getCurLinkInfo();
            if (linkInfo != null) {
                L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
                vpb.setFormWay(linkInfo.getFormway());
                vpb.setLinkType(linkInfo.getLinkType());
                l2NaviBean.setIsServiceAreaRoad(linkInfo.isAtService() ? 1 : 0);
            }
            // 为防止并发问题，发送消息时暂停接口回调，防止修改l2NaviBean对象
            // unRegisterAdapterCallback();
            l2DriveObserver.onSdTbtDataChange(l2NaviBean);
            // registerAdapterCallback();
        }
    }

    private LinkInfo getCurLinkInfo() {
        if (mNaviEtaInfo != null) {
            int curSegIdx = mNaviEtaInfo.getCurSegIdx();
            int curLinkIdx = mNaviEtaInfo.getCurLinkIdx();
            RouteCurrentPathParam currentPath = RouteAdapter.getInstance().getCurrentPath(MapType.MAIN_SCREEN_MAIN_MAP);
            if (currentPath != null) {
                PathInfo pathInfo = (PathInfo) currentPath.getMPathInfo();
                if (pathInfo != null) {
                    SegmentInfo segmentInfo = pathInfo.getSegmentInfo(curSegIdx);
                    if (segmentInfo != null) {
                        return segmentInfo.getLinkInfo(curLinkIdx);
                    }
                }
            }
        }
        return null;
    }
}
