package com.fy.navi.service.adapter.l2;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
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
import com.fy.navi.service.define.position.LocalParallelRoadEntity;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.ScheduledFuture;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Adapter implements GuidanceObserver, INaviStatusCallback, IPositionAdapterCallback {
    private static final String TAG = L2Adapter.class.getSimpleName();
    private L2DriveObserver l2DriveObserver;
    private volatile L2NaviBean l2NaviBean;
    private ScheduledFuture scheduledFuture;
    private String mNaviStatus;

    private L2Adapter() {
        l2NaviBean = new L2NaviBean();
        l2NaviBean.setVehiclePosition(new L2NaviBean.VehiclePositionBean());
        l2NaviBean.setGuidePointInfo(new L2NaviBean.GuidePointInfoBean());
        l2NaviBean.setCrossInfoData(new L2NaviBean.CrossInfoDataBean());
        l2NaviBean.setAheadIntersections(new ArrayList<>());
        registerAdapterCallback();
    }

    public static L2Adapter getInstance() {
        return Helper.la;
    }

    public void registerCallback(L2DriveObserver driveObserver) {
        this.l2DriveObserver = driveObserver;
    }

    public void unregisterCallback(String packageName) {
        this.l2DriveObserver = null;
    }

    /*** 道路属性 **/
    public void graspRouteResult(L2NaviBean.VehiclePositionBean vehicleP) {
        L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
        vehiclePosition.setLocationLinkOffset(vehicleP.getLocationLinkOffset()); // 自车绑路后的link上offset，距离link起点的距离
        vehiclePosition.setLocationLongitude(vehicleP.getLocationLongitude()); // 自车经度坐标（在sd route上的）
        vehiclePosition.setLocationLatitude(vehicleP.getLocationLatitude()); // 自车纬度坐标（在sd route上的
        vehiclePosition.setRoadOwnership(vehicleP.getRoadOwnership()); // 自车所在道路所有权
        if (ConvertUtils.isEmpty(l2DriveObserver)) return;
        l2DriveObserver.onSdTbtDataChange(GsonUtils.toJson(l2NaviBean));
        Logger.i(TAG, "道路属性 :", l2NaviBean);
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
        l2NaviBean.setEndParkingInfo(endParkingInfo);
        if (ConvertUtils.isEmpty(l2DriveObserver)) return;
        l2DriveObserver.onSdTbtDataChange(GsonUtils.toJson(l2NaviBean));
        Logger.i(TAG, "设置停车场信息 :", l2NaviBean);
//        sendMessage("setEndParkInfo");
    }

    /*** 引导信息 **/
    @Override
    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
        L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();

        vehiclePosition.setCurPathID((int) naviEtaInfo.pathID); // 当前选择得路线下标
        vehiclePosition.setDistToDestination(naviEtaInfo.getAllDist()); // 当前位置到目的地距离
        vehiclePosition.setLocationLinkIndex(naviEtaInfo.curLinkIdx); //自车当前位置的link索引，跟全局路线中的link索引对应

        vehiclePosition.setCurrentSpeedLimit(naviEtaInfo.curLinkSpeed); // 当前自车所在道路限速
        vehiclePosition.setRoadClass(naviEtaInfo.curRoadClass); // 当前自车所在道路等级

        l2NaviBean.getGuidePointInfo().setNextGuideDist(naviEtaInfo.getNextDist()); // 到下一个引导点的剩余距离（导航：2000米以内发出）
        int maneuverId = naviEtaInfo.getNextManeuverID();
        l2NaviBean.getGuidePointInfo().setNextGuideType(maneuverId); // 引导点动作高德原始接口（导航：2000米以内发出）.
        if (16 == maneuverId) {
            L2NaviBean.TunnelInfoBean tunnelInfoBean = new L2NaviBean.TunnelInfoBean();
            tunnelInfoBean.setTunnelDist(naviEtaInfo.getNextDist());
            l2NaviBean.setTunnelInfo(tunnelInfoBean);
        }

        crossInfoDataBean.setTrafficLightPosition(naviEtaInfo.getNextDist());

        ArrayList<NaviEtaInfo.NaviCrossNaviInfo> naviCrossNaviInfos = naviEtaInfo.nextCrossInfo;
        if (!ConvertUtils.isEmpty(naviCrossNaviInfos)) {
            NaviEtaInfo.NaviCrossNaviInfo nextNaviInfo = naviCrossNaviInfos.get(0);
            /*** 发起下个路口的车道线信息得请求{@link L2Adapter#onLaneInfoReceived} **/
            NaviAdapter.getInstance().queryAppointLanesInfo(nextNaviInfo.segmentIndex, nextNaviInfo.linkIndex);
        }
        Logger.i(TAG, "引导信息 :", l2NaviBean);
        sendMessage("");
    }

    @Override
    public void onPlayTTS(SoundInfoEntity pInfo) {
        L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
        vehiclePosition.setTtsText(pInfo.getText()); // 语音播报对应的文本（导航状态，前方一个）
        Logger.i(TAG, "语音播报 :", l2NaviBean);
        sendMessage("onPlayTTS");
    }

    @Override
    public void onNaviStop() {

    }

    /*** 引导状态回调 **/
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
                vehiclePosition.setNaviStatus(2); //无效值或默认状态
                break;
            case NaviStatus.NaviStatusType.NAVING:
                vehiclePosition.setNaviStatus(1); //无效值或默认状态
                break;
            case NaviStatus.NaviStatusType.CRUISE:
                vehiclePosition.setNaviStatus(3); //无效值或默认状态
                break;
        }
        if (ConvertUtils.isEmpty(l2DriveObserver)) return;
        l2DriveObserver.onSdTbtDataChange(GsonUtils.toJson(l2NaviBean));
        Logger.i(TAG, "引导状态回调 :", l2NaviBean);
//        sendMessage("onNaviStatusChange");
    }

    @Override
    public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
        L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
        vehiclePosition.setMainSideRots(entity.getFlag());// 自车所在主辅路状态判断信号
        ArrayList<LocalParallelRoadEntity> parallelRoadEntities = entity.getLocalParallelRoadArrayList();
        if (ConvertUtils.isEmpty(parallelRoadEntities)) return;
        LocalParallelRoadEntity parallelRoadEntity = parallelRoadEntities.get(0);
        vehiclePosition.setFormWay(parallelRoadEntity.getFormWay()); // 自车所在路段的formway信息
        vehiclePosition.setLinkType(parallelRoadEntity.getLinkType()); // 自车所在路段的道路类型
        Logger.i(TAG, "主辅路 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
        if (!isShowLane) {
            crossInfoDataBean.setLaneNum(0);
            crossInfoDataBean.setHighLightLanes(new ArrayList<>());
            crossInfoDataBean.setHighLightLaneTypes(new ArrayList<>());
            crossInfoDataBean.setBackLaneType(new ArrayList<>());
            crossInfoDataBean.setFrontLaneType(new ArrayList<>());
            crossInfoDataBean.setLaneTypes(new ArrayList<>());
            crossInfoDataBean.setSegmentIndex(0);
            crossInfoDataBean.setLinkIndex(0);
            crossInfoDataBean.setTimestamp(0);
            l2NaviBean.setHasTidalLane(0);
            l2NaviBean.setAheadIntersections(new ArrayList<>());
            return;
        }
        boolean hasTidalLane = false;
        if (ConvertUtils.isEmpty(laneInfoEntity)) return;
        ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
        if (ConvertUtils.isEmpty(backLanes)) return;
        int LaneNum = laneInfoEntity.getBackLane().size();
        List<Integer> highLightLanes = new ArrayList<>();
        List<Integer> highLightLaneTypes = new ArrayList<>();
        for (int i = 0; i < LaneNum; i++) {
            int recommend = laneInfoEntity.getOptimalLane().get(i) != NaviConstant.LaneAction.LANE_ACTION_NULL ? 1 : 0;
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
        crossInfoDataBean.setLaneNum(LaneNum); // 下个路口车道数
        crossInfoDataBean.setHighLightLanes(highLightLanes); // 设置推荐车道
        crossInfoDataBean.setHighLightLaneTypes(highLightLaneTypes); // 设置表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
        crossInfoDataBean.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
        crossInfoDataBean.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
        crossInfoDataBean.setLaneTypes(backLanes); // 下个路口所有车道通行方向
        crossInfoDataBean.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
        crossInfoDataBean.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
        crossInfoDataBean.setTimestamp(System.currentTimeMillis());
        // 前方推荐车道信息列表
        List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = new ArrayList<>();
        L2NaviBean.AheadIntersectionsBean aheadIntersection = new L2NaviBean.AheadIntersectionsBean();
        aheadIntersection.setLaneNum(highLightLanes.size()); // 下个路口车道数
        aheadIntersection.setLinkIndex(laneInfoEntity.getLinkIdx()); // 与segment_index结合使用，用于映射到导航路径上
        aheadIntersection.setSegmentIndex(laneInfoEntity.getSegmentIdx()); // 与link_index结合使用，用于映射到导航路径上
        aheadIntersection.setLaneTypes(backLanes); // 下个路口所有车道通行方向
        aheadIntersection.setHighLightLaneTypes(highLightLaneTypes); // 表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
        aheadIntersection.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
        aheadIntersection.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
        aheadIntersection.setTimestamp(System.currentTimeMillis());
        aheadIntersections.add(aheadIntersection);
        l2NaviBean.setAheadIntersections(aheadIntersections);
        Logger.i(TAG, "车道线 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onUpdateTrafficLightCountdown(int isHaveTrafficLight, GeoPoint geoPoint) {
        l2NaviBean.getCrossInfoData().setHasTrafficLight(isHaveTrafficLight == 0 ? 0 : 1); // 路口是否有红绿灯
        Logger.i(TAG, "红绿灯 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list) {
        if (ConvertUtils.isEmpty(list)) {
            Logger.i(TAG, "list null");
            l2NaviBean.setMixForks(new ArrayList<>());
            return;
        }
        List<L2NaviBean.MixForksBean> mixForksBeanList = l2NaviBean.getMixForks(); // 混淆路口信息列表
        for (int i = 0; i < list.size(); i++) {
//        for (NaviMixForkInfo mixForkInfo : list) {
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
        Logger.i(TAG, "混淆路口 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    /**
     * 最近的限速电子眼
     *
     * @param cameraInfo 电子眼实体
     */
    @Override
    public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
        if (ConvertUtils.isEmpty(cameraInfo)) {
            Logger.i(TAG, "cameraInfo null");
            L2NaviBean.LimitCameraDataBean limitCameraDataBean = new L2NaviBean.LimitCameraDataBean();
            limitCameraDataBean.setSpdLmtEleEyeSpeedValue(255);
            l2NaviBean.setLimitCameraData(limitCameraDataBean);
            return;
        }
        if (cameraInfo.getDistance() > 2000) { // 超过2km无需返回
            Logger.i(TAG, "over 2km");
            L2NaviBean.LimitCameraDataBean limitCameraDataBean = new L2NaviBean.LimitCameraDataBean();
            limitCameraDataBean.setSpdLmtEleEyeSpeedValue(255);
            l2NaviBean.setLimitCameraData(limitCameraDataBean);
            return;
        }
        L2NaviBean.LimitCameraDataBean limitCameraData = new L2NaviBean.LimitCameraDataBean();
        limitCameraData.setSpdLmtEleEyeDist(cameraInfo.getDistance());
        limitCameraData.setSpdLmtEleEyeSpeedValue(cameraInfo.getSpeed());
        l2NaviBean.setLimitCameraData(limitCameraData);
        Logger.i(TAG, "电子眼 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    /**
     * 区间测速
     *
     * @param speedEntity 车速实体
     */
    @Override
    public void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
        if (ConvertUtils.isEmpty(speedEntity)) {
            Logger.d(TAG, "speedEntity null");
            l2NaviBean.setIntervalCameraData(new L2NaviBean.IntervalCameraDataBean());
            return;
        }
        L2NaviBean.IntervalCameraDataBean intervalCameraDataBean = new L2NaviBean.IntervalCameraDataBean();
        intervalCameraDataBean.setIntervalCameraStartPointDist(speedEntity.getDistance() - speedEntity.getRemainDistance());
        intervalCameraDataBean.setIntervalCameraEndPointDist(speedEntity.getRemainDistance());
        ArrayList<Short> speedList = speedEntity.getLimitSpeedList();
        Short speed = 0;
        for (Short i : speedList) {
            if (i != 0) {
                speed = i;
            }
        }
        intervalCameraDataBean.setIntervalCameraSpeedValue(speed);
        Logger.d(TAG, "setIntervalCameraData:" + GsonUtils.toJson(intervalCameraDataBean));
        l2NaviBean.setIntervalCameraData(intervalCameraDataBean);
        Logger.i(TAG, "区间测速 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onCurrentRoadSpeed(int speed) {
        L2NaviBean.WarningFacilityBean warningFacilityBean = new L2NaviBean.WarningFacilityBean();
        if (0 != speed) {
            warningFacilityBean.setLimitSpeed(speed); // 警示牌限速值
        }
        l2NaviBean.setWarningFacility(warningFacilityBean);
        Logger.i(TAG, "当前限速 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
        if (ConvertUtils.isEmpty(sapaInfoEntity)) {
            Logger.i(TAG, "sapaInfoEntity null");
            return;
        }
        int type = sapaInfoEntity.getType();
        ArrayList<SapaInfoEntity.SAPAItem> sapaItems = sapaInfoEntity.getList();
        if (ConvertUtils.isEmpty(sapaItems)) {
            l2NaviBean.setTollStationDist(0); // 集合为空，代表没有收费站信息
        } else {
            SapaInfoEntity.SAPAItem sapaItem = sapaItems.get(0);
            int distance = sapaItem.getRemainDist();
            if (NaviConstant.SapaItemsType.TOLL_STATION_LIST == type
                    || NaviConstant.SapaItemsType.TOLL_STATION_AND_SPAS == type) {
                l2NaviBean.setTollStationDist(distance); // 收费站距离
            } else {
                l2NaviBean.setTollStationDist(0); // 与前方收费站剩余距离 0表示没有收费站
                if (100 < distance) {
                    l2NaviBean.setIsServiceAreaRoad(1); // 当前道路是否服务区内 是
                } else {
                    l2NaviBean.setIsServiceAreaRoad(0); // 当前道路是否服务区内 否
                }
            }
        }
        Logger.i(TAG, "当前限速 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    @Override
    public void onManeuverInfo(NaviManeuverInfo respData) {
        if (!ConvertUtils.isEmpty(respData)) {
            l2NaviBean.setRampDist(respData.getDisToCurrentPos()); // 到前方匝道口距离
        }
        Logger.i(TAG, "匝道信息 :", l2NaviBean);
        sendMessage("onParallelRoadUpdate");
    }

    /**
     * 开始周期下发消息到智能驾驶.
     */
    public void startPeriodicTask() {
        if (!ConvertUtils.isEmpty(scheduledFuture)) {
            return;
        }
        registerAdapterCallback();
        scheduledFuture = ThreadManager.getInstance().asyncWithFixDelay(() -> sendMessage(""), 1, 3);
    }

    /**
     * 暂停消息下发.
     */
    public void stopPeriodicTask() {
        ThreadManager.getInstance().cancelDelayRun(scheduledFuture);
        unRegisterAdapterCallback();
    }

    /**
     * 执行动作.
     */
    private void sendMessage(String method) {
        if (ConvertUtils.isEmpty(l2DriveObserver)) {
            Logger.i(TAG, "l2++回调接口未注册");
        } else {
            // 为防止并发问题，发送消息时暂停接口回调，防止修改l2NaviBean对象
            // unRegisterAdapterCallback();
            l2DriveObserver.onSdTbtDataChange(GsonUtils.toJson(l2NaviBean));
            // registerAdapterCallback();
        }
    }

    /**
     * 向其他模块注册数据回到接口.
     */
    private void registerAdapterCallback() {
        NaviAdapter.getInstance().registerObserver(getClass().getSimpleName(), this);
        NavistatusAdapter.getInstance().registerCallback(this);
        PositionAdapter.getInstance().registerCallback(this);
    }

    /**
     * 向其他模块反注册数据回到接口.
     */
    private void unRegisterAdapterCallback() {
        NaviAdapter.getInstance().unregisterObserver(getClass().getSimpleName());
        NavistatusAdapter.getInstance().unRegisterCallback(this);
        PositionAdapter.getInstance().unregisterCallback(this);
    }

    private static final class Helper {
        private static final L2Adapter la = new L2Adapter();
    }
}
