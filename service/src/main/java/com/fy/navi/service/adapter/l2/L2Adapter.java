package com.fy.navi.service.adapter.l2;

import com.android.utils.ConvertUtils;
import com.fy.navi.service.adapter.navi.GuidanceObserver;
import com.fy.navi.service.adapter.navi.NaviAdapter;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.adapter.position.IPositionAdapterCallback;
import com.fy.navi.service.adapter.position.PositionAdapter;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.position.LocalParallelRoadEntity;

import java.util.ArrayList;
import java.util.List;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/3/12
 */
public class L2Adapter implements GuidanceObserver, INaviStatusCallback, IPositionAdapterCallback {
    private static final String TAG = L2Adapter.class.getSimpleName();
    private L2DriveObserver l2DriveObserver;
    private final L2NaviBean l2NaviBean;
    private final L2NaviBean.VehiclePositionBean vehiclePosition;
    private final L2NaviBean.GuidePointInfoBean guidePointInfoBean;
    private final L2NaviBean.CrossInfoDataBean crossInfoDataBean;
    private List<L2NaviBean.AheadIntersectionsBean> intersectionsBeans;

    private L2Adapter() {
        l2NaviBean = new L2NaviBean();
        vehiclePosition = new L2NaviBean.VehiclePositionBean();
        guidePointInfoBean = new L2NaviBean.GuidePointInfoBean();
        crossInfoDataBean = new L2NaviBean.CrossInfoDataBean();
        intersectionsBeans = new ArrayList<>();
        l2NaviBean.setVehiclePosition(vehiclePosition);
        l2NaviBean.setGuidePointInfo(guidePointInfoBean);
        l2NaviBean.setCrossInfoData(crossInfoDataBean);
        l2NaviBean.setAheadIntersections(intersectionsBeans);
    }

    public static L2Adapter getInstance() {
        return Helper.la;
    }

    public void registerCallback(L2DriveObserver driveObserver) {
        this.l2DriveObserver = driveObserver;
        NaviAdapter.getInstance().registerObserver(getClass().getSimpleName(), this);
        NavistatusAdapter.getInstance().registerCallback(this);
        PositionAdapter.getInstance().registerCallback(this);
    }

    public void unregisterCallback(String packageName) {
        this.l2DriveObserver = null;
    }

    /*** 道路属性 **/
    public void graspRouteResult(L2NaviBean.VehiclePositionBean vehicleP) {
        vehiclePosition.setLocationLinkOffset(vehicleP.getLocationLinkOffset());
        vehiclePosition.setRoadClass(vehicleP.getRoadClass());
        vehiclePosition.setLocationLongitude(vehicleP.getLocationLongitude());
        vehiclePosition.setLocationLatitude(vehicleP.getLocationLatitude());
        vehiclePosition.setRoadOwnership(vehicleP.getRoadOwnership());
        if (ConvertUtils.isEmpty(l2DriveObserver)) return;
        l2DriveObserver.onSelectRouteIndex(l2NaviBean);
    }

    /*** 引导信息 **/
    @Override
    public void onNaviInfo(NaviEtaInfo naviEtaInfo) {
        vehiclePosition.setCurPathID((int) naviEtaInfo.pathID); // 当前选择得路线下标
        vehiclePosition.setLocationLinkIndex(naviEtaInfo.curLinkIdx); //自车当前位置的link索引，跟全局路线中的link索引对应
        vehiclePosition.setDistToDestination(naviEtaInfo.allDist); // 当前位置到目的地距离
        vehiclePosition.setCurrentSpeedLimit(naviEtaInfo.curLinkSpeed); // 当前自车所在道路限速
        guidePointInfoBean.setNextGuideDist(naviEtaInfo.nextDist); // 到下一个引导点的剩余距离（导航：2000米以内发出）
        guidePointInfoBean.setNextGuideType(naviEtaInfo.nextManeuverID); // 引导点动作高德原始接口（导航：2000米以内发出）.
        ArrayList<NaviEtaInfo.NaviCrossNaviInfo> naviCrossNaviInfos = naviEtaInfo.nextCrossInfo;
        crossInfoDataBean.setTrafficLightPosition(naviEtaInfo.nextDist);
        if (ConvertUtils.isEmpty(naviCrossNaviInfos)) return;
        NaviEtaInfo.NaviCrossNaviInfo nextNaviInfo = naviCrossNaviInfos.get(0);
        /*** 发起下个路口的车道线信息得请求{@link L2Adapter#onLaneInfoReceived} **/
        NaviAdapter.getInstance().queryAppointLanesInfo(nextNaviInfo.segmentIndex, nextNaviInfo.linkIndex);

    }

    @Override
    public void onPlayTTS(SoundInfoEntity pInfo) {
        vehiclePosition.setTtsText(pInfo.getText()); // 语音播报对应的文本（导航状态，前方一个）
    }

    @Override
    public void onNaviStop() {

    }

    /*** 引导状态回调 **/
    @Override
    public void onNaviStatusChange(String naviStatus) {
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
    }

    @Override
    public void onParallelRoadUpdate(LocParallelInfoEntity entity) {
        vehiclePosition.setMainSideRots(entity.getFlag());// 自车所在主辅路状态判断信号
        ArrayList<LocalParallelRoadEntity> parallelRoadEntities = entity.getLocalParallelRoadArrayList();
        if (ConvertUtils.isEmpty(parallelRoadEntities)) return;
        LocalParallelRoadEntity parallelRoadEntity = parallelRoadEntities.get(0);
        vehiclePosition.setFormway(parallelRoadEntity.getFormWay()); // 自车所在路段的formway信息
        vehiclePosition.setLinkType(parallelRoadEntity.getLinkType()); // 自车所在路段的道路类型
    }

    @Override
    public void onLaneInfoReceived(LaneInfoEntity laneInfoEntity) {
        if (ConvertUtils.isEmpty(laneInfoEntity)) return;
        ArrayList<Integer> backLanes = laneInfoEntity.getBackLane();
        if (ConvertUtils.isEmpty(backLanes)) return;
        int LaneNum = laneInfoEntity.getBackLane().size();
        List<Integer> highLightLanes = new ArrayList<>();
        List<Integer> highLightLaneTypes = new ArrayList<>();
        for (int i = 0; i < LaneNum; i++) {
            int recommend = laneInfoEntity.getOptimalLane().get(i) != NaviConstant.LaneAction.LaneActionNULL ? 1 : 0;
            highLightLanes.add(recommend);
            for (int k = 0; k < NaviConstant.LaneActionConstants.naviLaneMap.length; k++) {
                if (NaviConstant.LaneActionConstants.naviLaneMap[k][0] != laneInfoEntity.getBackLane().get(i))
                    continue;
                if (NaviConstant.LaneActionConstants.naviLaneMap[k][1] != laneInfoEntity.getFrontLane().get(i))
                    continue;
                highLightLaneTypes.add(NaviConstant.LaneActionConstants.naviLaneMap[k][2]);
            }
        }
        crossInfoDataBean.setLaneNum(LaneNum); // 下个路口车道数
        crossInfoDataBean.setHighLightLanes(highLightLanes); // 设置推荐车道
        crossInfoDataBean.setHighLightLaneTypes(highLightLaneTypes); // 设置表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
        crossInfoDataBean.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
        crossInfoDataBean.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
        crossInfoDataBean.setLaneTypes(backLanes); // 下个路口所有车道通行方向
        crossInfoDataBean.setSegmentIndex(laneInfoEntity.segmentIdx); // 与link_index结合使用，用于映射到导航路径上
        crossInfoDataBean.setLinkIndex(laneInfoEntity.linkIdx); // 与segment_index结合使用，用于映射到导航路径上
        // 前方推荐车道信息列表
        L2NaviBean.AheadIntersectionsBean aheadIntersection = new L2NaviBean.AheadIntersectionsBean();
        aheadIntersection.setLaneNum(highLightLanes.size()); // 下个路口车道数
        aheadIntersection.setLinkIndex(laneInfoEntity.linkIdx); // 与segment_index结合使用，用于映射到导航路径上
        aheadIntersection.setSegmentIndex(laneInfoEntity.segmentIdx); // 与link_index结合使用，用于映射到导航路径上
        aheadIntersection.setLaneTypes(backLanes); // 下个路口所有车道通行方向
        aheadIntersection.setHighLightLaneTypes(highLightLaneTypes); // 表达的是每个车道可以通行的方向，比如自车直行过路口的时候，直行加右转车道对应的就是直行
        aheadIntersection.setFrontLaneType(laneInfoEntity.getFrontLaneType()); // 引导点处可通行车道的类型信息
        aheadIntersection.setBackLaneType(laneInfoEntity.getBackLaneType()); // 引导点处所有车道的类型信息
    }

    @Override
    public void onUpdateTrafficLightCountdown(int isHaveTrafficLight, GeoPoint geoPoint) {
        crossInfoDataBean.setHasTrafficLight(isHaveTrafficLight == 0 ? 0 : 1); // 路口是否有红绿灯
    }

    @Override
    public void onShowSameDirectionMixForkInfo(List<NaviMixForkInfo> list) {
        if(ConvertUtils.isEmpty(list)) return;
        List<L2NaviBean.MixForksBean> mixForksBeanList = l2NaviBean.getMixForks(); // 混淆路口信息列表
        for (NaviMixForkInfo mixForkInfo : list) {
            if(ConvertUtils.isEmpty(mixForkInfo)) continue;
            L2NaviBean.MixForksBean mixForksBean = new L2NaviBean.MixForksBean();
            mixForksBean.setDistance(mixForkInfo.getDist());
            mixForksBean.setRoadClass(mixForkInfo.getRoadclass());
            mixForksBean.setSegmentIndex(mixForkInfo.getSegmentIndex());
            mixForksBean.getPosition().setX(mixForkInfo.pos.lat);
            mixForksBean.getPosition().setY(mixForkInfo.pos.lon);
            mixForksBeanList.add(mixForksBean);
        }


    }

    private static final class Helper {
        private static final L2Adapter la = new L2Adapter();
    }
}
