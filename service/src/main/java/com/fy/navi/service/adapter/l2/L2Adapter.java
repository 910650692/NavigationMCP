package com.fy.navi.service.adapter.l2;

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
import com.fy.navi.service.adapter.route.RouteResultObserver;
import com.fy.navi.service.define.cruise.CruiseFacilityEntity;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.RouteLineLayerParam;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.L2NaviBean;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.NaviRoadFacilityEntity;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.TrafficLightCountdownEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteAlongCityParam;
import com.fy.navi.service.define.route.RouteAlterChargeStationParam;
import com.fy.navi.service.define.route.RouteChargeStationParam;
import com.fy.navi.service.define.route.RouteCurrentPathParam;
import com.fy.navi.service.define.route.RouteL2Data;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RouteRestAreaParam;
import com.fy.navi.service.define.route.RouteRestTollGateParam;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.route.RouteTrafficIncidentParam;
import com.fy.navi.service.define.route.RouteWeatherParam;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchParkInOutInfo;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
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
    private int mTaskId;
    private L2NaviBean.EndParkingInfo mEndParkingInfo = new L2NaviBean.EndParkingInfo();

    private L2Adapter() {
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
        RouteAdapter.getInstance().registerRouteObserver(TAG, mRouteResultObserver);
        SearchPackage.getInstance().registerCallBack(TAG, mSearchResultCallback);
    }

    /**
     * 向其他模块反注册数据回到接口.
     */
    private void unRegisterAdapterCallback() {
        NaviAdapter.getInstance().unregisterObserver(getClass().getSimpleName());
        NavistatusAdapter.getInstance().unRegisterCallback(mINaviStatusCallback);
        PositionAdapter.getInstance().unregisterCallback(mIPositionAdapterCallback);
    }

    private final SearchResultCallback mSearchResultCallback = new SearchResultCallback() {
        @Override
        public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
        }

        @Override
        public void onSilentSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {
            if (taskId != mTaskId) {
                return;
            }
            Logger.i(TAG, "收到停车场信息");
            if (searchResultEntity == null) {
                return;
            }
            List<PoiInfoEntity> mPoiList = searchResultEntity.getMPoiList();
            if (mPoiList == null || mPoiList.isEmpty()) {
                return;
            }
            PoiInfoEntity poiInfoEntity = mPoiList.get(0);
            if (poiInfoEntity == null) {
                return;
            }
            List<ParkingInfo> parkingInfoList = poiInfoEntity.getParkingInfoList();
            if (parkingInfoList == null || parkingInfoList.isEmpty()) {
                return;
            }
            Logger.i(TAG, "收到停车场信息", parkingInfoList);
            ParkingInfo parkingInfo = parkingInfoList.get(0);
            if (parkingInfo == null) {
                return;
            }
            List<SearchParkInOutInfo> mSearchParkInOutInfos = parkingInfo.getMSearchParkInOutInfos();
            if (mSearchParkInOutInfos == null) {
                return;
            }
            for (int i = 0; i < mSearchParkInOutInfos.size(); i++) {
                SearchParkInOutInfo searchParkInOutInfo = mSearchParkInOutInfos.get(i);
                if (searchParkInOutInfo == null) {
                    continue;
                }
                if ("出口".equals(searchParkInOutInfo.getKeytype())) {
                    mEndParkingInfo.setMParkingExit(searchParkInOutInfo.getX(), searchParkInOutInfo.getY());
                } else if ("入口".equals(searchParkInOutInfo.getKeytype())) {
                    mEndParkingInfo.setMParkingEnter(searchParkInOutInfo.getX(), searchParkInOutInfo.getY());
                }
            }
        }
    };

    private final RouteResultObserver mRouteResultObserver = new RouteResultObserver() {
        @Override
        public void onRouteSuccess(String successMsg) {

        }

        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            if (requestRouteResult == null) {
                return;
            }
            if (Objects.equals(NavistatusAdapter.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
                searchParking();
            }
        }

        @Override
        public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {

        }

        @Override
        public void onRouteRestAreaInfo(RouteRestAreaParam routeRestAreaParam) {

        }

        @Override
        public void onRouteWeatherInfo(RouteWeatherParam routeWeatherParam) {

        }

        @Override
        public void onRouteChargeStationInfo(RouteChargeStationParam routeChargeStationParam) {

        }

        @Override
        public void onRouteAlterChargeStationInfo(RouteAlterChargeStationParam routeAlterChargeStationParam) {

        }

        @Override
        public void onRouteRestrictionInfo(RouteRestrictionParam routeRestrictionParam) {

        }

        @Override
        public void onRouteRestTollGateInfo(RouteRestTollGateParam routeRestTollGateParam) {

        }

        @Override
        public void onRouteCityInfo(RouteAlongCityParam routeAlongCityParam) {

        }

        @Override
        public void onRouteTrafficIncidentInfo(RouteTrafficIncidentParam routeTrafficIncidentParam) {

        }

        @Override
        public void onRouteRanges(ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {

        }

        @Override
        public void onRouteFail(RequestRouteResult requestRouteResult, int errorCode, String errorMsg, long requestId) {

        }

        @Override
        public void onRouteL2Info(RouteL2Data routeL2Data) {

        }

        @Override
        public void onRouteTMCInfo(RouteTMCParam param) {

        }

        @Override
        public void onReRoute() {

        }
    };

    private void searchParking() {
        RouteParam endPoint = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        if (endPoint == null) {
            return;
        }
//        Logger.i(TAG, "开始搜索停车场");
//        mTaskId = SearchPackage.getInstance().aroundSearch(1, "停车场",
//                endPoint.getRealPos(), "2000", true);
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
            vehiclePosition.setLocationLinkIndex(getLocationLinkIndex(naviEtaInfo.curSegIdx, naviEtaInfo.curLinkIdx)); //自车当前位置的link索引，跟全局路线中的link索引对应
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

            Logger.i(TAG, "引导信息", naviEtaInfo.getNextDist(), guidePointInfo, tunnelInfo, rampDist, vehiclePosition);

            if (naviEtaInfo.getAllDist() <= 2000) {
                RouteParam endPoint = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
                if (endPoint != null) {
                    PoiInfoEntity mPoiInfoEntity = endPoint.getMPoiInfoEntity();
                    if (mPoiInfoEntity == null) {
                        return;
                    }
                    List<ParkingInfo> parkingInfoList = mPoiInfoEntity.getParkingInfoList();
                    if (parkingInfoList == null || parkingInfoList.isEmpty()) {
                        return;
                    }
                    ParkingInfo parkingInfo = parkingInfoList.get(0);
                    if (parkingInfo == null) {
                        return;
                    }
                    List<SearchParkInOutInfo> mSearchParkInOutInfos = parkingInfo.getMSearchParkInOutInfos();
                    if (mSearchParkInOutInfos == null) {
                        return;
                    }
                    L2NaviBean.EndParkingInfo endParkingInfo = l2NaviBean.getEndParkingInfo();
                    for (int i = 0; i < mSearchParkInOutInfos.size(); i++) {
                        SearchParkInOutInfo searchParkInOutInfo = mSearchParkInOutInfos.get(i);
                        if (searchParkInOutInfo == null) {
                            continue;
                        }
                        if ("出口".equals(searchParkInOutInfo.getKeytype())) {
                            endParkingInfo.setMParkingExit(searchParkInOutInfo.getX(), searchParkInOutInfo.getY());
                        } else if ("入口".equals(searchParkInOutInfo.getKeytype())) {
                            endParkingInfo.setMParkingEnter(searchParkInOutInfo.getX(), searchParkInOutInfo.getY());
                        }
                    }
                }
//                l2NaviBean.setEndParkingInfo(mEndParkingInfo);
            }
        }

        @Override
        public void onPlayTTS(SoundInfoEntity pInfo) {
            L2NaviBean.VehiclePositionBean vehiclePosition = l2NaviBean.getVehiclePosition();
            String text = pInfo.getText();
            vehiclePosition.setTtsText(text); // 语音播报对应的文本（导航状态，前方一个）
            Logger.i(TAG, "语音播报", text);
        }

        @Override
        public void onPlayRing(int type) {

        }

        @Override
        public void onLaneInfoReceived(ArrayList<LaneInfoEntity> laneInfoList) {
            if (laneInfoList == null || laneInfoList.isEmpty()) {
                Logger.i(TAG, "lane null");
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
            }
            Logger.i(TAG, "下一个路口车道信息", laneInfoList);
        }

        @Override
        public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            if (!isShowLane) {
                Logger.i(TAG, "lane hide");
                crossInfoDataBean.setHighLightLanes(new ArrayList<>());
                crossInfoDataBean.setHighLightLaneTypes(new ArrayList<>());
                crossInfoDataBean.setBackLaneType(new ArrayList<>());
                crossInfoDataBean.setFrontLaneType(new ArrayList<>());
                crossInfoDataBean.setSegmentIndex(-1);
                crossInfoDataBean.setLinkIndex(-1);
                crossInfoDataBean.setTimestamp(0);
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>());
                l2NaviBean.setHasTidalLane(0);
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

            Logger.i(TAG, "当前车道线", l2NaviBean.getHasTidalLane(), l2NaviBean.getCrossInfoData());
        }

        @Override
        public void onUpdateTrafficLightCountdown(final ArrayList<TrafficLightCountdownEntity> list) {
            if (ConvertUtils.isEmpty(list)) {
                Logger.i(TAG, "红绿灯 null 1");
                l2NaviBean.getCrossInfoData().setTrafficLightPosition(0xFFFF);
                l2NaviBean.getCrossInfoData().setHasTrafficLight(0);
                return;
            }
            TrafficLightCountdownEntity lightCountdown = list.get(0);
            if (lightCountdown == null) {
                Logger.i(TAG, "红绿灯 null 2");
                l2NaviBean.getCrossInfoData().setTrafficLightPosition(0xFFFF);
                l2NaviBean.getCrossInfoData().setHasTrafficLight(0);
                return;
            }
            int trafficLightDis = getTrafficLightDis(lightCountdown.getMSegmentIndex(), lightCountdown.getMLinkIndex());
            l2NaviBean.getCrossInfoData().setTrafficLightPosition(trafficLightDis);
            l2NaviBean.getCrossInfoData().setHasTrafficLight(1);
            Logger.i(TAG, "红绿灯", trafficLightDis);
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
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getDistance() > 2000) { // 超过2km无需返回
                Logger.i(TAG, "over 2km");
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
                limitCameraData.setSpdLmtEleEyeSpeedValue(0xFF);
                return;
            }
            if (cameraInfo.getSpeed() == 0) {
                Logger.i(TAG, "speed 0");
                limitCameraData.setSpdLmtEleEyeDist(0xFFFF);
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

            L2NaviBean.WarningFacilityBean warningFacility = l2NaviBean.getWarningFacility();
            if (warningFacility.getBoardSignType() == 10) {
                warningFacility.setLimitSpeed(speed);
            } else {
                warningFacility.setLimitSpeed(0);
            }
            Logger.i(TAG, "当前限速", speed);
        }

        @Override
        public void onNaviSAPAInfo(SapaInfoEntity sapaInfoEntity) {
            Logger.i(TAG, sapaInfoEntity);
            if (ConvertUtils.isEmpty(sapaInfoEntity)) {
                Logger.i(TAG, "sapaInfoEntity null");
                l2NaviBean.setTollStationDist(0xFFFF);
                return;
            }
            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = sapaInfoEntity.getList();
            if (ConvertUtils.isEmpty(sapaItems)) {
                Logger.i(TAG, "sapaItems null");
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
            Logger.i(TAG, "收费站距离", l2NaviBean.getTollStationDist());
        }

        @Override
        public void onNaviStop() {

        }

        @Override
        public void onShowNaviFacility(ArrayList<NaviRoadFacilityEntity> naviRoadFacilityEntitys) {
            L2NaviBean.WarningFacilityBean warningFacility = l2NaviBean.getWarningFacility();
            if (naviRoadFacilityEntitys == null || naviRoadFacilityEntitys.isEmpty()) {
                Logger.i(TAG, "道路设施", "null");
                warningFacility.setBoardSignType(0);
                warningFacility.setBoardSignDist(0xFFFF);
                return;
            }
            NaviRoadFacilityEntity naviRoadFacilityEntity = naviRoadFacilityEntitys.get(0);
            warningFacility.setBoardSignType(naviRoadFacilityEntity.getType());
            warningFacility.setBoardSignDist(naviRoadFacilityEntity.getDistance());
            Logger.i(TAG, "道路设施", naviRoadFacilityEntity);
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
            mNaviEtaInfo = null;
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
                    searchParking();
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

        @Override
        public void onLocationInfo(LocInfoBean locationInfo) {
            L2NaviBean.VehiclePositionBean vpb = l2NaviBean.getVehiclePosition();
            vpb.setLocationLongitude(locationInfo.getLongitude());
            vpb.setLocationLatitude(locationInfo.getLatitude());
            vpb.setFormWay(locationInfo.getFormway());
            vpb.setLinkType(locationInfo.getLinkType());
            int ownership = locationInfo.getOwnership();
            vpb.setRoadOwnership(ownership == -1 ? 0 : ownership);
            Logger.i(TAG, "位置信息", locationInfo.getLongitude(), locationInfo.getLatitude(), locationInfo.getFormway(), locationInfo.getLinkType(), locationInfo.getOwnership());
        }
    };

    private final CruiseObserver mCruiseObserver = new CruiseObserver() {
        @Override
        public void onCruiseLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            L2NaviBean.CrossInfoDataBean crossInfoDataBean = l2NaviBean.getCrossInfoData();
            if (!isShowLane) {
                Logger.i(TAG, "lane hide");
                crossInfoDataBean.setTimestamp(0);
                crossInfoDataBean.setLaneNum(0);
                crossInfoDataBean.setLaneTypes(new ArrayList<>());
                return;
            }
            if (ConvertUtils.isEmpty(laneInfoEntity)) {
                Logger.i(TAG, "laneInfoEntity null");
                crossInfoDataBean.setTimestamp(0);
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

            LinkInfo linkInfo = getCurLinkInfo();
            if (linkInfo != null) {
                l2NaviBean.setIsServiceAreaRoad(linkInfo.isAtService() ? 1 : 0);
//                if (linkInfo.hasTrafficLight() && mNaviEtaInfo != null) {
//                    l2NaviBean.getCrossInfoData().setHasTrafficLight(1);
//                    l2NaviBean.getCrossInfoData().setTrafficLightPosition(mNaviEtaInfo.getLinkRemainDist());
//                } else {
//                    l2NaviBean.getCrossInfoData().setHasTrafficLight(0);
//                    l2NaviBean.getCrossInfoData().setTrafficLightPosition(0xFFFF);
//                }
            }
            if (Boolean.FALSE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
                l2NaviBean.getVehiclePosition().setNaviStatus(0x9);
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

    public int getTrafficLightDis(long segIdx, long linkIdx) {
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
