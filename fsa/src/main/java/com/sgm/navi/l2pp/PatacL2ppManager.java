package com.sgm.navi.l2pp;

import android.content.Context;
import android.content.Intent;

import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.cls.core.ClsLink;
import com.cls.core.Topic;
import com.cls.core.subscription.v3.CreateTopicRequest;
import com.cls.core.v1.SubscriptionClient;
import com.cls.vehicle.adas.map.v1.GeoPointCoordinates;
import com.cls.vehicle.adas.map.v1.MpilotEgoVehicleInfo;
import com.cls.vehicle.adas.map.v1.MpilotNavigationBoardSign;
import com.cls.vehicle.adas.map.v1.MpilotNavigationElectronicEyeSpeedLimit;
import com.cls.vehicle.adas.map.v1.MpilotNavigationGuidePoint;
import com.cls.vehicle.adas.map.v1.MpilotNavigationInformation;
import com.cls.vehicle.adas.map.v1.MpilotNavigationIntersection;
import com.cls.vehicle.adas.map.v1.MpilotNavigationMixFork;
import com.cls.vehicle.adas.map.v1.MpilotNavigationSectionSpeedLimit;
import com.cls.vehicle.adas.map.v1.MpilotNavigationTunnel;
import com.cls.vehicle.adas.map.v1.MpilotSDRoute;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteEndPoi;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteGuideGroups;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteLinks;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteList;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteRestTollGateInfos;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteSegments;
import com.cls.vehicle.adas.map.v1.MpilotSDRouteViaRoad;
import com.cls.vehicle.adas.map.v1.ParkingLotInfoList;
import com.sgm.navi.adas.JsonLog;
import com.sgm.navi.fsa.BuildConfig;
import com.sgm.navi.fsa.R;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.define.navi.L2NaviBean;
import com.sgm.navi.service.define.route.RouteL2Data;
import com.sgm.navi.service.define.signal.SignalConst;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.l2.L2InfoCallback;
import com.sgm.navi.service.logicpaket.l2.L2Package;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.google.protobuf.Any;
import com.sgm.cls.sdk.uprotocol.cloudevent.datamodel.CloudEventAttributes;
import com.sgm.cls.sdk.uprotocol.cloudevent.factory.CloudEventFactory;

import java.util.List;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import io.cloudevents.CloudEvent;

/**
 * PATAC L2++ 管理类
 */
public class PatacL2ppManager {
    private static final String TAG = PatacL2ppManager.class.getSimpleName();

    private static final String TBT_URI = "cls:/adas.map/1/mpilot_navigation#MpilotNavigationInformation";
    private static final String ROUTE_URI = "cls:/adas.map/1/mpilot_navigation#MpilotSDRouteList";

    private ClsLink mClsLink;
    private SubscriptionClient.FutureStub mUSubscription;
    private boolean mInitialized = false;
    private boolean mTbtTopic = false;
    private boolean mRouteTopic = false;
    private String mTtsStr;
    private ScheduledFuture mScheduledFuture;

    //region INSTANCE
    public static PatacL2ppManager getInstance() {
        return PatacL2ppManager.SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final PatacL2ppManager INSTANCE = new PatacL2ppManager();
    }

    private PatacL2ppManager() {
    }
    //endregion

    //region 初始化反初始化
    public void init() {
        // 标定配置判断
        if (CalibrationPackage.getInstance().adasConfigurationType() != 9) {
            Logger.i(TAG, "not PATAC L2++ configuration");
            return;
        }
        Logger.d(TAG, "init start");
        ExecutorService executor = new ThreadPoolExecutor(16, Integer.MAX_VALUE,
                60, TimeUnit.SECONDS, new LinkedBlockingQueue<>());
        Context context = AppCache.getInstance().getMContext();
        mClsLink = ClsLink.create(context, executor, (link, ready) -> {
            // ClsLink连接状态回调
            Logger.d(TAG, "clslink connection status callback: " , ready);
            if (ready) {
                if (mInitialized) {
                    Logger.i(TAG, "initialized");
                    return;
                }
                mClsLink = link;
                L2Package.getInstance().registerCallback(TAG, mL2InfoCallback);
                RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
                SignalPackage.getInstance().registerObserver(TAG, mSignalCallback);
                mUSubscription = SubscriptionClient.newFutureStub(link);
                // 创建tbt数据服务
                createTopic(TBT_URI);
                // 创建route数据服务
                createTopic(ROUTE_URI);
                mInitialized = true;
            } else {
                if (!mInitialized) {
                    Logger.i(TAG, "not initialized");
                    return;
                }
                L2Package.getInstance().unregisterCallback(TAG);
                RoutePackage.getInstance().unRegisterRouteObserver(TAG);
                SignalPackage.getInstance().unregisterObserver(TAG);
                mTbtTopic = false;
                mRouteTopic = false;
                mInitialized = false;
            }
        });
        // ClsLink连接
        mClsLink.connect();
        Logger.d(TAG, "init end");
    }

    private void createTopic(String uri) {
        if (mUSubscription == null) {
            Logger.d(TAG, "mUSubscription == null");
            return;
        }
        CreateTopicRequest createTopicRequest = CreateTopicRequest.newBuilder()
                .setTopic(Topic.newBuilder().setUri(uri).build())
                .build();
        mUSubscription.createTopic(createTopicRequest).whenComplete((status, throwable) -> {
            Logger.d(TAG, "create topic result: " , status.getCode() , ", " , uri);
            if (TBT_URI.equals(uri) && status.getCode() == 0) {
                Logger.d(TAG, "tbt topic create success");
                mTbtTopic = true;
            }
            if (ROUTE_URI.equals(uri) && status.getCode() == 0) {
                Logger.d(TAG, "route topic create success");
                mRouteTopic = true;
            }
            if (mTbtTopic && mRouteTopic) {
                Logger.d(TAG, "send topic create success broadcast");
                Intent intent = new Intent("com.sgm.navi.hmi.topic.release");
                AppCache.getInstance().getMContext().sendBroadcast(intent);
            }
        });
        Logger.d(TAG, "createTopic: " , uri);
    }
    //endregion

    //region SD Map功能
    private final L2InfoCallback mL2InfoCallback = new L2InfoCallback() {
        @Override
        public void onSdTbtDataChange(L2NaviBean l2NaviBean) {
            if (l2NaviBean == null) {
                Logger.w(TAG, "onSdTbtDataChange: l2NaviBean null");
                return;
            }
            Logger.d(TAG, "send tbt data: " , l2NaviBean);
            String json = GsonUtils.toJson(l2NaviBean);
            if (Logger.isDebugLevel()) JsonLog.saveJsonToCache(json, "l2.json", "l2_tbt");
            MpilotNavigationInformation.Builder mpilotNavigationInformation = MpilotNavigationInformation.newBuilder();

            MpilotNavigationBoardSign.Builder mpilotNavigationBoardSign = MpilotNavigationBoardSign.newBuilder();
            mpilotNavigationBoardSign.setBoardSignType(l2NaviBean.getWarningFacility().getBoardSignType());
            mpilotNavigationBoardSign.setDistanceToBoardSign(l2NaviBean.getWarningFacility().getBoardSignDist());
            mpilotNavigationBoardSign.setLimitSpeed(l2NaviBean.getWarningFacility().getLimitSpeed());
            mpilotNavigationInformation.setMpilotNavigationBoardSign(mpilotNavigationBoardSign);

            MpilotNavigationElectronicEyeSpeedLimit.Builder mpilotNavigationElectronicEyeSpeedLimit = MpilotNavigationElectronicEyeSpeedLimit.newBuilder();
            mpilotNavigationElectronicEyeSpeedLimit.setSpeedLimitElectronicEyeDistance(l2NaviBean.getLimitCameraData().getSpdLmtEleEyeDist());
            mpilotNavigationElectronicEyeSpeedLimit.setSpeedLimitElectronicEyeSpeedValue(l2NaviBean.getLimitCameraData().getSpdLmtEleEyeSpeedValue());
            mpilotNavigationInformation.setMpilotNavigationElectronicEyeSpeedLimit(mpilotNavigationElectronicEyeSpeedLimit);

            MpilotEgoVehicleInfo.Builder mpilotEgoVehicleInfo = MpilotEgoVehicleInfo.newBuilder();
            mpilotEgoVehicleInfo.setCurrentPathId(l2NaviBean.getVehiclePosition().getCurPathID());
            mpilotEgoVehicleInfo.setCurrentSpeedLimit(l2NaviBean.getVehiclePosition().getCurrentSpeedLimit());
            mpilotEgoVehicleInfo.setDistanceToDestination(l2NaviBean.getVehiclePosition().getDistToDestination());
            GeoPointCoordinates.Builder locationCoordinates = GeoPointCoordinates.newBuilder();
            locationCoordinates.setLongitudeX(l2NaviBean.getVehiclePosition().getLocationLongitude());
            locationCoordinates.setLatitudeY(l2NaviBean.getVehiclePosition().getLocationLatitude());
            mpilotEgoVehicleInfo.setLocationCoordinates(locationCoordinates);
            mpilotEgoVehicleInfo.setLocationLinkIndex((int) l2NaviBean.getVehiclePosition().getLocationLinkIndex());
            mpilotEgoVehicleInfo.setLocationLinkOffset(l2NaviBean.getVehiclePosition().getLocationLinkOffset());
            mpilotEgoVehicleInfo.setMainSideRots(l2NaviBean.getVehiclePosition().getMainSideRots());
            mpilotEgoVehicleInfo.setNaviStatus(l2NaviBean.getVehiclePosition().getNaviStatus());
            mpilotEgoVehicleInfo.setRoadClass(l2NaviBean.getVehiclePosition().getRoadClass());
            mpilotEgoVehicleInfo.setRoadOwnership(l2NaviBean.getVehiclePosition().getRoadOwnership());
            mpilotEgoVehicleInfo.setTtsText(l2NaviBean.getVehiclePosition().getTtsText());
            mpilotEgoVehicleInfo.setFormway(l2NaviBean.getVehiclePosition().getFormWay());
            mpilotEgoVehicleInfo.setLinkType(l2NaviBean.getVehiclePosition().getLinkType());
            mpilotNavigationInformation.setMpilotEgoVehicleInfo(mpilotEgoVehicleInfo);

            MpilotNavigationGuidePoint.Builder mpilotNavigationGuidePoint = MpilotNavigationGuidePoint.newBuilder();
            mpilotNavigationGuidePoint.setDistanceToNextGuidePoint(l2NaviBean.getGuidePointInfo().getNextGuideDist());
            mpilotNavigationGuidePoint.setOriginIconType(l2NaviBean.getGuidePointInfo().getNextGuideType());
            mpilotNavigationInformation.setMpilotNavigationGuidePoint(mpilotNavigationGuidePoint);

            MpilotNavigationSectionSpeedLimit.Builder mpilotNavigationSectionSpeedLimit = MpilotNavigationSectionSpeedLimit.newBuilder();
            mpilotNavigationSectionSpeedLimit.setIntervalCameraSpeedValue(l2NaviBean.getIntervalCameraData().getIntervalCameraSpeedValue());
            mpilotNavigationSectionSpeedLimit.setDistanceToIntervalCameraStartPoint(l2NaviBean.getIntervalCameraData().getIntervalCameraStartPointDist());
            mpilotNavigationSectionSpeedLimit.setDistanceToIntervalCameraEndPoint(l2NaviBean.getIntervalCameraData().getIntervalCameraEndPointDist());
            mpilotNavigationInformation.setMpilotNavigationSectionSpeedLimit(mpilotNavigationSectionSpeedLimit);

            // TODO 缺少字段
            MpilotNavigationIntersection.Builder mpilotNavigationIntersection = MpilotNavigationIntersection.newBuilder();
//            l2NaviBean.getCrossInfoData().getHasTrafficLight();
//            l2NaviBean.getCrossInfoData().getLaneNum();
//            l2NaviBean.getCrossInfoData().getTrafficLightPosition();
            mpilotNavigationIntersection.setSegmentIndex(l2NaviBean.getCrossInfoData().getSegmentIndex());
            mpilotNavigationIntersection.setLinkIndex(l2NaviBean.getCrossInfoData().getLinkIndex());
            mpilotNavigationIntersection.setTimestamp(l2NaviBean.getCrossInfoData().getTimestamp());
            mpilotNavigationIntersection.addAllRecommendLaneList(l2NaviBean.getCrossInfoData().getHighLightLanes());
            mpilotNavigationIntersection.addAllFrontLaneTypeList(l2NaviBean.getCrossInfoData().getFrontLaneType());
            mpilotNavigationIntersection.addAllBackLaneTypeList(l2NaviBean.getCrossInfoData().getBackLaneType());
            mpilotNavigationIntersection.addAllFrontLaneActionList(l2NaviBean.getCrossInfoData().getHighLightLaneTypes());
            mpilotNavigationIntersection.addAllBackLaneActionList(l2NaviBean.getCrossInfoData().getLaneTypes());
            mpilotNavigationInformation.setMpilotNavigationIntersection(mpilotNavigationIntersection);

            MpilotNavigationTunnel.Builder mpilotNavigationTunnel = MpilotNavigationTunnel.newBuilder();
            mpilotNavigationTunnel.setTunnelLength(l2NaviBean.getTunnelInfo().getTunnelLength());
            mpilotNavigationTunnel.setToTunnelDistance(l2NaviBean.getTunnelInfo().getTunnelDist());
            mpilotNavigationInformation.setMpilotNavigationTunnel(mpilotNavigationTunnel);

            List<L2NaviBean.MixForksBean> mixForks = l2NaviBean.getMixForks();
            for (int i = 0; i < mixForks.size(); i++) {
                L2NaviBean.MixForksBean mixForksBean = mixForks.get(i);
                MpilotNavigationMixFork.Builder mpilotNavigationMixFork = MpilotNavigationMixFork.newBuilder();
                mpilotNavigationMixFork.setDistance(mixForksBean.getDistance());
                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(mixForksBean.getPosition().getX());
                geoPointCoordinates.setLatitudeY(mixForksBean.getPosition().getY());
                mpilotNavigationMixFork.setPosition(geoPointCoordinates);
                mpilotNavigationMixFork.setRoadClass(mixForksBean.getRoadClass());
                mpilotNavigationMixFork.setSegmentIndex(mixForksBean.getSegmentIndex());
                mpilotNavigationInformation.addMpilotNavigationMixForkList(mpilotNavigationMixFork);
            }

            // TODO 缺少字段
            List<L2NaviBean.AheadIntersectionsBean> aheadIntersections = l2NaviBean.getAheadIntersections();
            for (int i = 0; i < aheadIntersections.size(); i++) {
                L2NaviBean.AheadIntersectionsBean aheadIntersectionsBean = aheadIntersections.get(i);
                MpilotNavigationIntersection.Builder mpilotNavigationIntersection1 = MpilotNavigationIntersection.newBuilder();
//                aheadIntersectionsBean.getLaneNum()
                mpilotNavigationIntersection1.setSegmentIndex(aheadIntersectionsBean.getSegmentIndex());
                mpilotNavigationIntersection1.setLinkIndex(aheadIntersectionsBean.getLinkIndex());
                mpilotNavigationIntersection1.setTimestamp(aheadIntersectionsBean.getTimestamp());
                mpilotNavigationIntersection1.addAllFrontLaneTypeList(aheadIntersectionsBean.getFrontLaneType());
                mpilotNavigationIntersection1.addAllBackLaneTypeList(aheadIntersectionsBean.getBackLaneType());
                mpilotNavigationIntersection1.addAllFrontLaneActionList(aheadIntersectionsBean.getHighLightLaneTypes());
                mpilotNavigationIntersection1.addAllBackLaneActionList(aheadIntersectionsBean.getLaneTypes());
                mpilotNavigationInformation.addMpilotNavigationIntersectionList(mpilotNavigationIntersection1);
            }

            mpilotNavigationInformation.setMpilotNavigationTideLane(l2NaviBean.getHasTidalLane());
            mpilotNavigationInformation.setMpilotNavigationHasServiceStationRemind(l2NaviBean.getIsServiceAreaRoad() == 1);
            mpilotNavigationInformation.setMpilotNavigationRampRemind(l2NaviBean.getRampDist());
            mpilotNavigationInformation.setMpilotNavigationTollStationDistance(l2NaviBean.getTollStationDist());

            publish(TBT_URI, Any.pack(mpilotNavigationInformation.build()));
        }
    };

    /**
     * 算路观察者
     */
    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        /**
         * 路线上充电站数据回调    、
         * @param routeL2Data 路线信息
         */
        @Override
        public void onL2DataCallBack(final RouteL2Data routeL2Data) {
            if (routeL2Data == null) {
                Logger.w(TAG, "onL2DataCallBack: routeL2Data null");
                return;
            }
            Logger.d(TAG, "send route data: ");
//            JsonLogger.print("send route data", json);
            String json = GsonUtils.toJson(routeL2Data);
            if (Logger.isDebugLevel()) JsonLog.saveJsonToCache(json, "l2.json", "l2_route");
            MpilotSDRouteList.Builder mpilotSDRouteList = MpilotSDRouteList.newBuilder();
            MpilotSDRoute.Builder mpilotSdRoute = MpilotSDRoute.newBuilder();
            mpilotSdRoute.setSdkVersion(routeL2Data.getMSdkVersion());
            mpilotSdRoute.setEngineVersion(routeL2Data.getMEngineVersion());
            mpilotSdRoute.setLinkCount(routeL2Data.getMLinkCnt());
            mpilotSdRoute.setPathId(routeL2Data.getMPathID());
            mpilotSdRoute.setPointCount(routeL2Data.getMPntCnt());

            MpilotSDRouteEndPoi.Builder mpilotSdRouteEndPoi = MpilotSDRouteEndPoi.newBuilder();
            String endpoiid = routeL2Data.getMEndPoi().getMId();
            if (endpoiid != null) {
                mpilotSdRouteEndPoi.setEndPoiId(endpoiid);
            }
            String mName = routeL2Data.getMEndPoi().getMName();
            if (mName != null) {
                mpilotSdRouteEndPoi.setEndPoiName(mName);
            }
            mpilotSdRouteEndPoi.setEndPoiType(routeL2Data.getMEndPoi().getMType());
            List<RouteL2Data.EndPoiDTO.EntranceListDTO> entranceList = routeL2Data.getMEndPoi().getMEntranceList();
            if (entranceList != null) {
                for (int i = 0; i < entranceList.size(); i++) {
                    RouteL2Data.EndPoiDTO.EntranceListDTO entranceListDTO = entranceList.get(i);
                    GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                    geoPointCoordinates.setLongitudeX(entranceListDTO.getMX());
                    geoPointCoordinates.setLatitudeY(entranceListDTO.getMY());
                    mpilotSdRouteEndPoi.addParkingLotEntranceList(geoPointCoordinates);
                }
            }
            List<RouteL2Data.EndPoiDTO.ExitListDTO> exitList = routeL2Data.getMEndPoi().getMExitList();
            if (exitList != null) {
                for (int i = 0; i < exitList.size(); i++) {
                    RouteL2Data.EndPoiDTO.ExitListDTO exitListDTO = exitList.get(i);
                    GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                    geoPointCoordinates.setLongitudeX(exitListDTO.getMX());
                    geoPointCoordinates.setLatitudeY(exitListDTO.getMY());
                    mpilotSdRouteEndPoi.addParkingLotExitList(geoPointCoordinates);
                }
            }
            List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO> parkingInfoList = routeL2Data.getMEndPoi().getMParkingInfoList();
            if (parkingInfoList != null) {
                for (int i = 0; i < parkingInfoList.size(); i++) {
                    RouteL2Data.EndPoiDTO.ParkingInfoListDTO parkingInfoListDTO = parkingInfoList.get(i);
                    ParkingLotInfoList.Builder parkingLotInfoList = ParkingLotInfoList.newBuilder();
                    parkingLotInfoList.setParkingLotId(parkingInfoListDTO.getMId());
                    parkingLotInfoList.setParkingLotName(parkingInfoListDTO.getMId());
                    parkingLotInfoList.setParkingLotType(parkingInfoListDTO.getMType());
                    List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.EntranceListDTO> mEntranceList = parkingInfoListDTO.getMEntranceList();
                    if (mEntranceList != null) {
                        mEntranceList.forEach(
                                entranceListDTO -> {
                                    GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                                    geoPointCoordinates.setLongitudeX(entranceListDTO.getMX());
                                    geoPointCoordinates.setLatitudeY(entranceListDTO.getMY());
                                    parkingLotInfoList.addParkingLotEntrancePositionList(geoPointCoordinates);
                                }
                        );
                    }
                    List<RouteL2Data.EndPoiDTO.ParkingInfoListDTO.ExitListDTO> mExitList = parkingInfoListDTO.getMExitList();
                    if (mExitList != null) {
                        mExitList.forEach(
                                entranceListDTO -> {
                                    GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                                    geoPointCoordinates.setLongitudeX(entranceListDTO.getMX());
                                    geoPointCoordinates.setLatitudeY(entranceListDTO.getMY());
                                    parkingLotInfoList.addParkingLotExitPositionList(geoPointCoordinates);
                                }
                        );
                    }
                    mpilotSdRouteEndPoi.addParkingLotInfoList(parkingLotInfoList);
                }
            }
            mpilotSdRoute.setMpilotSdRouteEndPoi(mpilotSdRouteEndPoi);

            routeL2Data.getMGuideGroups().forEach(guideGroup -> {
                MpilotSDRouteGuideGroups.Builder mpilotSdRouteGuideGroups = MpilotSDRouteGuideGroups.newBuilder();
                mpilotSdRouteGuideGroups.setGroupIconType(guideGroup.getMGroupIconType());
                mpilotSdRouteGuideGroups.setGroupLength(guideGroup.getMGroupLen());
                mpilotSdRouteGuideGroups.setGroupName(guideGroup.getMGroupName());
                mpilotSdRouteGuideGroups.setGroupTime(guideGroup.getMGroupTime());
                mpilotSdRouteGuideGroups.setGroupTrafficLightsCount(guideGroup.getMGroupTrafficLightsCount());

                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(guideGroup.getMGroupEnterCoord().getMMap().getMX());
                geoPointCoordinates.setLatitudeY(guideGroup.getMGroupEnterCoord().getMMap().getMY());
                mpilotSdRouteGuideGroups.setGroupEnterCoordinates(geoPointCoordinates);
                List<RouteL2Data.GuideGroupsDTO.SegmentsDTO> mSegments = guideGroup.getMSegments();
                if (mSegments != null) {
                    mSegments.forEach(segment -> {
                        MpilotSDRouteSegments.Builder mpilotSdRouteSegments = MpilotSDRouteSegments.newBuilder();
                        mpilotSdRouteSegments.setCurrentSegmentLinkCount(segment.getMCrntSegmLinkCnt());
                        mpilotSdRouteSegments.setDescription(segment.getMDescription());
                        mpilotSdRouteSegments.setIsArriveWayPoint(segment.getMIsArriveWayPoint());
                        mpilotSdRouteSegments.setLinkBegIndex(segment.getMLinkBegIdx());
                        mpilotSdRouteSegments.setNavigationAssistAction(segment.getMNavigationAssistAction());
                        mpilotSdRouteSegments.setNavigationLength(segment.getMNavigationLen());
                        mpilotSdRouteSegments.setNavigationMainAction(segment.getMNavigationMainAction());
                        mpilotSdRouteSegments.setNavigationNextRoadName(segment.getMNavigationNextRoadName());
                        mpilotSdRouteSegments.setTrafficLightNumber(segment.getMTrafficLightNum());
                        mpilotSdRouteSegments.setTravelTime(segment.getMTravelTime());
                        mpilotSdRouteGuideGroups.addMpilotSdRouteSegments(mpilotSdRouteSegments);
                    });
                }

                mpilotSdRoute.addMpilotSdRouteGuideGroups(mpilotSdRouteGuideGroups);
            });

            routeL2Data.getMLinks().forEach(link -> {
                MpilotSDRouteLinks.Builder mpilotSdRouteLinks = MpilotSDRouteLinks.newBuilder();
                mpilotSdRouteLinks.setFormway(link.getMFormway());
                mpilotSdRouteLinks.setIsToll(link.getMIsToll());
                mpilotSdRouteLinks.setLength(link.getMLen());
                mpilotSdRouteLinks.setLinkId(link.getMLinkID());
                mpilotSdRouteLinks.setLinkType(link.getMLinktype());
                mpilotSdRouteLinks.setPointBegIndex(link.getMPntBegIdx());
                mpilotSdRouteLinks.setPointCount(link.getMPntCnt());
                mpilotSdRouteLinks.setRoadClass(link.getMRoadclass());
                mpilotSdRouteLinks.setRoadName(link.getMRoadname());
                Integer mUrid = link.getMUrid();
                if (mUrid != null) {
                    mpilotSdRouteLinks.setUrid(mUrid);
                }
                mpilotSdRouteLinks.setAdminCode(String.valueOf(link.getMAdminCode()));//
                mpilotSdRouteLinks.setHasMixFork(link.getMHasMixFork());
                mpilotSdRouteLinks.setHasTrafficLight(link.getMHasTrafficLight());
                mpilotSdRouteLinks.setHasMultiOut(link.getMHasMultiOut());
                mpilotSdRouteLinks.setMainAction(link.getMMainAction());
                mpilotSdRouteLinks.setHasParallel(link.getMHasParallel());
                mpilotSdRouteLinks.setDirection(link.getMDirection());
                mpilotSdRouteLinks.setLaneNum(link.getMLaneNum());
                mpilotSdRouteLinks.setSpeedLimit(link.getMSpeedLimit());
                mpilotSdRouteLinks.setRoadOwnership(link.getMRoadOwnerShip());
                mpilotSdRoute.addMpilotSdRouteLinks(mpilotSdRouteLinks);
            });

            routeL2Data.getMPnts().forEach(pnt -> {
                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(pnt.getMX());
                geoPointCoordinates.setLatitudeY(pnt.getMY());
                mpilotSdRoute.addRoutePointsCoordinates(geoPointCoordinates);
            });

            routeL2Data.getMRestTollGateInfos().forEach(restTollGateInfo -> {
                // TODO json中是list
                MpilotSDRouteRestTollGateInfos.Builder mpilotSdRouteRestTollGateInfos = MpilotSDRouteRestTollGateInfos.newBuilder();
                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(restTollGateInfo.getMPos().getMX());
                geoPointCoordinates.setLatitudeY(restTollGateInfo.getMPos().getMY());
                mpilotSdRouteRestTollGateInfos.setTollGatePosition(geoPointCoordinates);
                mpilotSdRouteRestTollGateInfos.setRemainDistance(restTollGateInfo.getMRemainDist());
                mpilotSdRouteRestTollGateInfos.setRemainTime(restTollGateInfo.getMRemainTime());
                mpilotSdRouteRestTollGateInfos.setTollGateName(restTollGateInfo.getMTollGateName());
                mpilotSdRoute.setMpilotSdRouteRestTollGateInfos(mpilotSdRouteRestTollGateInfos);
            });

            routeL2Data.getMTrafficLights().forEach(trafficLight -> {
                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(trafficLight.getMX());
                geoPointCoordinates.setLatitudeY(trafficLight.getMY());
                mpilotSdRoute.addTrafficLightsCoordinates(geoPointCoordinates);
            });

            routeL2Data.getMViaRoads().forEach(viaRoad -> {
                MpilotSDRouteViaRoad.Builder mpilotSdRouteViaRoad = MpilotSDRouteViaRoad.newBuilder();
                mpilotSdRouteViaRoad.setRoadName(viaRoad.getMRoadName());
                mpilotSdRouteViaRoad.setMinLaneNumber(viaRoad.getMMinLaneNum());
                mpilotSdRouteViaRoad.setMaxLaneNumber(viaRoad.getMMaxLaneNum());
                mpilotSdRouteViaRoad.setMinSpeedLimit(viaRoad.getMMinSpeedLimit());
                mpilotSdRouteViaRoad.setMaxSpeedLimit(viaRoad.getMMaxSpeedLimit());
                mpilotSdRouteViaRoad.setLength(viaRoad.getMLength());
                mpilotSdRouteViaRoad.setRoadClass(viaRoad.getMRoadClass());
                GeoPointCoordinates.Builder geoPointCoordinates = GeoPointCoordinates.newBuilder();
                geoPointCoordinates.setLongitudeX(viaRoad.getMCoordinate().getMX());
                geoPointCoordinates.setLatitudeY(viaRoad.getMCoordinate().getMY());
                mpilotSdRouteViaRoad.setViaRoadCoordinates(geoPointCoordinates);
                mpilotSdRoute.addMpilotSdRouteViaRoad(mpilotSdRouteViaRoad);
            });

            mpilotSDRouteList.addMpilotSdRoute(mpilotSdRoute);
            publish(ROUTE_URI, Any.pack(mpilotSDRouteList.build()));
        }
    };

    private void publish(String uri, Any protoPayload) {
        CloudEvent cloudEvent = CloudEventFactory.publish(uri, protoPayload, CloudEventAttributes.empty());
        mClsLink.publish(cloudEvent);
    }
    //endregion

    //region TTS播报功能
    private final SignalCallback mSignalCallback = new SignalCallback() {
        @Override
        public void onNaviOnADASStateChanged(int state) {
            Logger.d(TAG, "signal callback state= " , state);
            Context context = AppCache.getInstance().getMContext();
            switch (state) {
                case SignalConst.L2_NOP.CLOSE_TO_NOA_AREA_TRUE:
                    sendTTS(context.getString(R.string.close_to_noa_area_true));
                    break;
                case SignalConst.L2_NOP.STATUS_ACTIVE_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.status_active_indication_on_true));
                    break;
                case SignalConst.L2_NOP.STATUS_NORMAL_TO_OVERRIDE_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.status_normal_to_override_indication_on_true));
                    break;
                case SignalConst.L2_NOP.STATUS_OVERRIDE_TO_NORMAL_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.status_override_to_normal_indication_on_true));
                    break;
                case SignalConst.L2_NOP.CLOSE_TO_TIGHT_CURVE_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.close_to_tight_curve_indication_on_true));
                    break;
                case SignalConst.L2_NOP.INTO_TIGHT_CURVE_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.into_tight_curve_indication_on_true));
                    break;
                case SignalConst.L2_NOP.TAKE_STEERING_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.take_steering_indication_on_true));
                    break;
                case SignalConst.L2_NOP.MERGE_INTO_MAIN_ROAD_TRUE:
                    sendTTS(context.getString(R.string.merge_into_main_road_true));
                    break;
                case SignalConst.L2_NOP.LANE_CHANGING_TO_FOLLOW_ROUTE_LEFT:
                    sendTTS(context.getString(R.string.lane_changing_to_follow_route_left));
                    break;
                case SignalConst.L2_NOP.LANE_CHANGING_TO_FOLLOW_ROUTE_RIGHT:
                    sendTTS(context.getString(R.string.lane_changing_to_follow_route_right));
                    break;
                case SignalConst.L2_NOP.TEXT_TO_SPEECH_LANE_CHANGE_ABORT_TRUE:
                    sendTTS(context.getString(R.string.text_to_speech_lane_change_abort_true));
                    break;
                case SignalConst.L2_NOP.DISTANCE_TO_RAMP_2000M_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.distance_to_ramp_2000m_indication_on_true));
                    break;
                case SignalConst.L2_NOP.DISTANCE_TO_RAMP_500M_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.distance_to_ramp_500m_indication_on_true));
                    break;
                case SignalConst.L2_NOP.COMPLICATED_ROAD_CONDITION_LANE_CHANGE_FAILED_TRUE:
                    sendTTS(context.getString(R.string.complicated_road_condition_lane_change_failed_true), true);
                    break;
                case SignalConst.L2_NOP.CHANGING_TO_FAST_LANE_LEFT:
                    sendTTS(context.getString(R.string.changing_to_fast_lane_left));
                    break;
                case SignalConst.L2_NOP.CONFIRM_CHANGE_TO_FAST_LANE_LEFT:
                    sendTTS(context.getString(R.string.confirm_change_to_fast_lane_left));
                    break;
                case SignalConst.L2_NOP.CHANGING_TO_FAST_LANE_RIGHT:
                    sendTTS(context.getString(R.string.changing_to_fast_lane_right));
                    break;
                case SignalConst.L2_NOP.CONFIRM_CHANGE_TO_FAST_LANE_RIGHT:
                    sendTTS(context.getString(R.string.confirm_change_to_fast_lane_right));
                    break;
                case SignalConst.L2_NOP.EXIT_RAMP_TO_NON_LIMITED_ACCESS_ROAD_TRUE:
                    sendTTS(context.getString(R.string.exit_ramp_to_non_limited_access_road_true));
                    break;
                case SignalConst.L2_NOP.DISTANCE_TO_END_500M_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.distance_to_end_500m_indication_on_true));
                    break;
                case SignalConst.L2_NOP.FINISHED_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.finished_indication_on_true), true);
                    break;
                case SignalConst.L2_NOP.TAKE_VEHICLE_CONTROL_INDICATION_ON_TRUE:
                    sendTTS(context.getString(R.string.take_vehicle_control_indication_on_true));
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_CONSTRUCTION:
                    sendTTS(context.getString(R.string.deactivation_reason_construction), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_MAP_UNAVAILABLE:
                    sendTTS(context.getString(R.string.deactivation_reason_map_unavailable), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_GPS_UNAVAILABLE:
                    sendTTS(context.getString(R.string.deactivation_reason_gps_unavailable), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_TRAFFIC_JAM:
                    sendTTS(context.getString(R.string.deactivation_reason_traffic_jam), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_TIGHTCURVE:
                    sendTTS(context.getString(R.string.deactivation_reason_tightcurve), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_SPEEDOUTLIMIT:
                    sendTTS(context.getString(R.string.deactivation_reason_speedoutlimit), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_COMPLICATED_ROAD_CONDITION:
                    sendTTS(context.getString(R.string.deactivation_reason_complicated_road_condition), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_UNAVAILABLE:
                    sendTTS(context.getString(R.string.deactivation_reason_unavailable), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_TUNNEL:
                    sendTTS(context.getString(R.string.deactivation_reason_tunnel), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_SERVICE_NAVIGATION_ON_ADAS_SYSTEM:
                    sendTTS(context.getString(R.string.deactivation_reason_service_navigation_on_adas_system), true);
                    break;
                case SignalConst.L2_NOP.DEACTIVATION_REASON_DRIVER_ACTION:
                    sendTTS(context.getString(R.string.deactivation_reason_driver_action), true);
                    break;
                default:
                    Logger.w("not find state: " + state);
            }
        }
    };

    private void sendTTS(final String tts) {
        sendTTS(tts, false);
    }

    private void sendTTS(final String tts, final boolean highPriority) {
        if (Objects.equals(mTtsStr, tts)) {
            return;
        }
        mTtsStr = tts;
        L2NopTts.sendTTS(tts, highPriority);
        if (mScheduledFuture != null) {
            mScheduledFuture.cancel(true);
        }
        mScheduledFuture = ThreadManager.getInstance().asyncDelayWithResult(() -> mTtsStr = null, 5);
    }
    //endregion
}
