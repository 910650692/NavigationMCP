package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.path.model.LightBarItem;
import com.autonavi.gbl.common.path.model.RoadClass;
import com.autonavi.gbl.common.path.model.TollGateInfo;
import com.autonavi.gbl.common.path.model.TrafficItem;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.CrossNaviInfo;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.CruiseInfo;
import com.autonavi.gbl.guide.model.DriveReport;
import com.autonavi.gbl.guide.model.ExitDirectionInfo;
import com.autonavi.gbl.guide.model.LaneInfo;
import com.autonavi.gbl.guide.model.LightBarDetail;
import com.autonavi.gbl.guide.model.LightBarInfo;
import com.autonavi.gbl.guide.model.ManeuverIconResponseData;
import com.autonavi.gbl.guide.model.ManeuverInfo;
import com.autonavi.gbl.guide.model.MixForkInfo;
import com.autonavi.gbl.guide.model.NaviCameraExt;
import com.autonavi.gbl.guide.model.NaviFacility;
import com.autonavi.gbl.guide.model.NaviFacilityType;
import com.autonavi.gbl.guide.model.NaviGreenWaveCarSpeed;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.guide.model.NaviInfoPanel;
import com.autonavi.gbl.guide.model.NaviIntervalCameraDynamicInfo;
import com.autonavi.gbl.guide.model.NaviRoadFacility;
import com.autonavi.gbl.guide.model.NaviStatisticsInfo;
import com.autonavi.gbl.guide.model.NaviSubCameraExt;
import com.autonavi.gbl.guide.model.SAPAInquireResponseData;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.model.TimeAndDist;
import com.autonavi.gbl.guide.model.TmcInfoData;
import com.autonavi.gbl.guide.model.VectorCrossImageType;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.NaviDriveReportEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviInfoEntity;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
import com.fy.navi.service.define.navi.NaviRoadFacilityEntity;
import com.fy.navi.service.define.navi.NaviViaEntity;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.ParkingInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.utils.NumberUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

// TODO: 2024/12/30 数据需要根据业务精简一下
public final class NaviDataFormatHelper {

    public static final String TAG = "NaviDataFormatHelper";

    private NaviDataFormatHelper() {

    }

    private static SapaInfoEntity mCurrentSapaInfoEntity;

    /**
     * @param info info
     * @return LaneInfoEntity
     */
    public static LaneInfoEntity forMatLaneInfo(final LaneInfo info) {
        final LaneInfoEntity laneInfoEntity = new LaneInfoEntity();
        laneInfoEntity.setBackLane(info.backLane);
        laneInfoEntity.setBackExtenLane(info.backExtenLane);
        laneInfoEntity.setPoint(new GeoPoint(info.point.lon, info.point.lat));
        laneInfoEntity.setFrontLane(info.frontLane);
        laneInfoEntity.setOptimalLane(info.optimalLane);
        laneInfoEntity.setExtensionLane(info.extensionLane);
        laneInfoEntity.setFrontLaneType(info.frontLaneType);
        laneInfoEntity.setBackLaneType(info.backLaneType);
        laneInfoEntity.setSegmentIdx(info.segmentIdx);
        laneInfoEntity.setLinkIdx(info.linkIdx);
        return laneInfoEntity;
    }

    /**
     * @param list list
     * @return CruiseInfoEntity
     */
    public static CruiseInfoEntity formatCruiseCameraExt(final ArrayList<NaviCameraExt> list) {
        final CruiseInfoEntity cruiseCameraList = new CruiseInfoEntity();
        if (list != null && !list.isEmpty()) {
            for (NaviCameraExt cameraInfo : list) {
                if (cameraInfo.subCameras == null || cameraInfo.subCameras.isEmpty()) {
                    continue;
                }
                final NaviSubCameraExt subCameraExt = cameraInfo.subCameras.get(0);
                if (subCameraExt.speed == null || subCameraExt.speed.isEmpty()) {
                    continue;
                }
                cruiseCameraList.setSpeed(subCameraExt.speed);
                cruiseCameraList.distance = cameraInfo.distance;
            }
        }
        return cruiseCameraList;
    }

    /**
     * @param info info
     * @return SoundInfoEntity
     */
    public static SoundInfoEntity formatSoundInfo(final SoundInfo info) {
        final SoundInfoEntity soundInfoEntity = new SoundInfoEntity();
        if (null != info) {
            soundInfoEntity.setText(info.text);
            soundInfoEntity.setSoundType(info.soundType);
        }
        return soundInfoEntity;
    }

    /**
     * @param type type
     * @return SoundInfoEntity
     */
    public static SoundInfoEntity formatSoundInfo(final int type) {
        final SoundInfoEntity soundInfoEntity = new SoundInfoEntity();
        soundInfoEntity.setRingType(type);
        soundInfoEntity.setRingType(true);
        return soundInfoEntity;
    }

    /**
     * @param cruiseInfo cruiseInfo
     * @return CruiseInfoEntity
     */
    public static CruiseInfoEntity formatCruiseInfo(final CruiseInfo cruiseInfo) {
        final CruiseInfoEntity cruiseInfoEntity = new CruiseInfoEntity();
        if (cruiseInfo != null) {
            cruiseInfoEntity.setRoadClass(cruiseInfo.roadClass);
            cruiseInfoEntity.setRoadName(cruiseInfo.roadName);
        }
        return cruiseInfoEntity;
    }


    /**
     * @param maneuverInfo maneuverInfo
     * @return NaviManeuverInfo
     */
    public static NaviManeuverInfo formatManeuverInfo(final ManeuverInfo maneuverInfo) {
        final NaviManeuverInfo naviManeuverInfo = new NaviManeuverInfo();
        naviManeuverInfo.setDateType(NaviConstant.ManeuverDataType.MANEUVER);
        if (maneuverInfo != null) {
            naviManeuverInfo.setType(maneuverInfo.type);
            naviManeuverInfo.setPathID(maneuverInfo.pathID);
            naviManeuverInfo.setSegmentIndex(maneuverInfo.segmentIndex);
            naviManeuverInfo.setManeuverID(maneuverInfo.maneuverID);
        }
        return naviManeuverInfo;
    }

    /**
     * @param responseData responseData
     * @return NaviManeuverInfo
     */
    public static NaviManeuverInfo formatManeuverIconData(
            final ManeuverIconResponseData responseData) {
        final NaviManeuverInfo naviManeuverIconResponseData = new NaviManeuverInfo();
        naviManeuverIconResponseData.setDateType(NaviConstant.ManeuverDataType.MANEUVER_ICON);
        if (responseData != null) {
            naviManeuverIconResponseData.setData(responseData.data);
            final NaviManeuverInfo.NaviManeuverConfig naviManeuverConfig = new NaviManeuverInfo.NaviManeuverConfig();
            naviManeuverConfig.setWidth(responseData.requestConfig.width);
            naviManeuverConfig.setHeight(responseData.requestConfig.height);
            naviManeuverConfig.setBackColor(responseData.requestConfig.backColor);
            naviManeuverConfig.setRoadColor(responseData.requestConfig.roadColor);
            naviManeuverConfig.setArrowColor(responseData.requestConfig.arrowColor);
            naviManeuverConfig.setPathID(responseData.requestConfig.pathID);
            naviManeuverConfig.setSegmentIdx(responseData.requestConfig.segmentIdx);
            naviManeuverConfig.setManeuverID(responseData.requestConfig.maneuverID);
            naviManeuverIconResponseData.setRequestConfig(naviManeuverConfig);
        }
        return naviManeuverIconResponseData;
    }

    /**
     * @param directionInfo directionInfo
     * @return NaviManeuverInfo
     */
    public static NaviManeuverInfo formatExitDirectionInfo(final ExitDirectionInfo directionInfo) {
        final NaviManeuverInfo naviExitDirectionInfo = new NaviManeuverInfo();
        naviExitDirectionInfo.setDateType(NaviConstant.ManeuverDataType.EXIT_DIRECTION);
        if (directionInfo != null) {
            naviExitDirectionInfo.setExitNameInfo(directionInfo.exitNameInfo);
            naviExitDirectionInfo.setDirectionInfo(directionInfo.directionInfo);
            naviExitDirectionInfo.setEntranceExit(directionInfo.entranceExit);
        }
        return naviExitDirectionInfo;
    }

    /**
     * @param tollGateInfo tollGateInfo
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity formatTollGateInfo(final TollGateInfo tollGateInfo) {
        final SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
        if (tollGateInfo != null) {
            if (mCurrentSapaInfoEntity != null) {
                mCurrentSapaInfoEntity.setLaneTypes(tollGateInfo.laneTypes);
                return mCurrentSapaInfoEntity;
            } else {
                sapaInfoEntity.setLaneTypes(tollGateInfo.laneTypes);
            }
        }
        return sapaInfoEntity;
    }

    /**
     * @param naviInfoList naviInfoList
     * @return NaviEtaInfo
     */
    public static NaviEtaInfo forMatNaviInfo(final ArrayList<NaviInfo> naviInfoList) {
        final NaviEtaInfo naviEtaInfo = new NaviEtaInfo();
        if (!ConvertUtils.isEmpty(naviInfoList)) {
            //取第一个路段的信息
            final NaviInfo naviInfo = naviInfoList.get(NumberUtils.NUM_0);
            naviEtaInfo.pathID = naviInfo.pathID;
            naviEtaInfo.setAllTime(naviInfo.routeRemain.time);
            naviEtaInfo.setAllDist(naviInfo.routeRemain.dist);
            naviEtaInfo.type = naviInfo.type;
            naviEtaInfo.setCurRouteName(naviInfo.curRouteName);
            naviEtaInfo.setRingOutCnt(naviInfo.ringOutCnt);
            naviEtaInfo.routeRemainLightCount = naviInfo.routeRemainLightCount;
            naviEtaInfo.linkRemainDist = naviInfo.linkRemainDist;
            naviEtaInfo.curSegIdx = naviInfo.curSegIdx;
            naviEtaInfo.curLinkIdx = naviInfo.curLinkIdx;
            naviEtaInfo.curPointIdx = naviInfo.curPointIdx;
            naviEtaInfo.curRoadClass = naviInfo.curRoadClass;
            naviEtaInfo.roundaboutOutAngle = naviInfo.roundaboutOutAngle;
            naviEtaInfo.driveTime = naviInfo.driveTime;
            naviEtaInfo.driveDist = naviInfo.driveDist;
            naviEtaInfo.cityCode = naviInfo.cityCode;
            naviEtaInfo.curLinkSpeed = naviInfo.curLinkSpeed;
            naviEtaInfo.segTipsDis = naviInfo.segTipsDis;
            naviEtaInfo.crossManeuverID = naviInfo.crossManeuverID;
            naviEtaInfo.NaviInfoFlag = naviInfo.NaviInfoFlag;
            naviEtaInfo.gateName = naviInfo.gateName;
            naviEtaInfo.aoiName = naviInfo.aoiName;
            naviEtaInfo.innerRoad = naviInfo.innerRoad;
            if (naviInfo.notAvoidInfo != null) {
                final NaviEtaInfo.NotAvoidInfo notAvoidInfo = new NaviEtaInfo.NotAvoidInfo();
                notAvoidInfo.type = naviInfo.notAvoidInfo.type;
                notAvoidInfo.distToCar = naviInfo.notAvoidInfo.distToCar;
                notAvoidInfo.forbidType = naviInfo.notAvoidInfo.forbidType;
                notAvoidInfo.valid = naviInfo.notAvoidInfo.valid;
                notAvoidInfo.coord2D = new GeoPoint(naviInfo.notAvoidInfo.coord2D.lon, naviInfo.notAvoidInfo.coord2D.lat, 0);
                notAvoidInfo.coord3D = new GeoPoint(naviInfo.notAvoidInfo.coord3D.lon,
                        naviInfo.notAvoidInfo.coord3D.lat, naviInfo.notAvoidInfo.coord3D.z);
                naviEtaInfo.notAvoidInfo = notAvoidInfo;
            }
            if (!ConvertUtils.isEmpty(naviInfo.viaRemain)) {
                final ArrayList<NaviEtaInfo.NaviTimeAndDist> naviTimeAndDists = new ArrayList<>();
                for (TimeAndDist timeAndDist : naviInfo.viaRemain) {
                    naviTimeAndDists.add(new NaviEtaInfo.NaviTimeAndDist(timeAndDist.time, timeAndDist.dist));
                }
                naviEtaInfo.viaRemain = naviTimeAndDists;
            }
            if (!ConvertUtils.isEmpty(naviInfo.ChargeStationRemain)) {
                final ArrayList<NaviEtaInfo.NaviTimeAndDist> naviTimeAndDists = new ArrayList<>();
                for (TimeAndDist timeAndDist : naviInfo.ChargeStationRemain) {
                    naviTimeAndDists.add(new NaviEtaInfo.NaviTimeAndDist(timeAndDist.time, timeAndDist.dist));
                }
                naviEtaInfo.ChargeStationRemain = naviTimeAndDists;
            }
            if (!ConvertUtils.isEmpty(naviInfo.nextCrossInfo)) {
                final ArrayList<NaviEtaInfo.NaviCrossNaviInfo> list = new ArrayList<>();
                for (CrossNaviInfo crossNaviInfo : naviInfo.nextCrossInfo) {
                    final NaviEtaInfo.NaviCrossNaviInfo bean = new NaviEtaInfo.NaviCrossNaviInfo();
                    bean.pathID = crossNaviInfo.pathID;
                    bean.segIdx = crossNaviInfo.segIdx;
                    bean.mainAction = crossNaviInfo.mainAction;
                    bean.assistAction = crossNaviInfo.assistAction;
                    bean.maneuverID = crossNaviInfo.maneuverID;
                    bean.crossManeuverID = crossNaviInfo.crossManeuverID;
                    bean.nextRoadName = crossNaviInfo.nextRoadName;
                    bean.curToSegmentDist = crossNaviInfo.curToSegmentDist;
                    bean.curToSegmentTime = crossNaviInfo.curToSegmentTime;
                    bean.outCnt = crossNaviInfo.outCnt;
                    bean.viaNum = crossNaviInfo.viaNum;
                    bean.destDirection = crossNaviInfo.destDirection;
                    bean.tunnelFlag = crossNaviInfo.tunnelFlag;
                    bean.reversed = crossNaviInfo.reversed;
                    bean.rev = crossNaviInfo.rev;
                    bean.segmentIndex = crossNaviInfo.maneuverInfo.segmentIndex;
                    bean.linkIndex = crossNaviInfo.maneuverInfo.linkIndex;
                    list.add(bean);
                }
                naviEtaInfo.nextCrossInfo = list;
            }
            if (!ConvertUtils.isEmpty(naviInfo.NaviInfoData)) {
                final ArrayList<NaviEtaInfo.NaviInfoPanel> list = new ArrayList<>();
                for (NaviInfoPanel naviInfoPanel : naviInfo.NaviInfoData) {
                    final NaviEtaInfo.NaviInfoPanel bean = new NaviEtaInfo.NaviInfoPanel();
                    bean.maneuverID = naviInfoPanel.maneuverID;
                    bean.nextRouteName = naviInfoPanel.nextRouteName;
                    bean.nextRoadNameSegIdx = naviInfoPanel.nextRoadNameSegIdx;
                    bean.nextRoadNameLinkIdx = naviInfoPanel.nextRoadNameLinkIdx;
                    bean.split = naviInfoPanel.split;
                    bean.segmentRemain = new NaviEtaInfo.NaviTimeAndDist(naviInfoPanel.segmentRemain.time, naviInfoPanel.segmentRemain.dist);
                    list.add(bean);
                }
                naviEtaInfo.NaviInfoData = list;
            }
            final NaviInfoPanel naviInfoPanel = naviInfo.NaviInfoData.get(naviInfo.NaviInfoFlag);
            naviEtaInfo.setCurManeuverID(naviInfoPanel.maneuverID);
            naviEtaInfo.setNextRouteName(naviInfoPanel.nextRouteName);
            if (!ConvertUtils.isEmpty(naviInfo.nextCrossInfo)) {
                final CrossNaviInfo nextCrossInfo = naviInfo.nextCrossInfo.get(NumberUtils.NUM_0);
                naviEtaInfo.setNextManeuverID(nextCrossInfo.maneuverID);
                naviEtaInfo.setNextDist(nextCrossInfo.curToSegmentDist);
            }
        }
        return naviEtaInfo;
    }

    public static ArrayList<NaviInfoEntity> forMatNaviInfoEntity(
            final ArrayList<NaviInfo> naviInfoList) {
        ArrayList<NaviInfoEntity> naviInfoEntities = new ArrayList<>();
        if (!ConvertUtils.isEmpty(naviInfoList)) {
            for (NaviInfo naviInfo : naviInfoList) {
                NaviInfoEntity naviInfoEntity = new NaviInfoEntity();
                naviInfoEntity.setPathId(naviInfo.pathID);
                naviInfoEntity.setCurSegIdx(naviInfo.curSegIdx);
                naviInfoEntity.setRemainTime(naviInfo.routeRemain.time);
                naviInfoEntity.setRouteRemainLightCount(naviInfo.routeRemainLightCount);
                naviInfoEntities.add(naviInfoEntity);
            }
        }
        return naviInfoEntities;
    }

    /**
     * @param lightBarInfo   lightBarInfo
     * @param lightBarDetail lightBarDetail
     * @return NaviTmcInfo
     */
    public static NaviTmcInfo forMatTMCLightBar(final ArrayList<LightBarInfo> lightBarInfo,
                                                final LightBarDetail lightBarDetail) {
        final NaviTmcInfo naviTmcInfo = new NaviTmcInfo();
        final NaviTmcInfo.NaviLightBarDetail naviLightBarDetail = new NaviTmcInfo.NaviLightBarDetail();
        naviLightBarDetail.setPathID(lightBarDetail.pathID);
        naviLightBarDetail.setTotalDistance(lightBarDetail.totalDistance);
        naviLightBarDetail.setRestDistance(lightBarDetail.restDistance);
        naviLightBarDetail.setFinishDistance(lightBarDetail.finishDistance);
        final ArrayList<NaviTmcInfo.NaviTmcInfoData> tmcInfoList = new ArrayList<>();
        for (TmcInfoData tmcInfoData : lightBarDetail.tmcInfoData) {
            final NaviTmcInfo.NaviTmcInfoData naviTmcInfoData = new NaviTmcInfo.NaviTmcInfoData();
            naviTmcInfoData.setNumber(tmcInfoData.number);
            naviTmcInfoData.setStatus(tmcInfoData.status);
            naviTmcInfoData.setDistance(tmcInfoData.distance);
            naviTmcInfoData.setPercent(tmcInfoData.percent);
            naviTmcInfoData.setTravelTime(tmcInfoData.travelTime);
            tmcInfoList.add(naviTmcInfoData);
        }
        naviLightBarDetail.setTmcInfoData(tmcInfoList);
        naviTmcInfo.setLightBarDetail(naviLightBarDetail);
        final ArrayList<NaviTmcInfo.NaviLightBarInfo> list = new ArrayList<>();
        for (LightBarInfo info : lightBarInfo) {
            final NaviTmcInfo.NaviLightBarInfo barInfo = new NaviTmcInfo.NaviLightBarInfo();
            barInfo.setPathID(info.pathID);
            final ArrayList<NaviTmcInfo.NaviLightBarItem> items = new ArrayList<>();
            for (LightBarItem lightBarItem : info.itemList) {
                final NaviTmcInfo.NaviLightBarItem item = new NaviTmcInfo.NaviLightBarItem();
                item.setStatusFlag(lightBarItem.statusFlag);
                item.setStatus(lightBarItem.status);
                item.setFineStatus(lightBarItem.fineStatus);
                item.setLength(lightBarItem.length);
                item.setTimeOfSeconds(lightBarItem.timeOfSeconds);
                item.setStartSegmentIdx(lightBarItem.startSegmentIdx);
                item.setStartLinkIdx(lightBarItem.startLinkIdx);
                item.setStartLinkStatus(lightBarItem.startLinkStatus);
                item.setStartLinkFineStatus(lightBarItem.startLinkFineStatus);
                item.setEndSegmentIdx(lightBarItem.endSegmentIdx);
                item.setEndLinkIndex(lightBarItem.endLinkIndex);
                item.setEndLinkStatus(lightBarItem.endLinkStatus);
                item.setEndLinkFineStatus(lightBarItem.endLinkFineStatus);
                item.setStartTrafficItem(formatTrafficItem(lightBarItem.startTrafficItem));
                item.setStart3dTrafficItem(formatTrafficItem(lightBarItem.start3dTrafficItem));
                item.setEndTrafficItem(formatTrafficItem(lightBarItem.endTrafficItem));
                item.setEnd3dTrafficItem(formatTrafficItem(lightBarItem.end3dTrafficItem));
                items.add(item);
            }
            barInfo.setItemList(items);
            list.add(barInfo);
        }
        naviTmcInfo.setLightBarInfo(list);
        return naviTmcInfo;
    }

    /**
     * @param trafficItem trafficItem
     * @return NaviTmcInfo.NaviTrafficItem
     */
    private static NaviTmcInfo.NaviTrafficItem formatTrafficItem(final TrafficItem trafficItem) {
        final NaviTmcInfo.NaviTrafficItem naviTrafficItem = new NaviTmcInfo.NaviTrafficItem();
        naviTrafficItem.setLength(trafficItem.length);
        naviTrafficItem.setTravelTime(trafficItem.travelTime);
        naviTrafficItem.setRatio(trafficItem.ratio);
        naviTrafficItem.setStartIndex(trafficItem.startIndex);
        naviTrafficItem.setEndIndex(trafficItem.endIndex);
        naviTrafficItem.setStatus(trafficItem.status);
        naviTrafficItem.setFineStatus(trafficItem.fineStatus);
        naviTrafficItem.setSpeed(trafficItem.speed);
        naviTrafficItem.setCredibility(trafficItem.credibility);
        naviTrafficItem.setReverse(trafficItem.reverse);
        naviTrafficItem.setStartPnt(new GeoPoint(trafficItem.startPnt.lon, trafficItem.startPnt.lat,
                trafficItem.startPnt.z));
        naviTrafficItem.setEndPnt(new GeoPoint(trafficItem.endPnt.lon, trafficItem.endPnt.lat,
                trafficItem.endPnt.z));
        naviTrafficItem.setEndPnt(new GeoPoint(trafficItem.endPnt.lon, trafficItem.endPnt.lat,
                trafficItem.endPnt.z));
        return naviTrafficItem;
    }

    /**
     * @param info info
     * @return CrossImageEntity
     */
    public static CrossImageEntity forMatImageInfo(final CrossImageInfo info) {
        if (info != null) {
            final CrossImageEntity naviImageInfo = new CrossImageEntity();
            naviImageInfo.setType(info.type);
            naviImageInfo.setVectorType(info.vectorType);
            naviImageInfo.setDataBuf(info.dataBuf);
            naviImageInfo.setArrowDataBuf(info.arrowDataBuf);
            naviImageInfo.setOnlyVector(info.isOnlyVector);
            naviImageInfo.setDistance(info.distance);
            naviImageInfo.setCrossImageID(info.crossImageID);
            return naviImageInfo;
        }
        return null;
    }

    /**
     * @param type type
     * @return CrossImageEntity
     */
    public static CrossImageEntity forMatImageInfo(final int type) {
        final CrossImageEntity naviImageInfo = new CrossImageEntity();
        naviImageInfo.setType(type);
        return naviImageInfo;
    }

    /**
     * @param list list
     * @return SpeedOverallEntity
     */
    public static SpeedOverallEntity forMatNaviGreenWaveInfo(
            final ArrayList<NaviGreenWaveCarSpeed> list) {
        final SpeedOverallEntity speedOverallEntity = new SpeedOverallEntity();
        if (!ConvertUtils.isEmpty(list)) {
            final NaviGreenWaveCarSpeed naviGreenWaveCarSpeed = list.get(0);
            speedOverallEntity.setType(naviGreenWaveCarSpeed.type);
            speedOverallEntity.setMaxSpeed(naviGreenWaveCarSpeed.maxSpeed);
            speedOverallEntity.setMinSpeed(naviGreenWaveCarSpeed.minSpeed);
            speedOverallEntity.setLightCount(naviGreenWaveCarSpeed.lightCount);
        }
        speedOverallEntity.setSpeedType(NaviConstant.SpeedType.SPEED_GREEN_WAVE);
        return speedOverallEntity;
    }

    /**
     * @param cameraDynamicList cameraDynamicList
     * @return SpeedOverallEntity
     */
    public static SpeedOverallEntity forMatNaviSpeedCameraInfo(
            final ArrayList<NaviIntervalCameraDynamicInfo> cameraDynamicList) {
        final SpeedOverallEntity speedOverallEntity = new SpeedOverallEntity();
        if (!ConvertUtils.isEmpty(cameraDynamicList)) {
            final NaviIntervalCameraDynamicInfo info = cameraDynamicList.get(0);
            speedOverallEntity.setLimitSpeedList(info.speed);
            speedOverallEntity.setAverageSpeed(info.averageSpeed);
            speedOverallEntity.setRemainDistance(info.remainDistance);
            speedOverallEntity.setDistance(info.distance);
        }
        speedOverallEntity.setSpeedType(NaviConstant.SpeedType.SPEED_OVERALL);
        return speedOverallEntity;
    }

    /**
     * @param naviCameraList naviCameraList
     * @return CameraInfoEntity
     */
    public static CameraInfoEntity forMatNaviCameraInfo(
            final ArrayList<NaviCameraExt> naviCameraList) {
        //TODO 需要判断下此处还是否需要，该方法无法返回低道路等级限速摄像头信息，存在问题 - Niu
        final CameraInfoEntity cameraInfoEntity = new CameraInfoEntity();
        if (!ConvertUtils.isEmpty(naviCameraList)) {
            loop:
            for (NaviCameraExt cameraInfo : naviCameraList) {
                if (cameraInfo.subCameras == null || cameraInfo.subCameras.isEmpty()) {
                    continue;
                }
                if (((cameraInfo.roadClass == RoadClass.RoadClassFreeway || cameraInfo.roadClass == RoadClass.RoadClassCitySpeedway) &&
                        cameraInfo.distance <= 1000) || cameraInfo.distance <= 500) { // 高速路、城快 < 1KM 或者 普通道路 < 500M
                    final NaviSubCameraExt subCameraExt = cameraInfo.subCameras.get(0);
                    if (subCameraExt.speed == null || subCameraExt.speed.isEmpty()) {
                        continue;
                    }
                    for (Short speed : subCameraExt.speed) {
                        if (isValidSpeed(speed)) {
                            cameraInfoEntity.setCameraId(cameraInfo.cameraId);
                            cameraInfoEntity.setCoord2D(new GeoPoint(cameraInfo.coord2D.lon, cameraInfo.coord2D.lat));
                            cameraInfoEntity.setDistance(cameraInfo.distance);
                            cameraInfoEntity.setRoadClass(cameraInfo.roadClass);
                            cameraInfoEntity.setSubCameraId(subCameraExt.cameraId);
                            cameraInfoEntity.setSubType(subCameraExt.subType);
                            cameraInfoEntity.setMatch(subCameraExt.isMatch);
                            cameraInfoEntity.setSpeed(speed);
                            break loop;
                        }
                    }
                }
            }
        }
        return cameraInfoEntity;
    }

    /**
     * @param naviCameraList naviCameraList
     * @return CameraInfoEntity
     */
    public static CameraInfoEntity formatNearestCameraInfo(final ArrayList<NaviCameraExt> naviCameraList) {
        final CameraInfoEntity cameraInfoEntity = new CameraInfoEntity();
        if (!ConvertUtils.isEmpty(naviCameraList)) {
            for (NaviCameraExt cameraInfo : naviCameraList) {
                if (ConvertUtils.isEmpty(cameraInfo.subCameras)) {
                    continue;
                }
                for (NaviSubCameraExt subCamera : cameraInfo.subCameras) {
                    if (ConvertUtils.isEmpty(subCamera.speed)) {
                        continue;
                    }
                    for (Short speed : subCamera.speed) {
                        if (isValidSpeed(speed)) {
                            cameraInfoEntity.setCameraId(cameraInfo.cameraId);
                            cameraInfoEntity.setCoord2D(new GeoPoint(cameraInfo.coord2D.lon, cameraInfo.coord2D.lat));
                            cameraInfoEntity.setDistance(cameraInfo.distance);
                            cameraInfoEntity.setRoadClass(cameraInfo.roadClass);
                            cameraInfoEntity.setSubCameraId(subCamera.cameraId);
                            cameraInfoEntity.setSubType(subCamera.subType);
                            cameraInfoEntity.setMatch(subCamera.isMatch);
                            cameraInfoEntity.setSpeed(speed);
                            return cameraInfoEntity;
                        }
                    }
                }
            }
        }
        return cameraInfoEntity;
    }

    /**
     * @param speed speed
     * @return boolean
     */
    private static boolean isValidSpeed(final int speed) {
        return 0 < speed && speed < 0xff;
    }

    /**
     * @param responseData responseData
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity forMatSAPAInfo(final SAPAInquireResponseData responseData) {
        final ArrayList<NaviFacility> serviceAreaList = responseData.serviceAreaInfo.serviceAreaList;
        return forMatSAPAInfo(serviceAreaList);
//        SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
//        sapaInfoEntity.setRemainFreewayDistance(responseData.serviceAreaInfo.remainFreewayDistance);
//        sapaInfoEntity.setRemainFreewayTime(responseData.serviceAreaInfo.remainFreewayTime);
//        sapaInfoEntity.setRemainServiceAreaNum(responseData.serviceAreaInfo.remainServiceAreaNum);
//        if (!ConvertUtils.isEmpty(responseData.serviceAreaInfo.serviceAreaList)) {
//            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = new ArrayList<>();
//            for (NaviFacility naviFacility : responseData.serviceAreaInfo.serviceAreaList) {
//                sapaItems.add(getSAPAItem(naviFacility));
//            }
//            sapaInfoEntity.setList(sapaItems);
//        }
//        return sapaInfoEntity;
    }

    /**
     * @param list list
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity forMatSAPAInfo(final ArrayList<NaviFacility> list) {
        final SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
        if (!ConvertUtils.isEmpty(list)) {
            final int size = list.size();
            final ArrayList<SapaInfoEntity.SAPAItem> sapaItems = new ArrayList<>();
            final NaviFacility first = list.get(0);
            if (first.type == NaviFacilityType.NaviFacilityTypeServiceArea) {//服务区
                sapaInfoEntity.setType(NaviConstant.SapaItemsType.SPAS_LIST);
            } else if (first.type == NaviFacilityType.NaviFacilityTypeTollGate) {//收费站
                sapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_LIST);
            }
            sapaItems.add(getSAPAItem(first));
            if (size > 1) {
                final NaviFacility second = list.get(1);
                if (first.type == NaviFacilityType.NaviFacilityTypeServiceArea && second.type == NaviFacilityType.NaviFacilityTypeServiceArea) {
                    sapaInfoEntity.setType(NaviConstant.SapaItemsType.SPAS_LIST);//两个服务区
                    sapaItems.add(getSAPAItem(second));
                } else if ((first.type == NaviFacilityType.NaviFacilityTypeServiceArea && second.type == NaviFacilityType.NaviFacilityTypeTollGate)
                        || (first.type == NaviFacilityType.NaviFacilityTypeTollGate && second.type == NaviFacilityType.NaviFacilityTypeServiceArea)) {
                    sapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_AND_SPAS);//一个服务区一个收费站
                    sapaItems.add(getSAPAItem(second));
                } else if (first.type == NaviFacilityType.NaviFacilityTypeTollGate && second.type == NaviFacilityType.NaviFacilityTypeTollGate) {
                    sapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_LIST);//两个收费站
                    sapaItems.add(getSAPAItem(second));
                }
            }
            sapaInfoEntity.setList(sapaItems);
        } else {
            sapaInfoEntity.setType(NaviConstant.SapaItemsType.AUTO_UNKNOWN_ERROR);
        }
        mCurrentSapaInfoEntity = sapaInfoEntity;
        return sapaInfoEntity;
    }

    /**
     * @param naviFacility naviFacility
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity.SAPAItem getSAPAItem(final NaviFacility naviFacility) {
        final SapaInfoEntity.SAPAItem sapaItem = new SapaInfoEntity.SAPAItem();
        sapaItem.setRemainDist(naviFacility.remainDist);
        sapaItem.setType(naviFacility.type);
        sapaItem.setName(naviFacility.name);
        sapaItem.setPos(new GeoPoint(naviFacility.pos.lon, naviFacility.pos.lat));
        sapaItem.setSapaDetail(naviFacility.sapaDetail);
        sapaItem.setRemainTime(naviFacility.remainTime);
        sapaItem.setServicePOIID(naviFacility.servicePOIID);
        sapaItem.setBuildingStatus(naviFacility.buildingStatus);
        return sapaItem;
    }

    /**
     * @param crossInfo 路口信息
     * @return 图片信息
     */
    public static CrossImageInfo getCrossImageInfo(final CrossImageEntity crossInfo) {
        final CrossImageInfo crossImageInfo = new CrossImageInfo();
        switch (crossInfo.getType()) {
            case NaviConstant.CrossType.CROSS_TYPE_GRID:
                crossImageInfo.type = CrossType.CrossTypeGrid;
                break;
            case NaviConstant.CrossType.CROSS_TYPE_VECTOR:
                crossImageInfo.type = CrossType.CrossTypeVector;
                break;
            case NaviConstant.CrossType.CROSS_TYPE_3_D:
                crossImageInfo.type = CrossType.CrossType3D;
                break;
            default:
                break;
        }
        switch (crossInfo.getVectorType()) {
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_INVALID:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeInvalid;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_COMMON:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeCommon;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_ROUNDABOUT:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeRoundabout;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_CONFUSION:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeConfusion;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_NEAR:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeNear;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_DOUBLE_LIGHT:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeDoubleLight;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_SOLID_LINE:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeSolidLine;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_SOLID_NEAR:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeSolidNear;
                break;
            case NaviConstant.VectorCrossImageType.VECTOR_CROSS_IMAGE_TYPE_MIX_REVERSE:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeMixReverse;
                break;
            default:
                break;
        }
        crossImageInfo.dataBuf = crossInfo.getDataBuf();
        crossImageInfo.arrowDataBuf = crossInfo.getArrowDataBuf();
        crossImageInfo.isOnlyVector = crossInfo.isOnlyVector();
        crossImageInfo.distance = crossInfo.getDistance();
        return crossImageInfo;
    }

    /**
     * @param routeParam routeParam
     * @param obj        obj
     * @return entity
     */
    public static NaviViaEntity getNaviViaEntity(final RouteParam routeParam, final Object obj) {
        final NaviViaEntity naviViaEntity = new NaviViaEntity();
        naviViaEntity.setName(routeParam.getName());
        naviViaEntity.setAddress(routeParam.getAddress());
        naviViaEntity.setRealPos(routeParam.getRealPos());
        naviViaEntity.setPid(routeParam.getPoiID());
        naviViaEntity.setChargeInfo(routeParam.getChargeInfo());
        if (obj != null) {
            if (obj instanceof NaviEtaInfo) {
                final NaviEtaInfo naviEtaInfo = (NaviEtaInfo) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(naviEtaInfo.getAllTime()));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppContext.getInstance().getMContext(), naviEtaInfo.getAllDist(), naviEtaInfo.getAllTime()));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppContext.getInstance().getMContext(), naviEtaInfo.getAllTime()));
                naviViaEntity.setmArriveTimeStamp(naviEtaInfo.getAllTime());
            } else if (obj instanceof NaviEtaInfo.NaviTimeAndDist) {
                final NaviEtaInfo.NaviTimeAndDist timeAndDist = (NaviEtaInfo.NaviTimeAndDist) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(timeAndDist.time));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppContext.getInstance().getMContext(), timeAndDist.dist, timeAndDist.time));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppContext.getInstance().getMContext(), timeAndDist.time));
                naviViaEntity.setmArriveTimeStamp(timeAndDist.time);
            }
        }
        return naviViaEntity;
    }

    /**
     * @param obj obj
     * @return entity
     */
    public static PoiInfoEntity getPoiInfoEntity(final Object obj) {
        if (obj instanceof NaviParkingEntity naviParkingEntity) {
            return new PoiInfoEntity().setName(naviParkingEntity.getName())
                    .setAddress(naviParkingEntity.getAddress())
                    .setPoiType(naviParkingEntity.getPoiType())
                    .setPid(naviParkingEntity.getPid())
                    .setPoint(naviParkingEntity.getPoint());
        } else if (obj instanceof RouteParam routeParam) {
            return new PoiInfoEntity().setName(routeParam.getName())
                    .setAddress(routeParam.getAddress())
                    .setPoiType(routeParam.getPoiType())
                    .setPid(routeParam.getPoiID())
                    .setPoint(routeParam.getRealPos());
        }
        return null;
    }

    /**
     * @param poiInfoEntity entity
     * @param isEndPoi      endPoi
     * @return entity
     */
    public static NaviParkingEntity getNaviParkingEntity(final PoiInfoEntity poiInfoEntity,
                                                         final boolean isEndPoi) {
        if (null == poiInfoEntity) {
            Logger.e(TAG, "poiInfoEntity is null");
            return new NaviParkingEntity();
        }
        final String distance = poiInfoEntity.getDistance();
        final String meter = AppContext.getInstance().getMContext().getString(com.android.utils.R.string.meter);
        final String km = AppContext.getInstance().getMContext().getString(com.android.utils.R.string.km);
        final NaviParkingEntity naviParkingEntity = new NaviParkingEntity()
                .setEndPoi(isEndPoi)
                .setName(poiInfoEntity.getName())
                .setAddress(poiInfoEntity.getAddress())
                .setPid(poiInfoEntity.getPid())
                .setPoiType(poiInfoEntity.getPoiType())
                .setDistance(distance)
                .setPoint(poiInfoEntity.getPoint())
                .setChargeInfoList(poiInfoEntity.getChargeInfoList());
        if (distance.contains(meter)) {
            naviParkingEntity.setSortDis(Double.parseDouble(distance.replace(meter, "").trim()));
        } else if (distance.contains(km)) {
            naviParkingEntity.setSortDis(Double.parseDouble(distance.replace(km, "").trim()) * 1000);
        }
        final List<ParkingInfo> parkingInfoList = poiInfoEntity.getParkingInfoList();
        if (!ConvertUtils.isEmpty(parkingInfoList)) {
            final ParkingInfo parkingInfo = parkingInfoList.get(0);
//            List<SearchParkInOutInfo> parkInOutInfos =  parkingInfo.getSearchParkInOutInfos();
//            naviParkingEntity
            final int spaceTotal = parkingInfo.getSpaceTotal();
            final int spaceFree = parkingInfo.getSpaceFree();
            if (spaceTotal > 0 && spaceFree > -1) {
                naviParkingEntity.setSpaceTotal(spaceTotal)
                        .setSpaceFree(spaceFree);
                //-停车位紧张：总车位数<=30个，剩余车位<30% ；总车位数>30个，剩余车位<10% 或 剩余车位少于10个。
                if ((spaceTotal <= 30 && ((double) (spaceFree / spaceTotal) < 0.3)) ||
                        (spaceTotal > 30 && (spaceFree < 10 || ((double) (spaceFree / spaceTotal) < 0.1)))) {
                    naviParkingEntity.setTag(AppContext.getInstance().getMContext().getString(com.android.utils.R.string.navi_parking_tight));
                } else {
                    naviParkingEntity.setTag(AppContext.getInstance().getMContext().getString(com.android.utils.R.string.navi_parking_adequate));
                }
                naviParkingEntity.setNum(String.valueOf(spaceFree / spaceTotal));
            } else {
                Logger.d("SceneNaviParkListImpl spaceTotal= " + spaceTotal + ",spaceFree= " + spaceFree);
            }
        }
        return naviParkingEntity;
    }

    public static List<NaviMixForkInfo> formaterMixForkList(List<MixForkInfo> mixForkInfos) {
        List<NaviMixForkInfo> naviMixForkInfos = new ArrayList<>();
        for (MixForkInfo mixFork : mixForkInfos) {
            Coord2DDouble coord2DDouble = mixFork.pos;
            GeoPoint geoPoint = new GeoPoint(coord2DDouble.lon, coord2DDouble.lat);
            NaviMixForkInfo naviMixForkInfo = new NaviMixForkInfo(geoPoint,
                    mixFork.dist, mixFork.roadclass, mixFork.segmentIndex);
            naviMixForkInfos.add(naviMixForkInfo);
        }
        return naviMixForkInfos;
    }

    /**
     * 数据转化
     *
     * @param report 驾驶报告
     * @return NaviDriveReportEntity
     */
    public static NaviDriveReportEntity formaterDriveReport(final DriveReport report) {
        final NaviDriveReportEntity naviDriveReportEntity = new NaviDriveReportEntity();
        naviDriveReportEntity.setVehicleType(report.vehicleType);
        final ArrayList<NaviDriveReportEntity.NaviDriveEventEntity> driveEventList =
                (ArrayList<NaviDriveReportEntity.NaviDriveEventEntity>) report.
                        driverEventList.stream()
                        .map(event -> new NaviDriveReportEntity.NaviDriveEventEntity(
                                event.lat,
                                event.lon,
                                event.type,
                                event.time,
                                event.level
                        ))
                        .collect(Collectors.toList());
        naviDriveReportEntity.setDriverEventList(driveEventList);
        final NaviDriveReportEntity.NaviStatisticsInfoEntity naviStatisticsInfoEntity =
                new NaviDriveReportEntity.NaviStatisticsInfoEntity();
        final NaviStatisticsInfo naviStatisticsInfo = report.blNaviStatisticsInfo;
        naviStatisticsInfoEntity.setStartUTC(
                naviStatisticsInfo.startUTC);
        naviStatisticsInfoEntity.setStartSecond(
                naviStatisticsInfo.startSecond);
        naviStatisticsInfoEntity.setNormalRouteTime(
                naviStatisticsInfo.normalRouteTime);
        naviStatisticsInfoEntity.setSavedTime(
                naviStatisticsInfo.savedTime);
        naviStatisticsInfoEntity.setEstimateTime(
                naviStatisticsInfo.estimateTime);
        naviStatisticsInfoEntity.setEstimateDist(
                naviStatisticsInfo.estimateDist);
        naviStatisticsInfoEntity.setDrivenTime(
                naviStatisticsInfo.drivenTime);
        naviStatisticsInfoEntity.setDrivenDist(
                naviStatisticsInfo.drivenDist);
        naviStatisticsInfoEntity.setAverageSpeed(
                naviStatisticsInfo.averageSpeed);
        naviStatisticsInfoEntity.setHighestSpeed(
                naviStatisticsInfo.highestSpeed);
        naviStatisticsInfoEntity.setOverspeedCount(
                naviStatisticsInfo.overspeedCount);
        naviStatisticsInfoEntity.setOverspeedCountEx(
                naviStatisticsInfo.overspeedCountEx);
        naviStatisticsInfoEntity.setHighwayOverSpeedLowCnt(
                naviStatisticsInfo.highwayOverSpeedLowCnt);
        naviStatisticsInfoEntity.setHighwayOverSpeedMidCnt(
                naviStatisticsInfo.highwayOverSpeedMidCnt);
        naviStatisticsInfoEntity.setHighwayOverSpeedHighCnt(
                naviStatisticsInfo.highwayOverSpeedHighCnt);
        naviStatisticsInfoEntity.setNormalOverSpeedLowCnt(
                naviStatisticsInfo.normalOverSpeedLowCnt);
        naviStatisticsInfoEntity.setNormalOverSpeedMidCnt(
                naviStatisticsInfo.normalOverSpeedMidCnt);
        naviStatisticsInfoEntity.setNormalOverSpeedHighCnt(
                naviStatisticsInfo.normalOverSpeedHighCnt);
        naviStatisticsInfoEntity.setAccidentAreaCount(
                naviStatisticsInfo.accidentAreaCount);
        naviStatisticsInfoEntity.setRerouteCount(
                naviStatisticsInfo.rerouteCount);
        naviStatisticsInfoEntity.setBrakesCount(
                naviStatisticsInfo.brakesCount);
        naviStatisticsInfoEntity.setSlowTime(
                naviStatisticsInfo.slowTime);
        naviStatisticsInfoEntity.setArrTrafficDist(
                naviStatisticsInfo.arrTrafficDist);
        naviStatisticsInfoEntity.setArrRoadDist(
                naviStatisticsInfo.arrRoadDist);
        naviStatisticsInfoEntity.setArrSpeedClass(
                naviStatisticsInfo.arrSpeedClass);
        naviDriveReportEntity.setNaviStatisticsInfoEntity(naviStatisticsInfoEntity);
        return naviDriveReportEntity;
    }

    /**
     * 引导中的道路设施数据转化
     * @param list list
     * @return 转化后的数据
     */
    public static ArrayList<NaviRoadFacilityEntity> formatRoadFacilityList(
            ArrayList<NaviRoadFacility> list) {
        ArrayList<NaviRoadFacilityEntity> listEntity = new ArrayList<>();
        if (!ConvertUtils.isEmpty(list)) {
            for (NaviRoadFacility facility : list) {
                if (facility == null) {
                    continue;
                }
                NaviRoadFacilityEntity entity = new NaviRoadFacilityEntity();
                entity.setDistance(facility.distance);
                entity.setType(facility.type);
                if (facility.coord2D != null) {
                    GeoPoint point = new GeoPoint();
                    point.setLat(facility.coord2D.lat);
                    point.setLon(facility.coord2D.lon);
                    entity.setGeoPoint(point);
                }
                listEntity.add(entity);
            }
        }
        return listEntity;
    }
}
