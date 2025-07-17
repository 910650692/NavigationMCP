package com.sgm.navi.service.adapter.navi.bls;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.model.Coord2DDouble;
import com.autonavi.gbl.common.path.model.ChargeStationInfo;
import com.autonavi.gbl.common.path.model.ChargingStation;
import com.autonavi.gbl.common.path.model.ElecVehicleETAInfo;
import com.autonavi.gbl.common.path.model.EnergyEndPoint;
import com.autonavi.gbl.common.path.model.LightBarItem;
import com.autonavi.gbl.common.path.model.TollGateInfo;
import com.autonavi.gbl.common.path.model.TrafficItem;
import com.autonavi.gbl.common.path.model.ViaMergeInfo;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.CrossNaviInfo;
import com.autonavi.gbl.guide.model.CruiseFacilityInfo;
import com.autonavi.gbl.guide.model.CruiseInfo;
import com.autonavi.gbl.guide.model.ExitDirectionInfo;
import com.autonavi.gbl.guide.model.LaneInfo;
import com.autonavi.gbl.guide.model.LightBarDetail;
import com.autonavi.gbl.guide.model.LightBarInfo;
import com.autonavi.gbl.guide.model.LightInfo;
import com.autonavi.gbl.guide.model.LightState;
import com.autonavi.gbl.guide.model.ManeuverIconResponseData;
import com.autonavi.gbl.guide.model.ManeuverInfo;
import com.autonavi.gbl.guide.model.MixForkInfo;
import com.autonavi.gbl.guide.model.NaviCameraExt;
import com.autonavi.gbl.guide.model.NaviCongestionDetailInfo;
import com.autonavi.gbl.guide.model.NaviCongestionInfo;
import com.autonavi.gbl.guide.model.NaviFacility;
import com.autonavi.gbl.guide.model.NaviFacilityType;
import com.autonavi.gbl.guide.model.NaviGreenWaveCarSpeed;
import com.autonavi.gbl.guide.model.NaviInfo;
import com.autonavi.gbl.guide.model.NaviInfoPanel;
import com.autonavi.gbl.guide.model.NaviIntervalCameraDynamicInfo;
import com.autonavi.gbl.guide.model.NaviRoadFacility;
import com.autonavi.gbl.guide.model.NaviSubCameraExt;
import com.autonavi.gbl.guide.model.SAPAInquireResponseData;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.model.TimeAndDist;
import com.autonavi.gbl.guide.model.TmcInfoData;
import com.autonavi.gbl.guide.model.TrafficLightCountdown;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.cruise.CruiseFacilityEntity;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.CrossImageEntity;
import com.sgm.navi.service.define.navi.FyChargingStation;
import com.sgm.navi.service.define.navi.FyElecVehicleETAInfo;
import com.sgm.navi.service.define.navi.FyEnergyEndPoint;
import com.sgm.navi.service.define.navi.FyViaMergeInfo;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.LightInfoEntity;
import com.sgm.navi.service.define.navi.NaviCongestionDetailInfoEntity;
import com.sgm.navi.service.define.navi.NaviCongestionInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviInfoEntity;
import com.sgm.navi.service.define.navi.NaviManeuverInfo;
import com.sgm.navi.service.define.navi.NaviMixForkInfo;
import com.sgm.navi.service.define.navi.NaviRoadFacilityEntity;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navi.NaviViaEntity;
import com.sgm.navi.service.define.navi.SapaInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navi.TrafficLightCountdownEntity;
import com.sgm.navi.service.define.route.RouteChargeStationDetailInfo;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.utils.NumberUtils;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;

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
            soundInfoEntity.setRangeType(info.rangeType);
        }
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
            naviExitDirectionInfo.setDisToCurrentPos(directionInfo.disToCurrentPos);
        }
        return naviExitDirectionInfo;
    }

    /**
     * @param tollGateInfo tollGateInfo
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity formatTollGateInfo(final TollGateInfo tollGateInfo) {
        if (ConvertUtils.isNull(mCurrentSapaInfoEntity)) {
            mCurrentSapaInfoEntity = new SapaInfoEntity();
        }
        if (tollGateInfo != null) {
            mCurrentSapaInfoEntity.setLaneTypes(tollGateInfo.laneTypes);
        } else {
            mCurrentSapaInfoEntity.setLaneTypes(new ArrayList<>());
        }
        return mCurrentSapaInfoEntity;
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
            naviEtaInfo.setRemainTime(naviInfo.routeRemain.time);
            naviEtaInfo.setRemainDist(naviInfo.routeRemain.dist);
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
            naviEtaInfo.mDriveDist = naviInfo.driveDist;
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
    /**
     * @param lightBarInfo   lightBarInfo
     * @param lightBarDetail lightBarDetail
     * @return NaviTmcInfo
     */
    public static NaviTmcInfo formatTmcLightBar(final ArrayList<LightBarInfo> lightBarInfo,
                                                final LightBarDetail lightBarDetail) {
        if (lightBarInfo == null || lightBarDetail == null) {
            return new NaviTmcInfo(); // 或者根据业务需求返回 null / 抛异常
        }

        final NaviTmcInfo naviTmcInfo = new NaviTmcInfo();
        final NaviTmcInfo.NaviLightBarDetail naviLightBarDetail = getNaviLightBarDetail(lightBarDetail);
        naviTmcInfo.setLightBarDetail(naviLightBarDetail);

        final ArrayList<NaviTmcInfo.NaviLightBarInfo> barInfoList = new ArrayList<>(lightBarInfo.size());

        for (LightBarInfo info : lightBarInfo) {
            if (info == null) continue;

            final NaviTmcInfo.NaviLightBarInfo barInfo = new NaviTmcInfo.NaviLightBarInfo();
            barInfo.setPathID(info.pathID);

            final ArrayList<NaviTmcInfo.NaviLightBarItem> itemArrayList;
            if (info.itemList != null) {
                itemArrayList = new ArrayList<>(info.itemList.size());
                for (LightBarItem lightBarItem : info.itemList) {
                    itemArrayList.add(convertToNaviLightBarItem(lightBarItem));
                }
            } else {
                itemArrayList = new ArrayList<>();
            }

            barInfo.setItemList(itemArrayList);
            barInfoList.add(barInfo);
        }

        naviTmcInfo.setLightBarInfo(barInfoList);
        return naviTmcInfo;
    }

    private static NaviTmcInfo.NaviLightBarItem convertToNaviLightBarItem(LightBarItem lightBarItem) {
        if (lightBarItem == null) {
            return new NaviTmcInfo.NaviLightBarItem();
        }

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

        return item;
    }


    private static @NonNull NaviTmcInfo.NaviLightBarDetail getNaviLightBarDetail(LightBarDetail lightBarDetail) {
        final NaviTmcInfo.NaviLightBarDetail naviLightBarDetail = new NaviTmcInfo.NaviLightBarDetail();
        naviLightBarDetail.setPathID(lightBarDetail.pathID);
        naviLightBarDetail.setTotalDistance(lightBarDetail.totalDistance);
        naviLightBarDetail.setRestDistance(lightBarDetail.restDistance);
        naviLightBarDetail.setFinishDistance(lightBarDetail.finishDistance);
        final ArrayList<NaviTmcInfo.NaviTmcInfoData> tmcInfoList = getNaviTmcInfoData(lightBarDetail);
        naviLightBarDetail.setTmcInfoData(tmcInfoList);
        return naviLightBarDetail;
    }

    private static @NonNull ArrayList<NaviTmcInfo.NaviTmcInfoData> getNaviTmcInfoData(LightBarDetail lightBarDetail) {
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
        return tmcInfoList;
    }

    /**
     * @param trafficItem trafficItem
     * @return NaviTmcInfo.NaviTrafficItem
     */
    /**
     * @param trafficItem trafficItem
     * @return NaviTmcInfo.NaviTrafficItem
     */
    private static NaviTmcInfo.NaviTrafficItem formatTrafficItem(final TrafficItem trafficItem) {
        if (trafficItem == null) {
            return null; // 或者抛出异常，视业务需求而定
        }
        final NaviTmcInfo.NaviTrafficItem naviTrafficItem = new NaviTmcInfo.NaviTrafficItem();
        GeoPoint startPoint = null;
        if (trafficItem.startPnt != null) {
            startPoint = new GeoPoint(trafficItem.startPnt.lon, trafficItem.startPnt.lat, trafficItem.startPnt.z);
        }
        naviTrafficItem.setStartPnt(startPoint);
        GeoPoint endPoint = null;
        if (trafficItem.endPnt != null) {
            endPoint = new GeoPoint(trafficItem.endPnt.lon, trafficItem.endPnt.lat, trafficItem.endPnt.z);
        }
        naviTrafficItem.setEndPnt(endPoint);
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
    public static CameraInfoEntity formatNearestCameraInfo(final ArrayList<NaviCameraExt> naviCameraList) {
        CameraInfoEntity bestMatch = new CameraInfoEntity();
        if (ConvertUtils.isEmpty(naviCameraList)) {
            return bestMatch;
        }
        for (NaviCameraExt cameraInfo : naviCameraList) {
            if (ConvertUtils.isEmpty(cameraInfo.subCameras) || cameraInfo.coord2D == null) {
                continue;
            }
            for (NaviSubCameraExt subCamera : cameraInfo.subCameras) {
                if (ConvertUtils.isEmpty(subCamera.speed)) {
                    continue;
                }
                for (Short speed : subCamera.speed) {
                    if (isValidSpeed(speed)) {
                        // 如果当前摄像头距离更近，则更新为最佳匹配
                        if (cameraInfo.distance < bestMatch.getDistance()) {
                            bestMatch.setCameraId(cameraInfo.cameraId);
                            bestMatch.setCoord2D(new GeoPoint(cameraInfo.coord2D.lon, cameraInfo.coord2D.lat));
                            bestMatch.setDistance(cameraInfo.distance);
                            bestMatch.setRoadClass(cameraInfo.roadClass);
                            bestMatch.setSubCameraId(subCamera.cameraId);
                            bestMatch.setSubType(subCamera.subType);
                            bestMatch.setMatch(subCamera.isMatch);
                            bestMatch.setSpeed(speed);
                        }
                    }
                }
            }
        }
        return bestMatch;
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
    }

    /**
     * @param list list
     * @return SapaInfoEntity
     */
    public static SapaInfoEntity forMatSAPAInfo(final ArrayList<NaviFacility> list) {
        if (ConvertUtils.isNull(mCurrentSapaInfoEntity)) {
            mCurrentSapaInfoEntity = new SapaInfoEntity();
        }
        if (!ConvertUtils.isEmpty(list)) {
            final int size = list.size();
            final ArrayList<SapaInfoEntity.SAPAItem> sapaItems = new ArrayList<>();
            final NaviFacility first = list.get(0);
            if (first.type == NaviFacilityType.NaviFacilityTypeServiceArea) {//服务区
                mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.SPAS_LIST);
            } else if (first.type == NaviFacilityType.NaviFacilityTypeTollGate) {//收费站
                mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_LIST);
            }
            sapaItems.add(getSAPAItem(first));
            if (size > 1) {
                final NaviFacility second = list.get(1);
                if (first.type == NaviFacilityType.NaviFacilityTypeServiceArea && second.type == NaviFacilityType.NaviFacilityTypeServiceArea) {
                    mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.SPAS_LIST);//两个服务区
                    sapaItems.add(getSAPAItem(second));
                } else if ((first.type == NaviFacilityType.NaviFacilityTypeServiceArea && second.type == NaviFacilityType.NaviFacilityTypeTollGate)
                        || (first.type == NaviFacilityType.NaviFacilityTypeTollGate && second.type == NaviFacilityType.NaviFacilityTypeServiceArea)) {
                    mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_AND_SPAS);//一个服务区一个收费站
                    sapaItems.add(getSAPAItem(second));
                } else if (first.type == NaviFacilityType.NaviFacilityTypeTollGate && second.type == NaviFacilityType.NaviFacilityTypeTollGate) {
                    mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_LIST);//两个收费站
                    sapaItems.add(getSAPAItem(second));
                }
            }
            mCurrentSapaInfoEntity.setList(sapaItems);
        } else {
            mCurrentSapaInfoEntity.setType(NaviConstant.SapaItemsType.AUTO_UNKNOWN_ERROR);
        }
        return mCurrentSapaInfoEntity;
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
     * @param routeParam routeParam
     * @param obj        obj
     * @return entity
     */
    public static NaviViaEntity getNaviViaEntity(final RouteParam routeParam, final Object obj, boolean isUserAdd, boolean isEndPoi) {
        final NaviViaEntity naviViaEntity = new NaviViaEntity();
        String name = routeParam.getName();
        Logger.i(TAG, "getNaviViaEntity: ", name);
        naviViaEntity.setName(name);
        naviViaEntity.setAddress(routeParam.getAddress());
        naviViaEntity.setRealPos(routeParam.getRealPos());
        naviViaEntity.setPid(routeParam.getPoiID());
        naviViaEntity.setChargeInfo(routeParam.getChargeInfo());
        naviViaEntity.setIsUserAdd(isUserAdd);
        naviViaEntity.setIsEndPoi(isEndPoi);
        if (obj != null) {
            if (obj instanceof NaviEtaInfo) {
                final NaviEtaInfo naviEtaInfo = (NaviEtaInfo) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(naviEtaInfo.getRemainTime()));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppCache.getInstance().getMContext(), naviEtaInfo.getRemainDist(), naviEtaInfo.getRemainTime()));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), naviEtaInfo.getRemainTime()));
                naviViaEntity.setmArriveTimeStamp(naviEtaInfo.getRemainTime());
            } else if (obj instanceof NaviEtaInfo.NaviTimeAndDist) {
                final NaviEtaInfo.NaviTimeAndDist timeAndDist = (NaviEtaInfo.NaviTimeAndDist) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(timeAndDist.time));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppCache.getInstance().getMContext(), timeAndDist.dist, timeAndDist.time));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppCache.getInstance().getMContext(), timeAndDist.time));
                naviViaEntity.setmArriveTimeStamp(timeAndDist.time);
            }
        }
        return naviViaEntity;
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
     * 引导中的道路设施数据转化
     *
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

    public static TrafficLightCountdownEntity formatTrafficLightCountdown(
            TrafficLightCountdown trafficLightCountdown) {
        TrafficLightCountdownEntity entity = new TrafficLightCountdownEntity();
        if (null != trafficLightCountdown) {
            LightInfoEntity lightInfoEntity = new LightInfoEntity();
            LightInfo lightInfo = trafficLightCountdown.lightInfo;
            if (null != lightInfo) {
                ArrayList<LightInfoEntity.LightStateEntity> lightStateEntities = getLightStateEntities(lightInfo);
                lightInfoEntity.setMLightStates(lightStateEntities);
                lightInfoEntity.setMDesc(lightInfo.desc);
                lightInfoEntity.setMDir(lightInfo.dir);
                lightInfoEntity.setMPhase(lightInfo.phase);
                lightInfoEntity.setMShowType(lightInfo.showType);
                lightInfoEntity.setMStandardType(lightInfo.standardType);
                lightInfoEntity.setMWaitNum(lightInfo.waitNum);
            }
            entity.setMLightInfo(lightInfoEntity);
            entity.setMLinkIndex(trafficLightCountdown.linkIndex);
            entity.setMLinkID(trafficLightCountdown.linkID);
            GeoPoint geoPoint = new GeoPoint();
            if (null != trafficLightCountdown.position) {
                geoPoint.setLon(trafficLightCountdown.position.lon);
                geoPoint.setLat(trafficLightCountdown.position.lat);
            }
            entity.setMPosition(geoPoint);
            entity.setMStatus(trafficLightCountdown.status);
            entity.setMPathID(trafficLightCountdown.pathID);
            entity.setMSegmentIndex(trafficLightCountdown.segmentIndex);
        }
        return entity;
    }

    private static @NonNull ArrayList<LightInfoEntity.LightStateEntity> getLightStateEntities(LightInfo lightInfo) {
        ArrayList<LightInfoEntity.LightStateEntity> lightStateEntities = new ArrayList<>();
        ArrayList<LightState> lightStates = lightInfo.lightStates;
        if (!ConvertUtils.isEmpty(lightStates)) {
            for (LightState lightState : lightStates) {
                LightInfoEntity.LightStateEntity lightStateEntity =
                        new LightInfoEntity.LightStateEntity();
                lightStateEntity.setMLightType(lightState.lightType);
                lightStateEntity.setMStime(lightState.stime);
                lightStateEntity.setMEtime(lightState.etime);
                lightStateEntity.setMLastSeconds(lightState.lastSeconds);
                lightStateEntities.add(lightStateEntity);
            }
        }
        return lightStateEntities;
    }

    public static CruiseFacilityEntity formatCruiseFacility(ArrayList<CruiseFacilityInfo> facilityInfoList) {
        if (facilityInfoList == null || facilityInfoList.isEmpty()) {
            return null;
        }
        CruiseFacilityEntity cruiseFacilityEntity = new CruiseFacilityEntity();
        CruiseFacilityInfo cruiseFacilityInfo = facilityInfoList.get(0);
        cruiseFacilityEntity.setDistance(cruiseFacilityInfo.distance);
        cruiseFacilityEntity.setLimitSpeed(cruiseFacilityInfo.limitSpeed);
        cruiseFacilityEntity.setType(cruiseFacilityInfo.type);
        return cruiseFacilityEntity;
    }

    public static NaviCongestionInfoEntity formatNaviCongestionInfo(NaviCongestionInfo info) {
        if (info == null) {
            return null;
        }
        NaviCongestionInfoEntity naviCongestionInfoEntity = new NaviCongestionInfoEntity();
        naviCongestionInfoEntity.setTotalRemainDist(info.totalRemainDist);
        naviCongestionInfoEntity.setTotalTimeOfSeconds(info.totalTimeOfSeconds);
        naviCongestionInfoEntity.setUnobstructed(info.unobstructed);
        ArrayList<NaviCongestionDetailInfoEntity> list = new ArrayList<>();
        naviCongestionInfoEntity.setCongestionInfos(list);
        ArrayList<NaviCongestionDetailInfo> congestionInfos = info.congestionInfos;
        if (congestionInfos == null) {
            return naviCongestionInfoEntity;
        }
        for (int i = 0; i < congestionInfos.size(); i++) {
            NaviCongestionDetailInfo naviCongestionDetailInfo = congestionInfos.get(i);
            if (naviCongestionDetailInfo == null) {
                continue;
            }
            NaviCongestionDetailInfoEntity entity = new NaviCongestionDetailInfoEntity();
            entity.setBeginSegmentIndex(naviCongestionDetailInfo.beginSegmentIndex);
            entity.setBeginLinkIndex(naviCongestionDetailInfo.beginLinkIndex);
            entity.setRemainDist(naviCongestionDetailInfo.remainDist);
            entity.setStatus(naviCongestionDetailInfo.status);
            entity.setTimeOfSeconds(naviCongestionDetailInfo.timeOfSeconds);
            list.add(entity);
        }
        return naviCongestionInfoEntity;
    }

    public static List<FyElecVehicleETAInfo> convertVehicleInfo(ArrayList<ElecVehicleETAInfo> elecVehicleETAInfo) {
        if (ConvertUtils.isEmpty(elecVehicleETAInfo)) return null;
        final List<FyElecVehicleETAInfo> desObj = new ArrayList<>(elecVehicleETAInfo.size());
        for (ElecVehicleETAInfo etaInfo : elecVehicleETAInfo) {
            if (null == etaInfo) continue;

            FyElecVehicleETAInfo fyElecVehicleETAInfo = getFyElecVehicleETAInfo(etaInfo);

            ArrayList<ChargingStation> chargeStationInfo = etaInfo.chargeStationInfo;

            if (null != chargeStationInfo) {
                ArrayList<FyChargingStation> fyChargingStations = new ArrayList<>(chargeStationInfo.size());
                for (ChargingStation chargeStation : chargeStationInfo) {
                    FyChargingStation fyChargingStation = new FyChargingStation();
                    BigInteger chargeSum = chargeStation.chargeEnrgySum;
                    if (null != chargeSum) {
                        fyChargingStation.setChargeEnrgySum(chargeSum.longValue());
                    }
                    fyChargingStation.setChargeInfo(getRouteChargeStationDetailInfo(chargeStation.chargeInfo));
                    fyChargingStations.add(fyChargingStation);
                }
                fyElecVehicleETAInfo.setChargeStationInfo(fyChargingStations);
            }

            desObj.add(fyElecVehicleETAInfo);
        }
        return desObj;
    }

    private static @NonNull RouteChargeStationDetailInfo getRouteChargeStationDetailInfo(ChargeStationInfo chargeStation) {
        RouteChargeStationDetailInfo chargeStationDetailInfo = new RouteChargeStationDetailInfo();
        if (null != chargeStation) {
            chargeStationDetailInfo.setMIndex(chargeStation.index);
            chargeStationDetailInfo.setMName(chargeStation.name);
            chargeStationDetailInfo.setMRemainingCapacity(chargeStation.remainingCapacity);
            chargeStationDetailInfo.setMRemainingPercent(chargeStation.remainingPercent);
            chargeStationDetailInfo.setMShow(new com.sgm.navi.service.define.route.Coord2DDouble(chargeStation.show.lon, chargeStation.show.lat));
            chargeStationDetailInfo.setMProjective(new com.sgm.navi.service.define.route.Coord2DDouble(chargeStation.projective.lon, chargeStation.projective.lat));
            chargeStationDetailInfo.setMPoiID(chargeStation.poiID);
            chargeStationDetailInfo.setMBrandName(chargeStation.brandName);
            chargeStationDetailInfo.setMMaxPower(chargeStation.maxPower);
            chargeStationDetailInfo.setMChargePercent(chargeStation.chargePercent);
            chargeStationDetailInfo.setMChargeTime(chargeStation.chargeTime);
        }
        return chargeStationDetailInfo;
    }

    private static @NonNull FyElecVehicleETAInfo getFyElecVehicleETAInfo(ElecVehicleETAInfo etaInfo) {
        FyElecVehicleETAInfo fyElecVehicleETAInfo = new FyElecVehicleETAInfo();
        fyElecVehicleETAInfo.setEnergyEndFlag(etaInfo.energyEndFlag);
        fyElecVehicleETAInfo.setPathID(etaInfo.pathID);

        EnergyEndPoint energyEndPoint = etaInfo.energyEndPoint;
        FyEnergyEndPoint fyEnergyEndPoint = new FyEnergyEndPoint(new GeoPoint(energyEndPoint.show.lat, energyEndPoint.show.lon),
                energyEndPoint.segmentIdx, energyEndPoint.linkIndex);
        fyElecVehicleETAInfo.setEnergyEndPoint(fyEnergyEndPoint);

        fyElecVehicleETAInfo.setElecLinkConsume(etaInfo.elecLinkConsume);
        fyElecVehicleETAInfo.setEnergySum(etaInfo.energySum);
        return fyElecVehicleETAInfo;
    }
}