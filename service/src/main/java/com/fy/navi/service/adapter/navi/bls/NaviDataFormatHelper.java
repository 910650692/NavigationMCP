package com.fy.navi.service.adapter.navi.bls;

import com.android.utils.ConvertUtils;
import com.android.utils.TimeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.autonavi.gbl.common.path.model.LightBarItem;
import com.autonavi.gbl.common.path.model.RoadClass;
import com.autonavi.gbl.common.path.model.TollGateInfo;
import com.autonavi.gbl.common.path.model.TrafficItem;
import com.autonavi.gbl.guide.model.CrossImageInfo;
import com.autonavi.gbl.guide.model.CrossNaviInfo;
import com.autonavi.gbl.guide.model.CrossType;
import com.autonavi.gbl.guide.model.CruiseInfo;
import com.autonavi.gbl.guide.model.ExitDirectionInfo;
import com.autonavi.gbl.guide.model.LaneInfo;
import com.autonavi.gbl.guide.model.LightBarDetail;
import com.autonavi.gbl.guide.model.LightBarInfo;
import com.autonavi.gbl.guide.model.ManeuverIconID;
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
import com.autonavi.gbl.guide.model.NaviSubCameraExt;
import com.autonavi.gbl.guide.model.SAPAInquireResponseData;
import com.autonavi.gbl.guide.model.SoundInfo;
import com.autonavi.gbl.guide.model.TimeAndDist;
import com.autonavi.gbl.guide.model.TmcInfoData;
import com.autonavi.gbl.guide.model.VectorCrossImageType;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.R;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.NaviMixForkInfo;
import com.fy.navi.service.define.navi.NaviParkingEntity;
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

// TODO: 2024/12/30 数据需要根据业务精简一下
public class NaviDataFormatHelper {
    public static LaneInfoEntity forMatLaneInfo(LaneInfo info) {
        LaneInfoEntity laneInfoEntity = new LaneInfoEntity();
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

    public static CruiseInfoEntity formatCruiseCameraExt(ArrayList<NaviCameraExt> list) {
        CruiseInfoEntity cruiseCameraList = new CruiseInfoEntity();
        if (list != null && !list.isEmpty()) {
            for (NaviCameraExt cameraInfo : list) {
                if (cameraInfo.subCameras == null || cameraInfo.subCameras.isEmpty()) {
                    continue;
                }
                NaviSubCameraExt subCameraExt = cameraInfo.subCameras.get(0);
                if (subCameraExt.speed == null || subCameraExt.speed.isEmpty()) {
                    continue;
                }
                cruiseCameraList.setSpeed(subCameraExt.speed);
                cruiseCameraList.distance = cameraInfo.distance;
            }
        }
        return cruiseCameraList;
    }

    public static SoundInfoEntity formatSoundInfo(SoundInfo info) {
        SoundInfoEntity soundInfoEntity = new SoundInfoEntity();
        if (null != info) {
            soundInfoEntity.setText(info.text);
        }
        return soundInfoEntity;
    }

    public static SoundInfoEntity formatSoundInfo(int type) {
        SoundInfoEntity soundInfoEntity = new SoundInfoEntity();
        soundInfoEntity.setRingType(type);
        soundInfoEntity.setRingType(true);
        return soundInfoEntity;
    }

    public static CruiseInfoEntity formatCruiseInfo(CruiseInfo cruiseInfo) {
        CruiseInfoEntity cruiseInfoEntity = new CruiseInfoEntity();
        if (cruiseInfo != null) {
            cruiseInfoEntity.setRoadClass(cruiseInfo.roadClass);
            cruiseInfoEntity.setRoadName(cruiseInfo.roadName);
        }
        return cruiseInfoEntity;
    }


    public static NaviManeuverInfo formatManeuverInfo(ManeuverInfo maneuverInfo) {
        NaviManeuverInfo naviManeuverInfo = new NaviManeuverInfo();
        naviManeuverInfo.setDateType(NaviConstant.ManeuverDataType.Maneuver);
        if (maneuverInfo != null) {
            naviManeuverInfo.setType(maneuverInfo.type);
            naviManeuverInfo.setPathID(maneuverInfo.pathID);
            naviManeuverInfo.setSegmentIndex(maneuverInfo.segmentIndex);
            naviManeuverInfo.setManeuverID(maneuverInfo.maneuverID);
        }
        return naviManeuverInfo;
    }

    public static NaviManeuverInfo formatManeuverIconData(ManeuverIconResponseData responseData) {
        NaviManeuverInfo naviManeuverIconResponseData = new NaviManeuverInfo();
        naviManeuverIconResponseData.setDateType(NaviConstant.ManeuverDataType.ManeuverIcon);
        if (responseData != null) {
            naviManeuverIconResponseData.setData(responseData.data);
            NaviManeuverInfo.NaviManeuverConfig naviManeuverConfig = new NaviManeuverInfo.NaviManeuverConfig();
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

    public static NaviManeuverInfo formatExitDirectionInfo(ExitDirectionInfo directionInfo) {
        NaviManeuverInfo naviExitDirectionInfo = new NaviManeuverInfo();
        naviExitDirectionInfo.setDateType(NaviConstant.ManeuverDataType.ExitDirection);
        if (directionInfo != null) {
            naviExitDirectionInfo.setExitNameInfo(directionInfo.exitNameInfo);
            naviExitDirectionInfo.setDirectionInfo(directionInfo.directionInfo);
            naviExitDirectionInfo.setEntranceExit(directionInfo.entranceExit);
        }
        return naviExitDirectionInfo;
    }

    public static SapaInfoEntity formatTollGateInfo(TollGateInfo tollGateInfo) {
        SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
        if (tollGateInfo != null) {
            sapaInfoEntity.setLaneTypes(tollGateInfo.laneTypes);
        }
        return sapaInfoEntity;
    }

    public static NaviEtaInfo forMatNaviInfo(ArrayList<NaviInfo> naviInfoList) {
        NaviEtaInfo naviEtaInfo = new NaviEtaInfo();
        if (!ConvertUtils.isEmpty(naviInfoList)) {
            //取第一个路段的信息
            NaviInfo naviInfo = naviInfoList.get(NumberUtils.NUM_0);
            naviEtaInfo.pathID = naviInfo.pathID;
            naviEtaInfo.allTime = naviInfo.routeRemain.time;
            naviEtaInfo.allDist = naviInfo.routeRemain.dist;
            naviEtaInfo.type = naviInfo.type;
            naviEtaInfo.curRouteName = naviInfo.curRouteName;
            naviEtaInfo.ringOutCnt = naviInfo.ringOutCnt;
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
                NaviEtaInfo.NotAvoidInfo notAvoidInfo = new NaviEtaInfo.NotAvoidInfo();
                notAvoidInfo.type = naviInfo.notAvoidInfo.type;
                notAvoidInfo.distToCar = naviInfo.notAvoidInfo.distToCar;
                notAvoidInfo.forbidType = naviInfo.notAvoidInfo.forbidType;
                notAvoidInfo.valid = naviInfo.notAvoidInfo.valid;
                notAvoidInfo.coord2D = new GeoPoint(naviInfo.notAvoidInfo.coord2D.lon, naviInfo.notAvoidInfo.coord2D.lat, 0);
                notAvoidInfo.coord3D = new GeoPoint(naviInfo.notAvoidInfo.coord3D.lon, naviInfo.notAvoidInfo.coord3D.lat, naviInfo.notAvoidInfo.coord3D.z);
                naviEtaInfo.notAvoidInfo = notAvoidInfo;
            }
            if (!ConvertUtils.isEmpty(naviInfo.viaRemain)) {
                ArrayList<NaviEtaInfo.NaviTimeAndDist> naviTimeAndDists = new ArrayList<>();
                for (TimeAndDist timeAndDist : naviInfo.viaRemain) {
                    naviTimeAndDists.add(new NaviEtaInfo.NaviTimeAndDist(timeAndDist.time, timeAndDist.dist));
                }
                naviEtaInfo.viaRemain = naviTimeAndDists;
            }
            if (!ConvertUtils.isEmpty(naviInfo.ChargeStationRemain)) {
                ArrayList<NaviEtaInfo.NaviTimeAndDist> naviTimeAndDists = new ArrayList<>();
                for (TimeAndDist timeAndDist : naviInfo.ChargeStationRemain) {
                    naviTimeAndDists.add(new NaviEtaInfo.NaviTimeAndDist(timeAndDist.time, timeAndDist.dist));
                }
                naviEtaInfo.ChargeStationRemain = naviTimeAndDists;
            }
            if (!ConvertUtils.isEmpty(naviInfo.nextCrossInfo)) {
                ArrayList<NaviEtaInfo.NaviCrossNaviInfo> list = new ArrayList<>();
                for (CrossNaviInfo crossNaviInfo : naviInfo.nextCrossInfo) {
                    NaviEtaInfo.NaviCrossNaviInfo bean = new NaviEtaInfo.NaviCrossNaviInfo();
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
                ArrayList<NaviEtaInfo.NaviInfoPanel> list = new ArrayList<>();
                for (NaviInfoPanel naviInfoPanel : naviInfo.NaviInfoData) {
                    NaviEtaInfo.NaviInfoPanel bean = new NaviEtaInfo.NaviInfoPanel();
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
            NaviInfoPanel naviInfoPanel = naviInfo.NaviInfoData.get(naviInfo.NaviInfoFlag);
            naviEtaInfo.curManeuverID = naviInfoPanel.maneuverID;
            naviEtaInfo.nextRouteName = naviInfoPanel.nextRouteName;
            if (!ConvertUtils.isEmpty(naviInfo.nextCrossInfo)) {
                CrossNaviInfo nextCrossInfo = naviInfo.nextCrossInfo.get(NumberUtils.NUM_0);
                naviEtaInfo.nextManeuverID = nextCrossInfo.maneuverID;
                naviEtaInfo.nextDist = nextCrossInfo.curToSegmentDist;
            }
        }
        return naviEtaInfo;
    }

    public static NaviTmcInfo forMatTMCLightBar(ArrayList<LightBarInfo> lightBarInfo, LightBarDetail lightBarDetail) {
        NaviTmcInfo naviTmcInfo = new NaviTmcInfo();
        NaviTmcInfo.NaviLightBarDetail naviLightBarDetail = new NaviTmcInfo.NaviLightBarDetail();
        naviLightBarDetail.pathID = lightBarDetail.pathID;
        naviLightBarDetail.totalDistance = lightBarDetail.totalDistance;
        naviLightBarDetail.restDistance = lightBarDetail.restDistance;
        naviLightBarDetail.finishDistance = lightBarDetail.finishDistance;
        ArrayList<NaviTmcInfo.NaviTmcInfoData> tmcInfoList = new ArrayList<>();
        for (TmcInfoData tmcInfoData : lightBarDetail.tmcInfoData) {
            NaviTmcInfo.NaviTmcInfoData naviTmcInfoData = new NaviTmcInfo.NaviTmcInfoData();
            naviTmcInfoData.number = tmcInfoData.number;
            naviTmcInfoData.status = tmcInfoData.status;
            naviTmcInfoData.distance = tmcInfoData.distance;
            naviTmcInfoData.percent = tmcInfoData.percent;
            naviTmcInfoData.travelTime = tmcInfoData.travelTime;
            tmcInfoList.add(naviTmcInfoData);
        }
        naviLightBarDetail.tmcInfoData = tmcInfoList;
        naviTmcInfo.lightBarDetail = naviLightBarDetail;
        ArrayList<NaviTmcInfo.NaviLightBarInfo> list = new ArrayList<>();
        for (LightBarInfo info : lightBarInfo) {
            NaviTmcInfo.NaviLightBarInfo barInfo = new NaviTmcInfo.NaviLightBarInfo();
            barInfo.pathID = info.pathID;
            ArrayList<NaviTmcInfo.NaviLightBarItem> items = new ArrayList<>();
            for (LightBarItem lightBarItem : info.itemList) {
                NaviTmcInfo.NaviLightBarItem item = new NaviTmcInfo.NaviLightBarItem();
                item.statusFlag = lightBarItem.statusFlag;
                item.status = lightBarItem.status;
                item.fineStatus = lightBarItem.fineStatus;
                item.length = lightBarItem.length;
                item.timeOfSeconds = lightBarItem.timeOfSeconds;
                item.startSegmentIdx = lightBarItem.startSegmentIdx;
                item.startLinkIdx = lightBarItem.startLinkIdx;
                item.startLinkStatus = lightBarItem.startLinkStatus;
                item.startLinkFineStatus = lightBarItem.startLinkFineStatus;
                item.endSegmentIdx = lightBarItem.endSegmentIdx;
                item.endLinkIndex = lightBarItem.endLinkIndex;
                item.endLinkStatus = lightBarItem.endLinkStatus;
                item.endLinkFineStatus = lightBarItem.endLinkFineStatus;
                item.startTrafficItem = formatTrafficItem(lightBarItem.startTrafficItem);
                item.start3dTrafficItem = formatTrafficItem(lightBarItem.start3dTrafficItem);
                item.endTrafficItem = formatTrafficItem(lightBarItem.endTrafficItem);
                item.end3dTrafficItem = formatTrafficItem(lightBarItem.end3dTrafficItem);
                items.add(item);
            }
            barInfo.itemList = items;
            list.add(barInfo);
        }
        naviTmcInfo.lightBarInfo = list;
        return naviTmcInfo;
    }

    private static NaviTmcInfo.NaviTrafficItem formatTrafficItem(TrafficItem trafficItem) {
        NaviTmcInfo.NaviTrafficItem naviTrafficItem = new NaviTmcInfo.NaviTrafficItem();
        naviTrafficItem.length = trafficItem.length;
        naviTrafficItem.travelTime = trafficItem.travelTime;
        naviTrafficItem.ratio = trafficItem.ratio;
        naviTrafficItem.startIndex = trafficItem.startIndex;
        naviTrafficItem.endIndex = trafficItem.endIndex;
        naviTrafficItem.status = trafficItem.status;
        naviTrafficItem.fineStatus = trafficItem.fineStatus;
        naviTrafficItem.speed = trafficItem.speed;
        naviTrafficItem.credibility = trafficItem.credibility;
        naviTrafficItem.reverse = trafficItem.reverse;
        naviTrafficItem.startPnt = new GeoPoint(trafficItem.startPnt.lon, trafficItem.startPnt.lat, trafficItem.startPnt.z);
        naviTrafficItem.endPnt = new GeoPoint(trafficItem.endPnt.lon, trafficItem.endPnt.lat, trafficItem.endPnt.z);
        return naviTrafficItem;
    }

    public static CrossImageEntity forMatImageInfo(CrossImageInfo info) {
        CrossImageEntity naviImageInfo = new CrossImageEntity();
        if (info != null) {
            naviImageInfo.setType(info.type);
            naviImageInfo.setVectorType(info.vectorType);
            naviImageInfo.setDataBuf(info.dataBuf);
            naviImageInfo.setArrowDataBuf(info.arrowDataBuf);
            naviImageInfo.setOnlyVector(info.isOnlyVector);
            naviImageInfo.setDistance(info.distance);
        }
        return naviImageInfo;
    }

    public static CrossImageEntity forMatImageInfo(int type) {
        CrossImageEntity naviImageInfo = new CrossImageEntity();
        naviImageInfo.setType(type);
        return naviImageInfo;
    }

    public static SpeedOverallEntity forMatNaviGreenWaveInfo(ArrayList<NaviGreenWaveCarSpeed> list) {
        SpeedOverallEntity speedOverallEntity = new SpeedOverallEntity();
        if (!ConvertUtils.isEmpty(list)) {
            NaviGreenWaveCarSpeed naviGreenWaveCarSpeed = list.get(0);
            speedOverallEntity.setType(naviGreenWaveCarSpeed.type);
            speedOverallEntity.setMaxSpeed(naviGreenWaveCarSpeed.maxSpeed);
            speedOverallEntity.setMinSpeed(naviGreenWaveCarSpeed.minSpeed);
            speedOverallEntity.setLightCount(naviGreenWaveCarSpeed.lightCount);
        }
        speedOverallEntity.setSpeedType(NaviConstant.SpeedType.SPEED_GREEN_WAVE);
        return speedOverallEntity;
    }

    public static SpeedOverallEntity forMatNaviSpeedCameraInfo(ArrayList<NaviIntervalCameraDynamicInfo> cameraDynamicList) {
        SpeedOverallEntity speedOverallEntity = new SpeedOverallEntity();
        if (!ConvertUtils.isEmpty(cameraDynamicList)) {
            NaviIntervalCameraDynamicInfo info = cameraDynamicList.get(0);
            speedOverallEntity.setLimitSpeedList(info.speed);
            speedOverallEntity.setAverageSpeed(info.averageSpeed);
            speedOverallEntity.setRemainDistance(info.remainDistance);
            speedOverallEntity.setDistance(info.distance);
        }
        speedOverallEntity.setSpeedType(NaviConstant.SpeedType.SPEED_OVERALL);
        return speedOverallEntity;
    }

    public static CameraInfoEntity forMatNaviCameraInfo(ArrayList<NaviCameraExt> naviCameraList) {
        CameraInfoEntity cameraInfoEntity = new CameraInfoEntity();
        if (!ConvertUtils.isEmpty(naviCameraList)) {
            loop:
            for (NaviCameraExt cameraInfo : naviCameraList) {
                if (cameraInfo.subCameras == null || cameraInfo.subCameras.isEmpty()) {
                    continue;
                }
                if (((cameraInfo.roadClass == RoadClass.RoadClassFreeway || cameraInfo.roadClass == RoadClass.RoadClassCitySpeedway) &&
                        cameraInfo.distance <= 1000) || cameraInfo.distance <= 500) { // 高速路、城快 < 1KM 或者 普通道路 < 500M
                    NaviSubCameraExt subCameraExt = cameraInfo.subCameras.get(0);
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

    private static boolean isValidSpeed(int speed) {
        return 0 < speed && speed < 0xff;
    }

    public static SapaInfoEntity forMatSAPAInfo(SAPAInquireResponseData responseData) {
        SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
        sapaInfoEntity.setRemainFreewayDistance(responseData.serviceAreaInfo.remainFreewayDistance);
        sapaInfoEntity.setRemainFreewayTime(responseData.serviceAreaInfo.remainFreewayTime);
        sapaInfoEntity.setRemainServiceAreaNum(responseData.serviceAreaInfo.remainServiceAreaNum);
        if (!ConvertUtils.isEmpty(responseData.serviceAreaInfo.serviceAreaList)) {
            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = new ArrayList<>();
            for (NaviFacility naviFacility : responseData.serviceAreaInfo.serviceAreaList) {
                sapaItems.add(getSAPAItem(naviFacility));
            }
            sapaInfoEntity.setList(sapaItems);
        }
        return sapaInfoEntity;
    }

    public static SapaInfoEntity forMatSAPAInfo(ArrayList<NaviFacility> list) {
        SapaInfoEntity sapaInfoEntity = new SapaInfoEntity();
        if (!ConvertUtils.isEmpty(list)) {
            int size = list.size();
            ArrayList<SapaInfoEntity.SAPAItem> sapaItems = new ArrayList<>();
            NaviFacility first = list.get(0);
            if (first.type == NaviFacilityType.NaviFacilityTypeServiceArea) {//服务区
                sapaInfoEntity.setType(NaviConstant.SapaItemsType.SPAS_LIST);
            } else if (first.type == NaviFacilityType.NaviFacilityTypeTollGate) {//收费站
                sapaInfoEntity.setType(NaviConstant.SapaItemsType.TOLL_STATION_LIST);
            }
            sapaItems.add(getSAPAItem(first));
            if (size > 1) {
                NaviFacility second = list.get(1);
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
        return sapaInfoEntity;
    }

    public static SapaInfoEntity.SAPAItem getSAPAItem(NaviFacility naviFacility) {
        SapaInfoEntity.SAPAItem sapaItem = new SapaInfoEntity.SAPAItem();
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

    public static CrossImageInfo getCrossImageInfo(CrossImageEntity crossInfo) {
        CrossImageInfo crossImageInfo = new CrossImageInfo();
        switch (crossInfo.getType()) {
            case NaviConstant.CrossType.CrossTypeGrid:
                crossImageInfo.type = CrossType.CrossTypeGrid;
                break;
            case NaviConstant.CrossType.CrossTypeVector:
                crossImageInfo.type = CrossType.CrossTypeVector;
                break;
            case NaviConstant.CrossType.CrossType3D:
                crossImageInfo.type = CrossType.CrossType3D;
                break;
        }
        switch (crossInfo.getVectorType()) {
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeInvalid:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeInvalid;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeCommon:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeCommon;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeRoundabout:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeRoundabout;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeConfusion:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeConfusion;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeNear:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeNear;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeDoubleLight:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeDoubleLight;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeSolidLine:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeSolidLine;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeSolidNear:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeSolidNear;
                break;
            case NaviConstant.VectorCrossImageType.VectorCrossImageTypeMixReverse:
                crossImageInfo.vectorType = VectorCrossImageType.VectorCrossImageTypeMixReverse;
                break;
        }
        crossImageInfo.dataBuf = crossInfo.getDataBuf();
        crossImageInfo.arrowDataBuf = crossInfo.getArrowDataBuf();
        crossImageInfo.isOnlyVector = crossInfo.isOnlyVector();
        crossImageInfo.distance = crossInfo.getDistance();
        return crossImageInfo;
    }

    public static NaviViaEntity getNaviViaEntity(RouteParam routeParam, Object obj) {
        NaviViaEntity naviViaEntity = new NaviViaEntity();
        naviViaEntity.setName(routeParam.getName());
        naviViaEntity.setAddress(routeParam.getAddress());
        naviViaEntity.setRealPos(routeParam.getRealPos());
        naviViaEntity.setPid(routeParam.getPoiID());
        naviViaEntity.setChargeInfo(routeParam.getChargeInfo());
        if (obj != null) {
            if (obj instanceof NaviEtaInfo) {
                NaviEtaInfo naviEtaInfo = (NaviEtaInfo) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(naviEtaInfo.allTime));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppContext.mContext, naviEtaInfo.allDist, naviEtaInfo.allTime));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppContext.mContext, naviEtaInfo.allTime));
            } else if (obj instanceof NaviEtaInfo.NaviTimeAndDist) {
                NaviEtaInfo.NaviTimeAndDist timeAndDist = (NaviEtaInfo.NaviTimeAndDist) obj;
                naviViaEntity.setArriveDay(TimeUtils.getArriveDay(timeAndDist.time));
                naviViaEntity.setDistance(TimeUtils.getRemainInfo(AppContext.mContext, timeAndDist.dist, timeAndDist.time));
                naviViaEntity.setArriveTime(TimeUtils.getArriveTime(AppContext.mContext, timeAndDist.time));
            }
        }
        return naviViaEntity;
    }

    public static PoiInfoEntity getPoiInfoEntity(Object obj) {
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

    public static NaviParkingEntity getNaviParkingEntity(PoiInfoEntity poiInfoEntity, boolean isEndPoi) {
        String distance = poiInfoEntity.getDistance();
        String meter = AppContext.mContext.getString(com.android.utils.R.string.meter);
        String km = AppContext.mContext.getString(com.android.utils.R.string.km);
        NaviParkingEntity naviParkingEntity = new NaviParkingEntity()
                .setEndPoi(isEndPoi)
                .setName(poiInfoEntity.getName())
                .setAddress(poiInfoEntity.getAddress())
                .setPid(poiInfoEntity.getPid())
                .setPoiType(poiInfoEntity.getPoiType())
                .setDistance(distance)
                .setPoint(poiInfoEntity.getPoint());
        if (distance.contains(meter)) {
            naviParkingEntity.setSortDis(Double.parseDouble(distance.replace(meter, "").trim()));
        } else if (distance.contains(km)) {
            naviParkingEntity.setSortDis(Double.parseDouble(distance.replace(km, "").trim()) * 1000);
        }
        List<ParkingInfo> parkingInfoList = poiInfoEntity.getParkingInfoList();
        if (!ConvertUtils.isEmpty(parkingInfoList)) {
            ParkingInfo parkingInfo = parkingInfoList.get(0);
            int spaceTotal = parkingInfo.getSpaceTotal();
            int spaceFree = parkingInfo.getSpaceFree();
            if (spaceTotal != -1 && spaceFree != -1) {
                naviParkingEntity.setSpaceTotal(spaceTotal)
                        .setSpaceFree(spaceFree);
                //-停车位紧张：总车位数<=30个，剩余车位<30% ；总车位数>30个，剩余车位<10% 或 剩余车位少于10个。
                if ((spaceTotal <= 30 && ((double) (spaceFree / spaceTotal) < 0.3)) ||
                        (spaceTotal > 30 && (spaceFree < 10 || ((double) (spaceFree / spaceTotal) < 0.1)))) {
                    naviParkingEntity.setTag(AppContext.mContext.getString(com.android.utils.R.string.navi_parking_tight));
                } else {
                    naviParkingEntity.setTag(AppContext.mContext.getString(com.android.utils.R.string.navi_parking_adequate));
                }
                naviParkingEntity.setNum(String.valueOf(spaceFree / spaceTotal));
            } else {
                Logger.d("SceneNaviParkListImpl spaceTotal= " + spaceTotal + ",spaceFree= " + spaceFree);
            }
        }
        return naviParkingEntity;
    }

    public static List<NaviMixForkInfo> formaterMixForkList(List<MixForkInfo> mixForkInfos){
        return GsonUtils.fromJson2List(mixForkInfos, NaviMixForkInfo.class);
    }
}
