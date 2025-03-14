package com.fy.navi.fsa.scene;


import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.gson.GsonUtils;
import com.fy.navi.fsa.FsaConstant;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.fsa.bean.CameraInfo;
import com.fy.navi.fsa.bean.ChargingStationInfo;
import com.fy.navi.fsa.bean.CurrentCarLocation;
import com.fy.navi.fsa.bean.DestInfo;
import com.fy.navi.fsa.bean.EnlargeMap;
import com.fy.navi.fsa.bean.ForwardCameraInfo;
import com.fy.navi.fsa.bean.GeoPoint;
import com.fy.navi.fsa.bean.HighWayEntranceInfo;
import com.fy.navi.fsa.bean.HighWayExitInfo;
import com.fy.navi.fsa.bean.HighWayServiceAreaInfo;
import com.fy.navi.fsa.bean.HighwayInfo;
import com.fy.navi.fsa.bean.HighwayService;
import com.fy.navi.fsa.bean.HighwaySubscribeInfo;
import com.fy.navi.fsa.bean.HighwayTotalInfo;
import com.fy.navi.fsa.bean.HudVideInfo;
import com.fy.navi.fsa.bean.ILSImageViewInfo;
import com.fy.navi.fsa.bean.JunctionViewInfo;
import com.fy.navi.fsa.bean.LaneDirection;
import com.fy.navi.fsa.bean.LaneInfo;
import com.fy.navi.fsa.bean.LaneItem;
import com.fy.navi.fsa.bean.LaneLineInfo;
import com.fy.navi.fsa.bean.LaneTypeInfo;
import com.fy.navi.fsa.bean.NaviIntervalSpeedInfo;
import com.fy.navi.fsa.bean.RemainInfo;
import com.fy.navi.fsa.bean.RoadCondition;
import com.fy.navi.fsa.bean.ServiceAreaInfo;
import com.fy.navi.fsa.bean.SpeedLimitCruiseInfo;
import com.fy.navi.fsa.bean.SpeedLimitSignData;
import com.fy.navi.fsa.bean.TollStation;
import com.fy.navi.fsa.bean.TurnInfo;
import com.fy.navi.service.adapter.navi.NaviConstant;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.CrossImageEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviManeuverInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navi.SapaInfoEntity;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.EvRangeOnRouteInfo;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.logicpaket.route.RoutePackage;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Hashtable;
import java.util.List;

/**
 * FSA中处理导航数据数据.
 */
public class FsaNaviScene {

    private final Hashtable<Integer, String> mFsaDataMap = new Hashtable<>();

    private NaviIntervalSpeedInfo mNaviIntervalSpeedInfo;
    private NaviEtaInfo mNaviEtaInfo;
    private NaviManeuverInfo mNaviManeuverInfo;
    private float mDrivePercent;
    private float mCurrentSpeed;

    public static FsaNaviScene getInstance() {
        return FsaNaviSceneHolder.INSTANCE;
    }

    private static final class FsaNaviSceneHolder {
        private static final FsaNaviScene INSTANCE = new FsaNaviScene();
    }

    private FsaNaviScene() {}

    /**
     * 根据导航回调的TBT信息发送对应的FsaEvent给客户端.
     *
     * @param naviETAInfo NaviETAInfo.
     */
    public void updateTbtInfo(MyFsaService fsaService, NaviEtaInfo naviETAInfo) {
        if (null == naviETAInfo) {
            Log.e(FsaConstant.FSA_TAG, "tbtInfo is null");
            return;
        }
        mNaviEtaInfo = naviETAInfo;
        //根据已行驶里程和剩余距离计算已行驶百分比
        int totalDistance = naviETAInfo.allDist + naviETAInfo.driveDist;
        float drivePercent = (naviETAInfo.driveDist / (float) totalDistance) * 100;
        mDrivePercent = drivePercent;
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_PASSED_PERCENT, String.valueOf(drivePercent));

        //当前道路名称
        if (!(null == naviETAInfo.curRouteName || naviETAInfo.curRouteName.isEmpty())) {
            fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_CURRENT_ROAD, naviETAInfo.curRouteName);
        }

        //TBT信息
        ArrayList<TurnInfo> laneInfoList = new ArrayList<>();
        TurnInfo turnInfo = new TurnInfo();
        turnInfo.setRoadName(naviETAInfo.curRouteName);
        turnInfo.setNextRoadName(naviETAInfo.nextRouteName);
        turnInfo.setDistanceToNextTurn(naviETAInfo.nextDist);
        turnInfo.setTurnKind(naviETAInfo.curManeuverID);
        turnInfo.setStraight(naviETAInfo.curManeuverID == FsaConstant.FSA_TURN_KIND.IconContinue);
        turnInfo.setRoadLevel(naviETAInfo.curRoadClass);
        if (null != naviETAInfo.NaviInfoData && naviETAInfo.NaviInfoData.size() > naviETAInfo.NaviInfoFlag) {
            NaviEtaInfo.NaviInfoPanel naviInfoPanel = naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag);
            turnInfo.setRemainDistance(naviInfoPanel.segmentRemain.dist);
            turnInfo.setDirectionName(naviInfoPanel.nextRouteName);
        }
        laneInfoList.add(turnInfo);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_TBT_INFO, GsonUtils.toJson(laneInfoList));

        //剩余时间和距离
        RemainInfo remainInfo = new RemainInfo();
        remainInfo.setRemainTime(naviETAInfo.allTime);
        remainInfo.setRemainDistance(naviETAInfo.allDist);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_REMAIN_TIME_DISTANCE, GsonUtils.toJson(remainInfo));
    }


    /**
     * 处理TMC信息，发送路况数据.
     *
     * @param fsaService MyFsaService.
     * @param naviTmcInfo NaviTmcInfo.
     */
    public void updateTmcInfo(MyFsaService fsaService, NaviTmcInfo naviTmcInfo) {
        if (null == fsaService) {
            Log.e(FsaConstant.FSA_TAG, "FSA service is null");
            return;
        }
        if (null == naviTmcInfo || null == naviTmcInfo.lightBarDetail || null == naviTmcInfo.lightBarDetail.tmcInfoData
                || naviTmcInfo.lightBarDetail.tmcInfoData.isEmpty()) {
            Log.e(FsaConstant.FSA_TAG, "naviTmcInfo is null");
        }

        ArrayList<RoadCondition> roadConditionList = new ArrayList<>();
        for (NaviTmcInfo.NaviTmcInfoData tmcInfoData : naviTmcInfo.lightBarDetail.tmcInfoData) {
            if (null == tmcInfoData) {
                continue;
            }
            RoadCondition roadCondition = new RoadCondition();
            roadCondition.setRoadConditionType(tmcInfoData.status);
            roadCondition.setTravelTime(tmcInfoData.travelTime);
            roadCondition.setDistance(tmcInfoData.distance);

            roadConditionList.add(roadCondition);
        }

        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_ROAD_CONDITION_INFO, GsonUtils.toJson(roadConditionList));
    }

    /**
     * 更新导航摄像头信息，根据type区分是否为区间测速.
     *
     * @param fsaService MyFsaService.
     * @param cameraInfo CameraInfoEntity.
     */
    public void updateNavigationCameraInfo(MyFsaService fsaService, CameraInfoEntity cameraInfo) {
        if (null == fsaService || null == cameraInfo) {
            Log.e(FsaConstant.FSA_TAG, "navigation camera is null");
            return;
        }

        int subType = cameraInfo.getSubType();
        if (FsaConstant.FSA_CAMERA_TYPE.IntervalVelocityStart == subType
                || FsaConstant.FSA_CAMERA_TYPE.IntervalVelocityEnd == subType
                || FsaConstant.FSA_CAMERA_TYPE.IntervalVelocityStartEnd == subType) {
            Log.d(FsaConstant.FSA_TAG, "updateNavigationCameraInfo: cameraId = "
                    + cameraInfo.getCameraId() + ", subCameraId = " + cameraInfo.getSubCameraId()
                    + ", type = " + cameraInfo.getSubType()
                    + ", remainDistance = " + cameraInfo.getDistance()
                    + ", speedLimit = " + cameraInfo.getSpeed());
            //区间测速的电子眼
            NaviIntervalSpeedInfo naviIntervalSpeedInfo = new NaviIntervalSpeedInfo();
            if (FsaConstant.FSA_CAMERA_TYPE.IntervalVelocityStart == subType) {
                naviIntervalSpeedInfo.setStatus(FsaConstant.FSA_VALUE.ZERO);
                naviIntervalSpeedInfo.setShowType(FsaConstant.FSA_VALUE.ZERO);
                naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
                naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            } else {
                naviIntervalSpeedInfo.setStatus(FsaConstant.FSA_VALUE.ONE);
                naviIntervalSpeedInfo.setShowType(FsaConstant.FSA_VALUE.TWO);
                naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
                naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            }
            CameraInfo cameraInfo1 = new CameraInfo();
            cameraInfo1.setId((int)cameraInfo.getSubCameraId());
            cameraInfo1.setRemainDistance(cameraInfo.getDistance());
            cameraInfo1.setSpeedLimit(cameraInfo.getSpeed());
            if (mNaviIntervalSpeedInfo != null) {
                cameraInfo1.setAverageSpeed(mNaviIntervalSpeedInfo.getCameraInfo().getAverageSpeed());
            }
            naviIntervalSpeedInfo.setCameraInfo(cameraInfo1);
            fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_INTERVAL_SPEED_INFO, GsonUtils.toJson(naviIntervalSpeedInfo));
            mNaviIntervalSpeedInfo = naviIntervalSpeedInfo;
        } else {
            //其他电子眼
            ForwardCameraInfo forwardCameraInfo = new ForwardCameraInfo();
            forwardCameraInfo.setType(subType);
            forwardCameraInfo.setRemainDistance(cameraInfo.getDistance());
            forwardCameraInfo.setSpeedLimit(cameraInfo.getSpeed());
            forwardCameraInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            com.fy.navi.service.define.bean.GeoPoint geoPoint = cameraInfo.getCoord2D();
            if (null != geoPoint) {
                GeoPoint point = new GeoPoint(geoPoint.lon, geoPoint.lat);
                forwardCameraInfo.setPosition(point);
            }
            ArrayList<ForwardCameraInfo> cameraList = new ArrayList<>();
            cameraList.add(forwardCameraInfo);
            fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_FORWARD_CAMERA, GsonUtils.toJson(cameraList));
        }

        SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(cameraInfo.getSpeed());
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(cameraInfo.isMatch());
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    /**
     * 更新引导态区间测速信息.
     *
     * @param fsaService MyFsaService.
     * @param speedCameraInfo SpeedOverallEntity.
     */
    public void updateIntervalCameraInfo(MyFsaService fsaService, SpeedOverallEntity speedCameraInfo) {
        if (null == fsaService || null == speedCameraInfo) {
            Log.e(FsaConstant.FSA_TAG, "interval camera is null");
            return;
        }
        ArrayList<Short> limitSpeedList = speedCameraInfo.getLimitSpeedList();
        short limitSpeed = 0;
        if (limitSpeedList != null) {
            for (int i = 0; i < limitSpeedList.size(); i++) {
                Short speed = limitSpeedList.get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    limitSpeed = speed;
                    break;
                }
            }
        }
        Log.d(FsaConstant.FSA_TAG, "updateIntervalCameraInfo: id = "
                + speedCameraInfo.getId() + ", type = " + speedCameraInfo.getType()
                + ", remainDistance = " + speedCameraInfo.getRemainDistance()
                + ", speedLimit = " + limitSpeed
                + ", averageSpeed = " + speedCameraInfo.getAverageSpeed());
        NaviIntervalSpeedInfo naviIntervalSpeedInfo = new NaviIntervalSpeedInfo();
        naviIntervalSpeedInfo.setStatus(FsaConstant.FSA_VALUE.ZERO);
        naviIntervalSpeedInfo.setShowType(FsaConstant.FSA_VALUE.ONE);
        CameraInfo cameraInfo = new CameraInfo();
        cameraInfo.setId(speedCameraInfo.getId());
        cameraInfo.setType(speedCameraInfo.getType());
        cameraInfo.setRemainDistance(speedCameraInfo.getRemainDistance());
        cameraInfo.setSpeedLimit(limitSpeed);
        cameraInfo.setAverageSpeed(speedCameraInfo.getAverageSpeed());
        naviIntervalSpeedInfo.setCameraInfo(cameraInfo);
        naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
        naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > limitSpeed);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_INTERVAL_SPEED_INFO, GsonUtils.toJson(naviIntervalSpeedInfo));
        mNaviIntervalSpeedInfo = naviIntervalSpeedInfo;
    }

    /**
     * 更新车道线信息.
     *
     * @param fsaService MyFsaService.
     * @param laneInfoEntity LaneInfoEntity.
     */
    public void updateLaneLineInfo(MyFsaService fsaService, boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        if (null == fsaService || null == laneInfoEntity) {
            Log.e(FsaConstant.FSA_TAG, "laneInfoEntity is null");
            return;
        }

        LaneLineInfo laneLineInfo = new LaneLineInfo();
        laneLineInfo.setShowType(isShowLane ? FsaConstant.FSA_VALUE.ZERO : FsaConstant.FSA_VALUE.TWO);
        LaneInfo laneInfo = new LaneInfo();
        laneInfo.setPosition(new GeoPoint(laneInfoEntity.getPoint().getLon(), laneInfoEntity.getPoint().getLat()));
        laneLineInfo.setLaneInfo(laneInfo);
        ArrayList<LaneItem> laneItemList = new ArrayList<>();
        laneInfo.setItemList(laneItemList);
        for (int i = 0; i < laneInfoEntity.getBackLane().size(); i++) {
            Integer laneType = laneInfoEntity.getBackLane().get(i);
            if (laneType == 0xFF) {
                continue;
            }
            LaneItem laneItem = new LaneItem();
            LaneTypeInfo laneTypeInfo = new LaneTypeInfo();
            laneTypeInfo.setLaneType(amapLane2fsa(laneType));
            Integer optimalLane = laneInfoEntity.getOptimalLane().get(i);
            if (optimalLane != 0xFF) {
                laneTypeInfo.setBright(true);
            }
            laneItem.setLaneTypeInfo(laneTypeInfo);
            ArrayList<LaneDirection> directionList = new ArrayList<>();
            amapLaneDirection2fsa(laneType, directionList);
            laneItem.setDirectionList(directionList);
            laneItemList.add(laneItem);
        }
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_LANE_INFO, GsonUtils.toJson(laneLineInfo));
    }

    private int amapLane2fsa(int type) {
        switch (type) {
            case 21:
                return FsaConstant.FSA_LANE_TYPE.TEXT_BUS_LANE; // 2 or 3 ?
            case 22:
                return FsaConstant.FSA_LANE_TYPE.INVALID_VALUE; // ?
            case 23:
                return FsaConstant.FSA_LANE_TYPE.VARIABLE_LANE;
            case 24:
                return FsaConstant.FSA_LANE_TYPE.INVALID_VALUE; // ?
            case 25:
                return FsaConstant.FSA_LANE_TYPE.TEXT_TIDAL_LANE;
            default:
                return FsaConstant.FSA_LANE_TYPE.NORMAL;
        }
    }

    private void amapLaneDirection2fsa(int type, ArrayList<LaneDirection> directionList) {
        switch (type) {
            case 0:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                break;
            case 1:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                break;
            case 2:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                break;
            case 3:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                break;
            case 4:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                break;
            case 5:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 6:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                break;
            case 7:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                break;
            case 8:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 9:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 10:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 11:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 12:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 16:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 17:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 18:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 19:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.GO_STRAIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_RIGHT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            case 20:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_LEFT, 0));
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.TURN_AROUND, 0));
                break;
            default:
                directionList.add(new LaneDirection(FsaConstant.FSA_LANE_DIRECTION.INVALID_VALUE, 0));
                break;
        }
    }

    /**
     * 更新巡航态前方限速信息信息.
     *
     * @param fsaService MyFsaService.
     * @param cruiseInfoEntity CruiseInfoEntity.
     */
    public void updateCruiseInfoEntity(MyFsaService fsaService, CruiseInfoEntity cruiseInfoEntity) {
        if (null == fsaService || null == cruiseInfoEntity) {
            Log.e(FsaConstant.FSA_TAG, "cruiseInfoEntity is null");
            return;
        }

        SpeedLimitCruiseInfo speedLimitCruiseInfo = new SpeedLimitCruiseInfo();
        ArrayList<Short> speedList = cruiseInfoEntity.getSpeed();
        if (speedList != null) {
            if (!speedList.isEmpty()) {
                speedLimitCruiseInfo.setShowType(FsaConstant.FSA_VALUE.ZERO);
            } else {
                speedLimitCruiseInfo.setShowType(FsaConstant.FSA_VALUE.TWO);
            }
        } else {
            speedLimitCruiseInfo.setShowType(FsaConstant.FSA_VALUE.TWO);
        }
        CameraInfo cameraInfo = new CameraInfo();
//        cameraInfo.setId();
//        cameraInfo.setType();
//        cameraInfo.setRemainDistance();
        if (speedList != null) {
            for (int i = 0; i < cruiseInfoEntity.getSpeed().size(); i++) {
                Short speed = cruiseInfoEntity.getSpeed().get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    cameraInfo.setSpeedLimit(speed);
                    break;
                }
            }
        }
        speedLimitCruiseInfo.setCameraInfo(cameraInfo);
        speedLimitCruiseInfo.setCurrentSpeed((int) mCurrentSpeed);
        speedLimitCruiseInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeedLimit());
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_CRUISE_SPEED_LIMIT, GsonUtils.toJson(speedLimitCruiseInfo));

        SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(cameraInfo.getSpeedLimit());
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(true);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    /**
     * 更新放大图信息.
     *
     * @param fsaService MyFsaService.
     * @param naviImageInfo CrossImageEntity.
     */
    public void updateEnlargeMap(MyFsaService fsaService, boolean isShowImage, CrossImageEntity naviImageInfo) {
        if (null == fsaService || null == naviImageInfo) {
            Log.e(FsaConstant.FSA_TAG, "naviImageInfo is null");
            return;
        }

        EnlargeMap enlargeMap = new EnlargeMap();
        if (naviImageInfo.getType() == NaviConstant.CrossType.CrossTypeGrid) {
            enlargeMap.setType(0);
            ILSImageViewInfo ilsImageViewInfo = new ILSImageViewInfo();
            ilsImageViewInfo.setType(0);
            ilsImageViewInfo.setWidth(500);
            ilsImageViewInfo.setWidth(320);
            ilsImageViewInfo.setRemainDistance((int)naviImageInfo.getDistance());
            ilsImageViewInfo.setProgressRatio((int) mDrivePercent);
//            ilsImageViewInfo.setArrowMapName("");
//            ilsImageViewInfo.setBackgroundMapName("");
            if (mNaviEtaInfo != null) {
                ilsImageViewInfo.setNextRoadName(mNaviEtaInfo.nextRouteName);
            }
            ilsImageViewInfo.setBackgroundMimeType("image/jpeg");
            if (naviImageInfo.getDataBuf() != null) {
                ilsImageViewInfo.setBackgroundMapBytes(Base64.getEncoder().encodeToString(naviImageInfo.getDataBuf()));
            }
            ilsImageViewInfo.setArrowMimeType("image/png");
            if (naviImageInfo.getArrowDataBuf() != null) {
                ilsImageViewInfo.setArrowMapBytes(Base64.getEncoder().encodeToString(naviImageInfo.getArrowDataBuf()));
            }
            enlargeMap.setIlsImageViewInfo(ilsImageViewInfo);
        } else if (naviImageInfo.getType() == NaviConstant.CrossType.CrossTypeVector) {
            enlargeMap.setType(1);
            JunctionViewInfo junctionViewInfo = new JunctionViewInfo();
            junctionViewInfo.setRemainDistance((int)naviImageInfo.getDistance());
            junctionViewInfo.setProgressRatio((int) mDrivePercent);
            if (mNaviEtaInfo != null) {
                junctionViewInfo.setNextRoadName(mNaviEtaInfo.nextRouteName);
            }
            junctionViewInfo.setImageMimeType("image/jpeg");
            if (naviImageInfo.getDataBuf() != null) {
                junctionViewInfo.setImageBytes(Base64.getEncoder().encodeToString(naviImageInfo.getDataBuf()));
                enlargeMap.setStatus(FsaConstant.FSA_VALUE.ZERO);
            } else {
                enlargeMap.setStatus(FsaConstant.FSA_VALUE.TWO);
            }
            enlargeMap.setJunctionViewInfo(junctionViewInfo);
        }
        Log.d(FsaConstant.FSA_TAG, "updateEnlargeMap: " + enlargeMap);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_ENLARGE_ICON, GsonUtils.toJson(enlargeMap));
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_HUD_ENLARGE_MAP, GsonUtils.toJson(enlargeMap));
    }

    /**
     * 更新高速服务区信息.
     *
     * @param fsaService MyFsaService.
     * @param sapaInfoEntity SapaInfoEntity.
     */
    public void updateHighwayService(MyFsaService fsaService, SapaInfoEntity sapaInfoEntity) {
        if (null == fsaService || null == sapaInfoEntity) {
            Log.e(FsaConstant.FSA_TAG, "sapaInfoEntity is null");
            return;
        }

        HighwayService highwayService = new HighwayService();
        highwayService.setType(0);
        ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos = new ArrayList<>();
        ArrayList<SapaInfoEntity.SAPAItem> list = sapaInfoEntity.getList();
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                SapaInfoEntity.SAPAItem sapaItem =  sapaInfoEntity.getList().get(i);
                HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
                if (sapaItem.getType() == 0) { // 服务区
                    highwaySubscribeInfo.setType(4);
                    HighWayServiceAreaInfo serviceAreaInfo = new HighWayServiceAreaInfo();
                    serviceAreaInfo.setName(sapaItem.getName());
                    serviceAreaInfo.setDuration((int)sapaItem.getRemainTime());
                    serviceAreaInfo.setDistance(sapaItem.getRemainDist());
                    serviceAreaInfo.setPosition(new GeoPoint(sapaItem.getPos().getLon(), sapaItem.getPos().getLat()));
                    serviceAreaInfo.setServiceTypes((int)sapaItem.getSapaDetail()); // TODO 未转换
                    highwaySubscribeInfo.setServiceAreaInfo(serviceAreaInfo);
                } else if (sapaItem.getType() == 1) { // 收费站
                    highwaySubscribeInfo.setType(1);
                    TollStation tollStation = new TollStation();
                    tollStation.setName(sapaItem.getName());
                    tollStation.setDistance(sapaItem.getRemainDist());
                    highwaySubscribeInfo.setTollStation(tollStation);
                } else {
                    continue;
                }
                highWaySubscribeInfos.add(highwaySubscribeInfo);
            }
        }
        if (mNaviManeuverInfo != null) {
            HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
            highwaySubscribeInfo.setType(3);
            HighWayExitInfo highwayExitInfo = new HighWayExitInfo();
            if (mNaviManeuverInfo.getExitNameInfo() != null && !mNaviManeuverInfo.getExitNameInfo().isEmpty()) {
                highwayExitInfo.setExitRoadName(mNaviManeuverInfo.getExitNameInfo().get(0));
            }
            if (mNaviManeuverInfo.getEntranceExit() != null) {
                highwayExitInfo.setExitRoadName(mNaviManeuverInfo.getEntranceExit());
            }
            if (mNaviManeuverInfo.getDirectionInfo() != null && !mNaviManeuverInfo.getDirectionInfo().isEmpty()) {
                highwayExitInfo.setExitDirectionName(mNaviManeuverInfo.getDirectionInfo().get(0));
            }
            highwaySubscribeInfo.setHighWayExitInfo(highwayExitInfo);
            highWaySubscribeInfos.add(highwaySubscribeInfo);

            HighwaySubscribeInfo highwaySubscribeInfo1 = new HighwaySubscribeInfo();
            highwaySubscribeInfo.setType(2);
            HighWayEntranceInfo highWayEntranceInfo = new HighWayEntranceInfo();
            if (mNaviManeuverInfo.getEntranceExit() != null) {
                highWayEntranceInfo.setRoadName(mNaviManeuverInfo.getEntranceExit());
            }
            highwaySubscribeInfo1.setHighWayEntranceInfo(highWayEntranceInfo);
            highWaySubscribeInfos.add(highwaySubscribeInfo1);
        }
        highwayService.setHighWaySubscribeInfos(highWaySubscribeInfos);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_SERVICE_AREA, GsonUtils.toJson(highwayService));

        HighwayService highwayService1 = new HighwayService();
        highwayService.setType(1);
        HighwayTotalInfo highwayTotalInfo = new HighwayTotalInfo();
        highwayTotalInfo.setShowType(0);
        HighwayInfo highwayInfo = new HighwayInfo();
        highwayInfo.setCurHighwayRoadName(mNaviEtaInfo.curRouteName);
        if (mNaviManeuverInfo != null) {
            if (mNaviManeuverInfo.getExitNameInfo() != null) {
                highwayInfo.setExitHighwayID(mNaviManeuverInfo.getExitNameInfo().get(0));
            }
            if (mNaviManeuverInfo.getDirectionInfo() != null) {
                highwayInfo.setExitHighwayDirectName(mNaviManeuverInfo.getDirectionInfo().get(0));
            }
        }
//        highwayInfo.setExitRemainDist();
//        highwayInfo.setExitHighwayNextRoadName();
//        highwayInfo.setNextGPRemainDist();
        int tollGateRemainDist = -1;
        int serviceAreaRemainDist = -1;
        int nextServiceAreaRemainDist = -1;
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(i);
                HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
                if (sapaItem.getType() == 0) { // 服务区
                    if (serviceAreaRemainDist == -1) {
                        serviceAreaRemainDist = sapaItem.getRemainDist();
                        continue;
                    }
                    if (sapaItem.getRemainDist() < serviceAreaRemainDist) {
                        serviceAreaRemainDist = sapaItem.getRemainDist();
                        highwayInfo.setServiceAreaName(sapaItem.getName());
                        highwayInfo.setNextServiceAreaName(sapaItem.getName());
                        highwayInfo.setServiceAreaRemainDist(sapaItem.getRemainDist());
                        highwayInfo.setNextServiceAreaRemainDist(sapaItem.getRemainDist());
                    }
                } else if (sapaItem.getType() == 1) { // 收费站
                    if (tollGateRemainDist == -1) {
                        tollGateRemainDist = sapaItem.getRemainDist();
                        continue;
                    }
                    if (sapaItem.getRemainDist() < tollGateRemainDist) {
                        tollGateRemainDist = sapaItem.getRemainDist();
                        highwayInfo.setTollGateName(sapaItem.getName());
                        highwayInfo.setTollGateRemainDist(sapaItem.getRemainDist());
                    }
                } else {
                    continue;
                }
                highWaySubscribeInfos.add(highwaySubscribeInfo);
            }
        }

        highwayService1.setHighwayTotalInfo(highwayTotalInfo);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_SERVICE_AREA, GsonUtils.toJson(highwayService1));

        ArrayList<ServiceAreaInfo> serviceAreaInfos = new ArrayList<>();
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                SapaInfoEntity.SAPAItem sapaItem =  sapaInfoEntity.getList().get(i);
                if (i == 5) {
                    return;
                }
                if (sapaItem.getType() == 0) { // 服务区
                    ServiceAreaInfo serviceAreaInfo = new ServiceAreaInfo();
                    serviceAreaInfo.setLocation(new GeoPoint(sapaItem.getPos().getLon(), sapaItem.getPos().getLat()));
                    serviceAreaInfo.setName(sapaItem.getName());
                    serviceAreaInfos.add(serviceAreaInfo);
                }
            }
        }
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_SERVICE_POI, GsonUtils.toJson(serviceAreaInfos));
    }

    /**
     * 更新自车位置.
     *
     * @param fsaService MyFsaService.
     * @param locationInfo LocInfoBean.
     */
    public void updateCurrentCarLocation(MyFsaService fsaService, LocInfoBean locationInfo) {
        CurrentCarLocation currentCarLocation = new CurrentCarLocation();
        currentCarLocation.setLocation(new GeoPoint(locationInfo.getLongitude(), locationInfo.getLatitude()));
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_SELF_DRIVING_POSITION, GsonUtils.toJson(currentCarLocation));
    }

    /**
     * 更新hud服务详情位置.
     *
     * @param fsaService MyFsaService.
     */
    public void updateHudVideInfo(MyFsaService fsaService) {
        HudVideInfo hudVideInfo = new HudVideInfo();
        hudVideInfo.setWidth(328);
        hudVideInfo.setHeight(172);
        hudVideInfo.setFormat(1);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_HUD_SERVICE_INIT, GsonUtils.toJson(hudVideInfo));
    }

    /**
     * 更新续航里程
     *
     * @param fsaService MyFsaService.
     * @param evRangeOnRouteInfos ArrayList<EvRangeOnRouteInfo>.
     */
    public void updateEvRangeOnRouteInfo(MyFsaService fsaService, ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_RANGE_ON_ROUTE, GsonUtils.toJson(evRangeOnRouteInfos));
    }

    /**
     * 周边充电站POI透出
     *
     * @param fsaService MyFsaService.
     * @param searchResultEntity SearchResultEntity
     */
    public void updateChargingStationInfo(MyFsaService fsaService, SearchResultEntity searchResultEntity) {
        List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
        if (poiList != null) {
            ArrayList<ChargingStationInfo> chargingStationInfos = new ArrayList<>();
            for (int i = 0; i < poiList.size(); i++) {
                if (i == 5) {
                    return;
                }
                PoiInfoEntity poiInfoEntity =  poiList.get(i);
                ChargingStationInfo chargingStationInfo = new ChargingStationInfo();
                chargingStationInfo.setLocation(new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat()));
                chargingStationInfo.setName(poiInfoEntity.getName());
                chargingStationInfos.add(chargingStationInfo);
            }
            fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_CHARGING_STATIONS_POI, GsonUtils.toJson(chargingStationInfos));
        }
    }

    /**
     * 更新导航中目的地变更信息
     *
     * @param fsaService MyFsaService.
     * @param requestRouteResult RequestRouteResult
     */
    public void updateDestInfo(MyFsaService fsaService, RequestRouteResult requestRouteResult) {
        RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        if (null != routeParam) {
            DestInfo destInfo = new DestInfo();
            destInfo.setName(routeParam.name);
            destInfo.setAddress(routeParam.address);
            if (null != routeParam.getRealPos()) {
                GeoPoint location = new GeoPoint(routeParam.getRealPos().lat, routeParam.getRealPos().lon);
                destInfo.setLocation(location);
            }
            fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_CHANGE_DESTINATION, GsonUtils.toJson(destInfo));
        }
    }

    /**
     * 更新导航中目的地变更信息
     *
     * @param fsaService MyFsaService.
     * @param speed int
     */
    public void updateSpeedLimitSignData(MyFsaService fsaService, int speed) {
        SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(speed);
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(false);
        fsaService.sendEvent(FsaConstant.FSA_FUNCTION.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    public void  updateCurrentSpeed(float speed) {
        mCurrentSpeed = speed;
    }

    public void updateNaviManeuverInfo(NaviManeuverInfo respData) {
        mNaviManeuverInfo = respData;
    }

    public void saveData(int functionId, String info) {
        mFsaDataMap.put(functionId, info);
    }

    public String getData(int functionId, String defaultValue) {
        return mFsaDataMap.getOrDefault(functionId, defaultValue);
    }
}
