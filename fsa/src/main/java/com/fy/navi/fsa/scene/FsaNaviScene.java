package com.fy.navi.fsa.scene;


import android.util.Log;

import com.android.utils.gson.GsonUtils;
import com.android.utils.thread.ThreadManager;
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
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.MapType;
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
import java.util.Base64;
import java.util.Hashtable;
import java.util.List;
import java.util.concurrent.TimeUnit;

public final class FsaNaviScene {

    private final Hashtable<Integer, String> mFsaDataMap = new Hashtable<>();

    private NaviIntervalSpeedInfo mNaviIntervalSpeedInfo;
    private NaviEtaInfo mNaviEtaInfo;
    private NaviManeuverInfo mNaviManeuverInfo;
    private LaneLineInfo mLaneLineInfo;
    private float mDrivePercent;
    private float mCurrentSpeed;

    public static FsaNaviScene getInstance() {
        return FsaNaviSceneHolder.INSTANCE;
    }

    private static final class FsaNaviSceneHolder {
        private static final FsaNaviScene INSTANCE = new FsaNaviScene();
    }

    private FsaNaviScene() {

    }

    /**
     * 处理TBT信息，发送TBT数据.
     *
     * @param fsaService  MyFsaService.
     * @param naviETAInfo NaviEtaInfo.
     */
    public void updateTbtInfo(final MyFsaService fsaService, final NaviEtaInfo naviETAInfo) {
        if (null == naviETAInfo) {
            Log.e(FsaConstant.FSA_TAG, "tbtInfo is null");
            return;
        }
        mNaviEtaInfo = naviETAInfo;
        //根据已行驶里程和剩余距离计算已行驶百分比
        final int totalDistance = naviETAInfo.getAllDist() + naviETAInfo.driveDist;
        final float drivePercent = (naviETAInfo.driveDist / (float) totalDistance) * 100;
        mDrivePercent = drivePercent;
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_PASSED_PERCENT, String.valueOf(drivePercent));

        //当前道路名称
        if (!(null == naviETAInfo.getCurRouteName() || naviETAInfo.getCurRouteName().isEmpty())) {
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_CURRENT_ROAD, naviETAInfo.getCurRouteName());
        }

        //TBT信息
        final ArrayList<TurnInfo> laneInfoList = new ArrayList<>();
        final TurnInfo turnInfo = new TurnInfo();
        turnInfo.setRoadName(naviETAInfo.getCurRouteName());
        turnInfo.setPosition(new GeoPoint(121.48998888888889, 31.379458888888887));
        turnInfo.setNextRoadName(naviETAInfo.getNextRouteName());
        turnInfo.setDistanceToNextTurn(naviETAInfo.getNextDist());
        turnInfo.setTurnKind(convertTbtType(naviETAInfo.getCurManeuverID()));
        turnInfo.setStraight(naviETAInfo.getCurManeuverID() == FsaConstant.FsaTurnKind.ICON_CONTINUE);
        turnInfo.setRoadLevel(naviETAInfo.curRoadClass);
        if (null != naviETAInfo.NaviInfoData && naviETAInfo.NaviInfoData.size() > naviETAInfo.NaviInfoFlag) {
            final NaviEtaInfo.NaviInfoPanel naviInfoPanel = naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag);
            turnInfo.setRemainDistance(naviInfoPanel.segmentRemain.dist);
            turnInfo.setDirectionName(naviInfoPanel.nextRouteName);
        }
        laneInfoList.add(turnInfo);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_TBT_INFO, GsonUtils.toJson(laneInfoList));

        //剩余时间和距离
        final RemainInfo remainInfo = new RemainInfo();
        remainInfo.setRemainTime(naviETAInfo.getAllTime());
        remainInfo.setRemainDistance(naviETAInfo.getAllDist());
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_REMAIN_TIME_DISTANCE, GsonUtils.toJson(remainInfo));
    }

    /**
     * TBT类型转换.
     *
     * @param type TBT类型.
     * @return TBT类型.
     */
    private int convertTbtType(final int type) {
        return switch (type) {
            case 0x2 -> 7;
            case 0x3 -> 3;
            case 0x4 -> 8;
            case 0x5 -> 2;
            case 0x6 -> 6;
            case 0x7 -> 4;
            case 0x8 -> 5;
            case 0x9 -> 1;
            case 0xB -> 9;
            case 0xC -> 10;
            default -> 0;
        };
    }


    /**
     * 处理TMC信息，发送路况数据.
     *
     * @param fsaService  MyFsaService.
     * @param naviTmcInfo NaviTmcInfo.
     */
    public void updateTmcInfo(final MyFsaService fsaService, final NaviTmcInfo naviTmcInfo) {
        if (null == fsaService) {
            Log.e(FsaConstant.FSA_TAG, "FSA service is null");
            return;
        }
        if (null == naviTmcInfo || null == naviTmcInfo.getLightBarDetail() || null == naviTmcInfo.getLightBarDetail().getTmcInfoData()
                || naviTmcInfo.getLightBarDetail().getTmcInfoData().isEmpty()) {
            Log.e(FsaConstant.FSA_TAG, "naviTmcInfo is null");
        }

        final ArrayList<RoadCondition> roadConditionList = new ArrayList<>();
        for (NaviTmcInfo.NaviTmcInfoData tmcInfoData : naviTmcInfo.getLightBarDetail().getTmcInfoData()) {
            if (null == tmcInfoData) {
                continue;
            }
            final RoadCondition roadCondition = new RoadCondition();
            roadCondition.setRoadConditionType(tmcInfoData.getStatus());
            roadCondition.setTravelTime(tmcInfoData.getTravelTime());
            roadCondition.setDistance(tmcInfoData.getDistance());

            roadConditionList.add(roadCondition);
        }

        fsaService.sendEvent(FsaConstant.FsaFunction.ID_ROAD_CONDITION_INFO, GsonUtils.toJson(roadConditionList));
    }

    /**
     * 更新导航摄像头信息，根据type区分是否为区间测速.
     *
     * @param fsaService MyFsaService.
     * @param cameraInfo CameraInfoEntity.
     */
    public void updateNavigationCameraInfo(final MyFsaService fsaService, final CameraInfoEntity cameraInfo) {
        if (null == fsaService || null == cameraInfo) {
            Log.e(FsaConstant.FSA_TAG, "navigation camera is null");
            return;
        }

        final int subType = cameraInfo.getSubType();
        if (FsaConstant.FsaCameraType.INTERVAL_VELOCITY_START == subType
                || FsaConstant.FsaCameraType.INTERVAL_VELOCITY_END == subType
                || FsaConstant.FsaCameraType.INTERVAL_VELOCITY_START_END == subType) {
            Log.d(FsaConstant.FSA_TAG, "updateNavigationCameraInfo: cameraId = "
                    + cameraInfo.getCameraId() + ", subCameraId = " + cameraInfo.getSubCameraId()
                    + ", type = " + cameraInfo.getSubType()
                    + ", remainDistance = " + cameraInfo.getDistance()
                    + ", speedLimit = " + cameraInfo.getSpeed());
            //区间测速的电子眼
            final NaviIntervalSpeedInfo naviIntervalSpeedInfo = new NaviIntervalSpeedInfo();
            if (FsaConstant.FsaCameraType.INTERVAL_VELOCITY_START == subType) {
                naviIntervalSpeedInfo.setStatus(FsaConstant.FsaValue.ZERO);
                naviIntervalSpeedInfo.setShowType(FsaConstant.FsaValue.ZERO);
                naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
                naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            } else {
                naviIntervalSpeedInfo.setStatus(FsaConstant.FsaValue.ONE);
                naviIntervalSpeedInfo.setShowType(FsaConstant.FsaValue.TWO);
                naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
                naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            }
            final CameraInfo cameraInfo1 = new CameraInfo();
            cameraInfo1.setId((int) cameraInfo.getSubCameraId());
            cameraInfo1.setRemainDistance(cameraInfo.getDistance());
            cameraInfo1.setSpeedLimit(cameraInfo.getSpeed());
            if (mNaviIntervalSpeedInfo != null) {
                cameraInfo1.setAverageSpeed(mNaviIntervalSpeedInfo.getCameraInfo().getAverageSpeed());
            }
            naviIntervalSpeedInfo.setCameraInfo(cameraInfo1);
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_INTERVAL_SPEED_INFO, GsonUtils.toJson(naviIntervalSpeedInfo));
            mNaviIntervalSpeedInfo = naviIntervalSpeedInfo;
        } else {
            //其他电子眼
            final ForwardCameraInfo forwardCameraInfo = new ForwardCameraInfo();
            forwardCameraInfo.setType(subType);
            forwardCameraInfo.setRemainDistance(cameraInfo.getDistance());
            forwardCameraInfo.setSpeedLimit(cameraInfo.getSpeed());
            forwardCameraInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeed());
            final com.fy.navi.service.define.bean.GeoPoint geoPoint = cameraInfo.getCoord2D();
            if (null != geoPoint) {
                final GeoPoint point = new GeoPoint(geoPoint.getLon(), geoPoint.getLat());
                forwardCameraInfo.setPosition(point);
            }
            final ArrayList<ForwardCameraInfo> cameraList = new ArrayList<>();
            cameraList.add(forwardCameraInfo);
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_FORWARD_CAMERA, GsonUtils.toJson(cameraList));
        }

        final SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(cameraInfo.getSpeed());
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(cameraInfo.isMatch());
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    /**
     * 更新引导态区间测速信息.
     *
     * @param fsaService      MyFsaService.
     * @param speedCameraInfo SpeedOverallEntity.
     */
    public void updateIntervalCameraInfo(final MyFsaService fsaService, final SpeedOverallEntity speedCameraInfo) {
        if (null == fsaService || null == speedCameraInfo) {
            Log.e(FsaConstant.FSA_TAG, "interval camera is null");
            return;
        }
        final ArrayList<Short> limitSpeedList = speedCameraInfo.getLimitSpeedList();
        short limitSpeed = 0;
        if (limitSpeedList != null) {
            for (int i = 0; i < limitSpeedList.size(); i++) {
                final Short speed = limitSpeedList.get(i);
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
        final NaviIntervalSpeedInfo naviIntervalSpeedInfo = new NaviIntervalSpeedInfo();
        naviIntervalSpeedInfo.setStatus(FsaConstant.FsaValue.ZERO);
        naviIntervalSpeedInfo.setShowType(FsaConstant.FsaValue.ONE);
        final CameraInfo cameraInfo = new CameraInfo();
        cameraInfo.setId(speedCameraInfo.getId());
        cameraInfo.setType(speedCameraInfo.getType());
        cameraInfo.setRemainDistance(speedCameraInfo.getRemainDistance());
        cameraInfo.setSpeedLimit(limitSpeed);
        cameraInfo.setAverageSpeed(speedCameraInfo.getAverageSpeed());
        naviIntervalSpeedInfo.setCameraInfo(cameraInfo);
        naviIntervalSpeedInfo.setCurrentSpeed((int) mCurrentSpeed);
        naviIntervalSpeedInfo.setOverSpeed(mCurrentSpeed > limitSpeed);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_INTERVAL_SPEED_INFO, GsonUtils.toJson(naviIntervalSpeedInfo));
        mNaviIntervalSpeedInfo = naviIntervalSpeedInfo;
    }

    /**
     * 更新车道线信息.
     *
     * @param fsaService     MyFsaService.
     * @param laneInfoEntity LaneInfoEntity.
     * @param isShowLane     是否显示车道线
     */
    public void updateLaneLineInfo(final MyFsaService fsaService, final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
        Log.d(FsaConstant.FSA_TAG, "updateLaneLineInfo: isShowLane = " + isShowLane + " --------");
        if (!isShowLane) {
            final LaneLineInfo laneLineInfo = new LaneLineInfo();
            laneLineInfo.setShowType(FsaConstant.HIDE);
            mLaneLineInfo = laneLineInfo;
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_LANE_INFO, GsonUtils.toJson(laneLineInfo));
            return;
        }
        if (laneInfoEntity == null) {
            Log.d(FsaConstant.FSA_TAG, "updateLaneLineInfo: laneInfoEntity = null");
            return;
        }
        Log.d(FsaConstant.FSA_TAG, "updateLaneLineInfo: frontLane = " + laneInfoEntity.getFrontLane());
        Log.d(FsaConstant.FSA_TAG, "updateLaneLineInfo: backLane = " + laneInfoEntity.getBackLane());
        Log.d(FsaConstant.FSA_TAG, "updateLaneLineInfo: optimalLane = " + laneInfoEntity.getOptimalLane());
        final LaneLineInfo laneLineInfo = new LaneLineInfo();
        if (mLaneLineInfo == null || mLaneLineInfo.getShowType() == FsaConstant.HIDE) {
            laneLineInfo.setShowType(FsaConstant.SHOW);
        } else {
            laneLineInfo.setShowType(FsaConstant.UPDATE);
        }
        // 车道线详细信息
        final LaneInfo laneInfo = new LaneInfo();
        if (laneInfoEntity.getPoint() != null) {
            laneInfo.setPosition(new GeoPoint(laneInfoEntity.getPoint().getLon(), laneInfoEntity.getPoint().getLat()));
        }
//        laneInfo.setRemainDistance(); 仪表与hud目前暂未使用
        laneLineInfo.setLaneInfo(laneInfo);
        // 车道线信息
        final ArrayList<LaneItem> laneItemList = new ArrayList<>();
        laneInfo.setItemList(laneItemList);
        // 遍历背景车道列表
        for (int i = 0; i < laneInfoEntity.getBackLane().size(); i++) {
            final Integer backlaneType = laneInfoEntity.getBackLane().get(i);
            final Integer frontlaneType = laneInfoEntity.getFrontLane().get(i);
            if (backlaneType == 0xFF) {
                continue;
            }
            // 车道线信息
            final LaneItem laneItem = new LaneItem();
            laneItemList.add(laneItem);
            // 车道线数量变化类型
//            laneItem.setLaneVariationType();
            // 车道线类型信息
            final LaneTypeInfo laneTypeInfo = new LaneTypeInfo();
            laneTypeInfo.setLaneType(amapLane2fsa(backlaneType));
            if (laneInfoEntity.getOptimalLane().get(i) != 0xFF) {
                laneTypeInfo.setBright(true);
            }
            laneItem.setLaneTypeInfo(laneTypeInfo);
            // 车道线方向信息
            final ArrayList<LaneDirection> directionList = new ArrayList<>();
            laneItem.setDirectionList(directionList);
            amapLaneDirection2fsa(frontlaneType, backlaneType, directionList);
        }
        mLaneLineInfo = laneLineInfo;
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_LANE_INFO, GsonUtils.toJson(laneLineInfo));
    }

    /**
     * 车道线类型转换.
     *
     * @param type int.
     * @return int.
     */
    private int amapLane2fsa(final int type) {
        switch (type) {
            case 21: // 公交车道
                return FsaConstant.FsaLaneType.BUS_LANE;
            case 22: // 空车道
                return FsaConstant.FsaLaneType.NORMAL;
            case 23: // 可变车道
                return FsaConstant.FsaLaneType.VARIABLE_LANE;
            case 24: // 专用车道
                return FsaConstant.FsaLaneType.TEXT_BUS_LANE;
            case 25: // 潮汐车道
                return FsaConstant.FsaLaneType.TEXT_TIDAL_LANE;
            case 13:
            case 14:
            case 15:
            case 0xFF:
                return FsaConstant.FsaLaneType.INVALID_VALUE;
            default:
                return FsaConstant.FsaLaneType.NORMAL;
        }
    }

    /**
     * 车道线方向转换.
     *
     * @param frontType     int.
     * @param backType      int.
     * @param directionList ArrayList.
     */
    private void amapLaneDirection2fsa(final int frontType, final int backType,
                                       final ArrayList<LaneDirection> directionList) {
        switch (backType) {
            case 0, 10:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                break;
            case 1, 20:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                break;
            case 2:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                break;
            case 3, 12:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                break;
            case 4:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                break;
            case 5:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 0 ? 1 : 2));
                break;
            case 6:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                break;
            case 7:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                break;
            case 9:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 11:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 17:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 16:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 18:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_LEFT, frontType == 1 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 19:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.GO_STRAIGHT, frontType == 0 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_RIGHT, frontType == 3 ? 1 : 2));
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.TURN_AROUND, frontType == 5 ? 1 : 2));
                break;
            case 21:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.INVALID_VALUE, frontType == 21 ? 1 : 2));
                break;
            case 22:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.INVALID_VALUE, frontType == 22 ? 1 : 2));
                break;
            case 23:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.INVALID_VALUE, frontType == 23 ? 1 : 2));
                break;
            case 24:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.INVALID_VALUE, frontType == 24 ? 1 : 2));
                break;
            case 8: // 右掉头 ？
            default:
                directionList.add(new LaneDirection(FsaConstant.FsaLaneDirection.INVALID_VALUE, 0));
                break;
        }
    }

    /**
     * 更新巡航态前方限速信息信息.
     *
     * @param fsaService       MyFsaService.
     * @param cruiseInfoEntity CruiseInfoEntity.
     */
    public void updateCruiseInfoEntity(final MyFsaService fsaService, final CruiseInfoEntity cruiseInfoEntity) {
        if (null == fsaService || null == cruiseInfoEntity) {
            Log.e(FsaConstant.FSA_TAG, "cruiseInfoEntity is null");
            return;
        }

        final SpeedLimitCruiseInfo speedLimitCruiseInfo = new SpeedLimitCruiseInfo();
        final ArrayList<Short> speedList = cruiseInfoEntity.getSpeed();
        if (speedList != null) {
            if (!speedList.isEmpty()) {
                speedLimitCruiseInfo.setShowType(FsaConstant.FsaValue.ZERO);
            } else {
                speedLimitCruiseInfo.setShowType(FsaConstant.FsaValue.TWO);
            }
        } else {
            speedLimitCruiseInfo.setShowType(FsaConstant.FsaValue.TWO);
        }
        final CameraInfo cameraInfo = new CameraInfo();
//        cameraInfo.setId();
//        cameraInfo.setType();
//        cameraInfo.setRemainDistance();
        if (speedList != null) {
            for (int i = 0; i < cruiseInfoEntity.getSpeed().size(); i++) {
                final Short speed = cruiseInfoEntity.getSpeed().get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    cameraInfo.setSpeedLimit(speed);
                    break;
                }
            }
        }
        speedLimitCruiseInfo.setCameraInfo(cameraInfo);
        speedLimitCruiseInfo.setCurrentSpeed((int) mCurrentSpeed);
        speedLimitCruiseInfo.setOverSpeed(mCurrentSpeed > cameraInfo.getSpeedLimit());
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_CRUISE_SPEED_LIMIT, GsonUtils.toJson(speedLimitCruiseInfo));

        final SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(cameraInfo.getSpeedLimit());
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(true);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    /**
     * 更新放大图信息.
     *
     * @param fsaService    MyFsaService.
     * @param naviImageInfo CrossImageEntity.
     * @param isShowImage   boolean.
     */
    public void updateEnlargeMap(final MyFsaService fsaService, final boolean isShowImage, final CrossImageEntity naviImageInfo) {
        Log.d(FsaConstant.FSA_TAG, "updateEnlargeMap: isShowImage = " + isShowImage);
        if (!isShowImage) {
            final EnlargeMap enlargeMap = new EnlargeMap();
            enlargeMap.setStatus(FsaConstant.HIDE);
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_ENLARGE_ICON, GsonUtils.toJson(enlargeMap));
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP, GsonUtils.toJson(enlargeMap));
            return;
        }

        if (null == naviImageInfo) {
            Log.e(FsaConstant.FSA_TAG, "naviImageInfo is null");
            return;
        }
        ThreadManager.getInstance().asyncDelay(() -> {
            final EnlargeMap enlargeMap = new EnlargeMap();
            enlargeMap.setStatus(FsaConstant.SHOW);
            enlargeMap.setType(1);
            final JunctionViewInfo junctionViewInfo = new JunctionViewInfo();
            junctionViewInfo.setRemainDistance((int) naviImageInfo.getDistance());
            junctionViewInfo.setProgressRatio((int) mDrivePercent);
            if (mNaviEtaInfo != null) {
                junctionViewInfo.setNextRoadName(mNaviEtaInfo.getNextRouteName());
            }
            junctionViewInfo.setImageMimeType("image/jpeg");
            junctionViewInfo.setImageBytes(Base64.getEncoder().encodeToString(fsaService.getmCrossImg()));
            enlargeMap.setJunctionViewInfo(junctionViewInfo);
            Log.d(FsaConstant.FSA_TAG, "updateEnlargeMap: " + enlargeMap);
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_ENLARGE_ICON, GsonUtils.toJson(enlargeMap));
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_HUD_ENLARGE_MAP, GsonUtils.toJson(enlargeMap));
        }, 700, TimeUnit.MILLISECONDS);

    }

    /**
     * 更新高速服务区信息.
     *
     * @param fsaService     MyFsaService.
     * @param sapaInfoEntity SapaInfoEntity.
     */
    public void updateHighwayService(final MyFsaService fsaService, final SapaInfoEntity sapaInfoEntity) {
        if (null == fsaService || null == sapaInfoEntity) {
            Log.e(FsaConstant.FSA_TAG, "sapaInfoEntity is null");
            return;
        }
        sendHighWaySubscribeInfos(fsaService, sapaInfoEntity);
        sendHighwayTotalInfo(fsaService, sapaInfoEntity);
        final ArrayList<SapaInfoEntity.SAPAItem> list = sapaInfoEntity.getList();
        final ArrayList<ServiceAreaInfo> serviceAreaInfos = new ArrayList<>();
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(i);
                if (i == 5) {
                    return;
                }
                if (sapaItem.getType() == 0) { // 服务区
                    final ServiceAreaInfo serviceAreaInfo = new ServiceAreaInfo();
                    serviceAreaInfo.setLocation(new GeoPoint(sapaItem.getPos().getLon(), sapaItem.getPos().getLat()));
                    serviceAreaInfo.setName(sapaItem.getName());
                    serviceAreaInfos.add(serviceAreaInfo);
                }
            }
        }
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_SERVICE_POI, GsonUtils.toJson(serviceAreaInfos));
    }

    /**
     * 更新服务区信息.
     *
     * @param fsaService     MyFsaService.
     * @param sapaInfoEntity SapaInfoEntity.
     */
    private void sendHighWaySubscribeInfos(final MyFsaService fsaService, final SapaInfoEntity sapaInfoEntity) {
        final HighwayService highwayService = new HighwayService();
        highwayService.setType(0);
        final ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos = new ArrayList<>();
        final ArrayList<SapaInfoEntity.SAPAItem> list = sapaInfoEntity.getList();
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(i);
                final HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
                if (sapaItem.getType() == 0) { // 服务区
                    highwaySubscribeInfo.setType(4);
                    final HighWayServiceAreaInfo serviceAreaInfo = new HighWayServiceAreaInfo();
                    serviceAreaInfo.setName(sapaItem.getName());
                    serviceAreaInfo.setDuration((int) sapaItem.getRemainTime());
                    serviceAreaInfo.setDistance(sapaItem.getRemainDist());
                    serviceAreaInfo.setPosition(new GeoPoint(sapaItem.getPos().getLon(), sapaItem.getPos().getLat()));
                    serviceAreaInfo.setServiceTypes((int) sapaItem.getSapaDetail()); // TODO 未转换
                    highwaySubscribeInfo.setServiceAreaInfo(serviceAreaInfo);
                } else if (sapaItem.getType() == 1) { // 收费站
                    highwaySubscribeInfo.setType(1);
                    final TollStation tollStation = new TollStation();
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
            final HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
            highwaySubscribeInfo.setType(3);
            final HighWayExitInfo highwayExitInfo = new HighWayExitInfo();
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

            final HighwaySubscribeInfo highwaySubscribeInfo1 = new HighwaySubscribeInfo();
            highwaySubscribeInfo.setType(2);
            final HighWayEntranceInfo highWayEntranceInfo = new HighWayEntranceInfo();
            if (mNaviManeuverInfo.getEntranceExit() != null) {
                highWayEntranceInfo.setRoadName(mNaviManeuverInfo.getEntranceExit());
            }
            highwaySubscribeInfo1.setHighWayEntranceInfo(highWayEntranceInfo);
            highWaySubscribeInfos.add(highwaySubscribeInfo1);
        }
        highwayService.setHighWaySubscribeInfos(highWaySubscribeInfos);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_SERVICE_AREA, GsonUtils.toJson(highwayService));
    }

    /**
     * 更新服务区信息.
     *
     * @param fsaService     MyFsaService.
     * @param sapaInfoEntity SapaInfoEntity.
     */
    private void sendHighwayTotalInfo(final MyFsaService fsaService, final SapaInfoEntity sapaInfoEntity) {
        final HighwayService highwayService = new HighwayService();
        final ArrayList<SapaInfoEntity.SAPAItem> list = sapaInfoEntity.getList();
        final ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos = new ArrayList<>();
        highwayService.setType(1);
        final HighwayTotalInfo highwayTotalInfo = new HighwayTotalInfo();
        highwayTotalInfo.setShowType(0);
        final HighwayInfo highwayInfo = new HighwayInfo();
        highwayInfo.setCurHighwayRoadName(mNaviEtaInfo.getCurRouteName());
        if (mNaviManeuverInfo != null) {
            if (mNaviManeuverInfo.getExitNameInfo() != null && !mNaviManeuverInfo.getExitNameInfo().isEmpty()) {
                highwayInfo.setExitHighwayID(mNaviManeuverInfo.getExitNameInfo().get(0));
            }
            if (mNaviManeuverInfo.getDirectionInfo() != null && !mNaviManeuverInfo.getDirectionInfo().isEmpty()) {
                highwayInfo.setExitHighwayDirectName(mNaviManeuverInfo.getDirectionInfo().get(0));
            }
        }
        int tollGateRemainDist = -1;
        int serviceAreaRemainDist = -1;
        if (list != null) {
            for (int i = 0; i < sapaInfoEntity.getList().size(); i++) {
                final SapaInfoEntity.SAPAItem sapaItem = sapaInfoEntity.getList().get(i);
                final HighwaySubscribeInfo highwaySubscribeInfo = new HighwaySubscribeInfo();
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

        highwayService.setHighwayTotalInfo(highwayTotalInfo);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_SERVICE_AREA, GsonUtils.toJson(highwayService));
    }

    /**
     * 更新自车位置.
     *
     * @param fsaService   MyFsaService.
     * @param locationInfo LocInfoBean.
     */
    public void updateCurrentCarLocation(final MyFsaService fsaService, final LocInfoBean locationInfo) {
        final CurrentCarLocation currentCarLocation = new CurrentCarLocation();
        currentCarLocation.setLocation(new GeoPoint(locationInfo.getLongitude(), locationInfo.getLatitude()));
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_SELF_DRIVING_POSITION, GsonUtils.toJson(currentCarLocation));
    }

    /**
     * 更新hud服务详情位置.
     *
     * @param fsaService MyFsaService.
     */
    public void updateHudVideInfo(final MyFsaService fsaService) {
        final HudVideInfo hudVideInfo = new HudVideInfo();
        hudVideInfo.setWidth(328);
        hudVideInfo.setHeight(172);
        hudVideInfo.setFormat(1);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_HUD_SERVICE_INIT, GsonUtils.toJson(hudVideInfo));
    }

    /**
     * 更新续航里程
     *
     * @param fsaService          MyFsaService.
     * @param evRangeOnRouteInfos EvRangeOn
     */
    public void updateEvRangeOnRouteInfo(final MyFsaService fsaService, final ArrayList<EvRangeOnRouteInfo> evRangeOnRouteInfos) {
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_RANGE_ON_ROUTE, GsonUtils.toJson(evRangeOnRouteInfos));
    }

    /**
     * 周边充电站POI透出
     *
     * @param fsaService         MyFsaService.
     * @param searchResultEntity SearchResultEntity
     */
    public void updateChargingStationInfo(final MyFsaService fsaService, final SearchResultEntity searchResultEntity) {
        final List<PoiInfoEntity> poiList = searchResultEntity.getPoiList();
        if (poiList != null) {
            final ArrayList<ChargingStationInfo> chargingStationInfos = new ArrayList<>();
            for (int i = 0; i < poiList.size(); i++) {
                if (i == 5) {
                    return;
                }
                final PoiInfoEntity poiInfoEntity = poiList.get(i);
                final ChargingStationInfo chargingStationInfo = new ChargingStationInfo();
                chargingStationInfo.setLocation(new GeoPoint(poiInfoEntity.getPoint().getLon(), poiInfoEntity.getPoint().getLat()));
                chargingStationInfo.setName(poiInfoEntity.getName());
                chargingStationInfos.add(chargingStationInfo);
            }
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_CHARGING_STATIONS_POI, GsonUtils.toJson(chargingStationInfos));
        }
    }

    /**
     * 更新导航中目的地变更信息
     *
     * @param fsaService         MyFsaService.
     * @param requestRouteResult RequestRouteResult
     */
    public void updateDestInfo(final MyFsaService fsaService, final RequestRouteResult requestRouteResult) {
        final RouteParam routeParam = RoutePackage.getInstance().getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        if (null != routeParam) {
            final DestInfo destInfo = new DestInfo();
            destInfo.setName(routeParam.getName());
            destInfo.setAddress(routeParam.getAddress());
            if (null != routeParam.getRealPos()) {
                final GeoPoint location = new GeoPoint(routeParam.getRealPos().getLon(), routeParam.getRealPos().getLat());
                destInfo.setLocation(location);
            }
            fsaService.sendEvent(FsaConstant.FsaFunction.ID_CHANGE_DESTINATION, GsonUtils.toJson(destInfo));
        }
    }

    /**
     * 更新导航中目的地变更信息
     *
     * @param fsaService MyFsaService.
     * @param speed      int
     */
    public void updateSpeedLimitSignData(final MyFsaService fsaService, final int speed) {
        final SpeedLimitSignData speedLimitSignData = new SpeedLimitSignData();
        speedLimitSignData.setSpeedLimit(speed);
        speedLimitSignData.setAssured(true);
        speedLimitSignData.setMapMatch(false);
        fsaService.sendEvent(FsaConstant.FsaFunction.ID_WHOLE_SPEED_LIMIT, GsonUtils.toJson(speedLimitSignData));
    }

    /**
     * 更新当前车速
     *
     * @param speed float
     */
    public void updateCurrentSpeed(final float speed) {
        mCurrentSpeed = speed;
    }

    /**
     * 更新导航 maneuver 信息
     *
     * @param respData NaviManeuverInfo
     */
    public void updateNaviManeuverInfo(final NaviManeuverInfo respData) {
        mNaviManeuverInfo = respData;
    }

    /**
     * 保存数据
     *
     * @param functionId int
     * @param info       String
     */
    public void saveData(final int functionId, final String info) {
        mFsaDataMap.put(functionId, info);
    }

    /**
     * 获取数据
     *
     * @param functionId   int
     * @param defaultValue String
     * @return String
     */
    public String getData(final int functionId, final String defaultValue) {
        return mFsaDataMap.getOrDefault(functionId, defaultValue);
    }
}
