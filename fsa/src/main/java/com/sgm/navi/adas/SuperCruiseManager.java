package com.sgm.navi.adas;

import com.android.utils.NetWorkUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.JsonLog;
import com.android.utils.log.Logger;
import com.gm.cn.adassdk.AdasDataStateListener;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.proto.NaviLinkProto;
import com.gm.cn.adassdk.proto.RouteInfo;
import com.google.protobuf.InvalidProtocolBufferException;
import com.google.protobuf.util.JsonFormat;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.CameraInfoEntity;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.SpeedOverallEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.RequestRouteResult;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.define.route.ScSegmentInfo;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.l2.L2Package;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;

import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

public final class SuperCruiseManager {
    private static final String TAG = SuperCruiseManager.class.getSimpleName();

    private NaviLinkProto.RoadInfo.Builder mRoadInfoBuilder;
    private NaviLinkProto.SpeedLimit.Builder mSpeedLimitBuilder;
    private AdasManager mAdasManager;
    private ScheduledExecutorService mScheduler;
    private ScheduledFuture<?> mLinkScheduledFuture;
    private ScheduledFuture<?> mRouteScheduledFuture;
    private boolean mInitialized = false;
    private int mRoadSpeedLimit = 0;
    private int mEleEyeSpeedLimit = 0;
    private int mZoneSpeedLimit = 0;
    private int mCruiseSpeedLimit = 0;
    private String mNaviStatus = "";

    private RouteInfo.RouteProto.Builder mRouteProtoBuilder;
    private RouteInfo.Route.Builder mRouteBuilder;
    private List<RouteInfo.Navlaneguidance> mNavlaneguidances = new ArrayList<>();
    private List<RouteInfo.Maneuver> mManeuvers = new ArrayList<>();
    private List<RouteInfo.Destination> mDestinations = new ArrayList<>();
    private List<RouteInfo.Segment> mSegments = new ArrayList<>();
    private long mRouteLength = 10000;
    private long mRate = 5;
    private boolean mReRouting;
    private long mPathId = -1;
    private NaviEtaInfo mNaviEtaInfo;

    //region 单例
    public static SuperCruiseManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final SuperCruiseManager INSTANCE = new SuperCruiseManager();
    }

    private SuperCruiseManager() {
    }
    //endregion

    //region 初始化
    public void init(final AdasManager adasManager) {
        if (CalibrationPackage.getInstance().adasConfigurationType() != 7) {
            Logger.i(TAG, "not GB Arch ACP3.1 configuration");
            return;
        }
        if (mInitialized) {
            Logger.w(TAG, "initialized");
            return;
        }
        Logger.i(TAG, "init start");
        mRoadInfoBuilder = NaviLinkProto.RoadInfo.newBuilder();
        mSpeedLimitBuilder = NaviLinkProto.SpeedLimit.newBuilder();
        mRouteProtoBuilder = RouteInfo.RouteProto.newBuilder();
        mRouteBuilder = RouteInfo.Route.newBuilder();

        mAdasManager = adasManager;
        initData();
        initScheduler();
        initObserver();
        mInitialized = true;
        Logger.i(TAG, "init end");
    }

    public void uninit() {
        if (!mInitialized) {
            Logger.w(TAG, "not initialized");
            return;
        }
        Logger.i(TAG, "uninit");
        NaviPackage.getInstance().unregisterObserver(TAG);
        CruisePackage.getInstance().unregisterObserver(TAG);
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mLinkScheduledFuture.cancel(true);
        mRouteScheduledFuture.cancel(true);
        mInitialized = false;
    }

    private void initData() {
        // 导航状态
        mNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        // 车道数量
        mRoadInfoBuilder.setLaneCount(0);
        // 推荐限速
        mSpeedLimitBuilder.setRecommendedSpeedLimit(0);
        // 限速值单位
        mSpeedLimitBuilder.setSpeedLimitUnit(NaviLinkProto.SpeedLimit.SpeedLimitUnitEnum.KM_PER_HOUR);
        // 国家代号
        mRoadInfoBuilder.setCountryCode(NaviLinkProto.RoadInfo.CountryCodeEnum.CN);
        // 条件限速
        mSpeedLimitBuilder.setConditionalSpeedLimit(0);
        // 条件限速
        mSpeedLimitBuilder.setConditionalSpeedCategory(NaviLinkProto.SpeedLimit.ConditionalSpeedCategoryEnum.CATEGORY_UNKNOWN);
        // 条件限速类型
        mSpeedLimitBuilder.setConditionalSpeedType(NaviLinkProto.SpeedLimit.ConditionalSpeedTypeEnum.OTHER_UNKNOWN);
        // China and China Taiwan, navi should send 1
        // Hongkong and Aomen, navi should send 0
        mRoadInfoBuilder.setDrivingSideCategory(NaviLinkProto.RoadInfo.DrivingSideCategoryEnum.NAVILINK_DRIVING_SIDE_LEFT);
        // 地图数据的日期
        final Boolean networkCapabilities = NetWorkUtils.Companion.getInstance().checkNetwork();
        Logger.i(TAG, "initData: networkCapabilities = ", networkCapabilities);
        if (Boolean.TRUE.equals(networkCapabilities)) {
            setOnlineMapVersion();
        } else {
            setOfflineMapVersion();
        }

        mRouteBuilder.setId("");
        mRouteBuilder.setLat(0);
        mRouteBuilder.setLon(0);
    }

    private void initObserver() {
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
        mAdasManager.addAdasDataListener(mAdasDataStateListener);
    }

    private void initScheduler() {
        mScheduler = Executors.newScheduledThreadPool(2);
        mLinkScheduledFuture = mScheduler.scheduleWithFixedDelay(mLinkTask, 0, 1, TimeUnit.SECONDS);
//        mRouteScheduledFuture = mScheduler.scheduleWithFixedDelay(mRouteTask, 0, mRate, TimeUnit.SECONDS);
    }
    //endregion

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            mNaviStatus = naviStatus;
            if (!NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
                mNaviEtaInfo = null;
                mPathId = -1;
            }
        }
    };

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
            if (!isShowLane || laneInfoEntity == null) {
                return;
            }
            mNavlaneguidances.clear();
            RouteInfo.Navlaneguidance.Builder builder = RouteInfo.Navlaneguidance.newBuilder();
            int linkDist = L2Package.getInstance().getLinkDist(laneInfoEntity.getSegmentIdx(), laneInfoEntity.getLinkIdx());
            builder.setDist(linkDist); // 车道线距离
            GeoPoint linkLastGeoPoint = L2Package.getInstance().getLinkLastGeoPoint(laneInfoEntity.getSegmentIdx(), laneInfoEntity.getLinkIdx());
            if (linkLastGeoPoint != null) {
                builder.setLat(linkLastGeoPoint.getLat());
                builder.setLon(linkLastGeoPoint.getLon());
            }

            List<Integer> lanes = new ArrayList<>();
            List<RouteInfo.ManeuverType> types = new ArrayList<>();
            ArrayList<Integer> frontLane = laneInfoEntity.getFrontLane();
            builder.setLanecount(frontLane.size()); // 车道线数量
            for (int i = 0; i < frontLane.size(); i++) {
                Integer integer = frontLane.get(i);
                if (integer != NaviConstant.LaneAction.LANE_ACTION_NULL) {
                    lanes.add(i);
                }
                types.add(amapLaneToManeuverType(integer));
            }
            builder.addAllLanes(lanes);
            builder.addAllMType(types);
            mNavlaneguidances.add(builder.build());
        }

        //导航信息
        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            mNaviEtaInfo = naviETAInfo;
            if (naviETAInfo == null) {
                Logger.w(TAG, "onNaviInfo: null");
                mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.UNRECOGNIZED);
                mRoadInfoBuilder.setControlledAccess(false);
                return;
            }
            Logger.i(TAG, "onNaviInfo: ", naviETAInfo.curRoadClass);
            switch (naviETAInfo.curRoadClass) {
                case 0: // 高速公路
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_HIGHWAY);
                    break;
                case 1: // 国道
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_SFREEWAY);
                    break;
                case 2: // 省道
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_FREEWAY);
                    break;
                case 3: // 县道
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_COLLECTOR);
                    break;
                case 4: // 乡公路
                case 5: // 县乡村内部道路
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_ALLEY);
                    break;
                case 6: // 城市快速路
                case 7: // 主要道路
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_ARTERIAL);
                    break;
                case 8: // 次要道路
                case 9: // 普通道路
                case 10: // 非导航道路
                default:
                    mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.ROAD_CATEGORY_LOCAL);

            }
            switch (naviETAInfo.curRoadClass) {
                case 0: // 高速公路
                case 6: // 城市快速路
                    mRoadInfoBuilder.setControlledAccess(true);
                    break;
                default:
                    mRoadInfoBuilder.setControlledAccess(false);
            }

            GeoPoint scGeoPoint = L2Package.getInstance().getScGeoPoint(naviETAInfo.curSegIdx, naviETAInfo.curLinkIdx, naviETAInfo.curPointIdx);
            if (scGeoPoint != null) {
                mRouteBuilder.setLat(scGeoPoint.getLat());
                mRouteBuilder.setLon(scGeoPoint.getLon());
            }
            mRouteBuilder.setId(String.valueOf(naviETAInfo.getPathID()));
            mManeuvers.clear();
            RouteInfo.Maneuver.Builder maneuverBuilder = RouteInfo.Maneuver.newBuilder();
            if (naviETAInfo.NaviInfoData != null) {
                int dist = naviETAInfo.NaviInfoData.get(naviETAInfo.NaviInfoFlag).segmentRemain.dist;
                maneuverBuilder.setDist(dist);
            }
            int curManeuverID = naviETAInfo.getCurManeuverID();
            maneuverBuilder.setMType(ScTbtIcon.get(curManeuverID));
            GeoPoint linkLastGeoPoint = L2Package.getInstance().getLinkLastGeoPoint(naviETAInfo.curSegIdx, naviETAInfo.curLinkIdx);
            if (linkLastGeoPoint != null) {
                maneuverBuilder.setLat(linkLastGeoPoint.getLat());
                maneuverBuilder.setLon(linkLastGeoPoint.getLon());
            }
            mManeuvers.add(maneuverBuilder.build());

            mDestinations.clear();
            List<RouteParam> allPoiParamList = RoutePackage.getInstance().getAllPoiParamList(MapType.MAIN_SCREEN_MAIN_MAP);
            ArrayList<NaviEtaInfo.NaviTimeAndDist> viaRemain = naviETAInfo.getViaRemain();
            if (allPoiParamList != null && !allPoiParamList.isEmpty()) {
                for (int i = 1; i < allPoiParamList.size(); i++) {
                    RouteInfo.Destination.Builder destinationBuilder = RouteInfo.Destination.newBuilder();
                    RouteParam routeParam = allPoiParamList.get(i);
                    if (i == allPoiParamList.size() - 1) {
                        destinationBuilder.setDist(naviETAInfo.getRemainDist());
                    } else {
                        NaviEtaInfo.NaviTimeAndDist viaDist = viaRemain.get(i - 1);
                        destinationBuilder.setDist(viaDist.getDist());
                    }
                    GeoPoint naviPos = routeParam.getRealPos();
                    if (naviPos == null) {
                        continue;
                    }
                    destinationBuilder.setLat(naviPos.getLat());
                    destinationBuilder.setLon(naviPos.getLon());
                    destinationBuilder.setDir(RouteInfo.Destination.Direction.AT_DESTINATION);
                    mDestinations.add(destinationBuilder.build());
                }
            }
        }

        @Override
        public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedEntity) {
            if (speedEntity == null) {
                Logger.w(TAG, "onNaviSpeedOverallInfo: null");
                mZoneSpeedLimit = 0;
                return;
            }
            mZoneSpeedLimit = speedEntity.getSpeedLimit();
            Logger.i(TAG, "onNaviSpeedOverallInfo: ", mZoneSpeedLimit);
        }

        @Override
        public void onNaviCameraInfo(final CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                Logger.w(TAG, "onNaviCameraInfo: null");
                mEleEyeSpeedLimit = 0;
                return;
            }
            mEleEyeSpeedLimit = cameraInfo.getSpeed();
            Logger.i(TAG, "onNaviCameraInfo: ", mEleEyeSpeedLimit);
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            mRoadSpeedLimit = speed;
            Logger.i(TAG, "onCurrentRoadSpeed: ", mRoadSpeedLimit);
        }

        @Override
        public void onNaviStop() {
            mDestinations.clear();
            mSegments.clear();
            mManeuvers.clear();
            mNavlaneguidances.clear();
            mRouteBuilder.setId("");
            mRouteBuilder.setLat(0);
            mRouteBuilder.setLon(0);
            mAdasManager.removeRouteInfo(String.valueOf(mPathId));
        }
    };

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(final CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null) {
                Logger.w(TAG, "onShowCruiseCameraExt: cruiseInfoEntity null");
                mCruiseSpeedLimit = 0;
                return;
            }
            if (cruiseInfoEntity.getSpeed() == null) {
                Logger.w(TAG, "onShowCruiseCameraExt: getSpeed null");
                mCruiseSpeedLimit = 0;
                return;
            }
            Logger.i(TAG, "onShowCruiseCameraExt: ", cruiseInfoEntity.getSpeed());
            // 取第一个有效值
            for (int i = 0; i < cruiseInfoEntity.getSpeed().size(); i++) {
                final Short speed = cruiseInfoEntity.getSpeed().get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    mCruiseSpeedLimit = speed;
                    break;
                }
            }
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {
        @Override
        public void onRouteResult(RequestRouteResult requestRouteResult) {
            Logger.d(TAG, "SuperCruise路线变更");
            mReRouting = false;
            updateSegmentData();
            sendRouteData();
        }

        @Override
        public void onReroute() {
            Logger.d(TAG, "SuperCruise重新规划中");
            mReRouting = true;
        }
    };

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetValidated() {

        }

        @Override
        public void onNetConnectSuccess() {
            Logger.i(TAG, "onNetConnectSuccess: ");
            setOnlineMapVersion();
        }

        @Override
        public void onNetDisConnect() {
            Logger.i(TAG, "onNetDisConnect: ");
            setOfflineMapVersion();
        }

        @Override
        public void onNetUnavailable() {

        }

        @Override
        public void onNetBlockedStatusChanged() {

        }

        @Override
        public void onNetLosing() {

        }

        @Override
        public void onNetLinkPropertiesChanged() {

        }
    };

    private final AdasDataStateListener mAdasDataStateListener = new AdasDataStateListener() {
        @Override
        public void onAvailable(long routeLength) {
            Logger.i(TAG, "onAvailable: ", routeLength);
            mRouteLength = routeLength;
        }

        @Override
        public void onAdasStart(long rate) {
            Logger.i(TAG, "onAdasStart: ", rate);
            mRate = rate;
            if (mRouteScheduledFuture != null) {
                mRouteScheduledFuture.cancel(true);
            }
            mRouteScheduledFuture = mScheduler.scheduleWithFixedDelay(mRouteTask, 0, mRate, TimeUnit.SECONDS);
        }

        @Override
        public void onChangeRate(long rate) {
            Logger.i(TAG, "onChangeRate: ", rate);
            mRate = rate;
            if (mRouteScheduledFuture != null) {
                mRouteScheduledFuture.cancel(true);
            }
            mRouteScheduledFuture = mScheduler.scheduleWithFixedDelay(mRouteTask, 0, mRate, TimeUnit.SECONDS);
        }

        @Override
        public void onAdasStop() {
            Logger.i(TAG, "onAdasStop: ");
            if (mRouteScheduledFuture != null) {
                mRouteScheduledFuture.cancel(true);
            }
        }
    };


    //region linkData
    private final Runnable mLinkTask = new Runnable() {
        @Override
        public void run() {
            try {
                sendLinkData();
            } catch (Exception e) {
                Logger.e(TAG, "sendData: ", e);
            }
        }
    };

    private void sendLinkData() {
        boolean mSpeedLimitDataAvailabl;
        switch (mNaviStatus) {
            case NaviStatus.NaviStatusType.NAVING://导航
                if (mRoadSpeedLimit > 0 || mEleEyeSpeedLimit > 0 || mZoneSpeedLimit > 0) {
                    mSpeedLimitDataAvailabl = true;
                    int minSpeedLimit = findMinNonZeroSpeedLimit(mRoadSpeedLimit, mEleEyeSpeedLimit, mZoneSpeedLimit);
                    if (mRoadSpeedLimit == minSpeedLimit) {
                        mSpeedLimitBuilder.setSpeedLimitAssured(false); // TODO 道路限速是false，其他是true
                        mSpeedLimitBuilder.setEffectSpeedLimit(0); // TODO 道路限速是0，其他是speedLimit
                        mSpeedLimitBuilder.setEffectiveSpeedCategory(NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_UNKNOWN);// TODO 道路限速是0，其他是speedLimit
                        mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.BY_TRAFFIC_SIGN); // TODO 路标限速是1，其他是7
                    } else if (mEleEyeSpeedLimit == minSpeedLimit) {
                        mSpeedLimitBuilder.setSpeedLimitAssured(true);
                        mSpeedLimitBuilder.setEffectSpeedLimit(minSpeedLimit);
                        mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(minSpeedLimit));
                        mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
                    } else if (mZoneSpeedLimit == minSpeedLimit) {
                        mSpeedLimitBuilder.setSpeedLimitAssured(true);
                        mSpeedLimitBuilder.setEffectSpeedLimit(minSpeedLimit);
                        mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(minSpeedLimit));
                        mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
                    }
                    mSpeedLimitBuilder.setPostedSpeedLimit(minSpeedLimit);
                    mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(minSpeedLimit));
                } else {
                    mSpeedLimitDataAvailabl = false;
                    mSpeedLimitBuilder.setSpeedLimitAssured(false);
                    mSpeedLimitBuilder.setEffectSpeedLimit(0);
                    mSpeedLimitBuilder.setEffectiveSpeedCategory(NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_UNKNOWN);
                    mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.BY_TRAFFIC_SIGN);
                    mSpeedLimitBuilder.setPostedSpeedLimit(0);
                    mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(0));
                }
                break;
            case NaviStatus.NaviStatusType.CRUISE://巡航
                mSpeedLimitDataAvailabl = mCruiseSpeedLimit > 0;
                mSpeedLimitBuilder.setSpeedLimitAssured(true);
                mSpeedLimitBuilder.setPostedSpeedLimit(mCruiseSpeedLimit);
                mSpeedLimitBuilder.setEffectSpeedLimit(mCruiseSpeedLimit);
                mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(mCruiseSpeedLimit));
                mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
                break;
            default:
                mSpeedLimitDataAvailabl = false;
        }

        final NaviLinkProto.NaviLink.Builder builder = NaviLinkProto.NaviLink.newBuilder();
        builder.setRoadInfo(mRoadInfoBuilder.build());
        builder.setSpeedLimit(mSpeedLimitBuilder.build());
        int brand = CalibrationPackage.getInstance().brand();
        if (brand == 2) { // TODO　SuperCruise提供ADAS测试用，后续需要删除
            builder.setDataAvailable(true);
        } else {
            builder.setDataAvailable(mSpeedLimitDataAvailabl);
        }
        final NaviLinkProto.NaviLink naviLink = builder.build();
        mAdasManager.sendNavilink(naviLink);
        // 下面的部分用于log打印
        if (!Logger.isDebugLevel()) {
            return;
        }
        final SuperCruiseJson superCruiseJson = new SuperCruiseJson();
        superCruiseJson.setDataAvailable(String.valueOf(builder.getDataAvailable()));
        superCruiseJson.setLaneCount(String.valueOf(mRoadInfoBuilder.getLaneCount()));
        superCruiseJson.setRoadCategory(String.valueOf(mRoadInfoBuilder.getRoadCategory()));
        superCruiseJson.setCountryCode(String.valueOf(mRoadInfoBuilder.getCountryCode()));
        superCruiseJson.setMapVersionYear(String.valueOf(mRoadInfoBuilder.getMapVersionYear()));
        superCruiseJson.setMapVersionQuarter(String.valueOf(mRoadInfoBuilder.getMapVersionQuarter()));
        superCruiseJson.setDrivingSideCategory(String.valueOf(mRoadInfoBuilder.getDrivingSideCategory()));
        superCruiseJson.setControlledAccess(String.valueOf(mRoadInfoBuilder.getControlledAccess()));
        superCruiseJson.setDividedRoadCategory(String.valueOf(mRoadInfoBuilder.getDividedRoadCategory()));
        superCruiseJson.setPostedSpeedLimit(String.valueOf(mSpeedLimitBuilder.getPostedSpeedLimit()));
        superCruiseJson.setRecommendedSpeedLimit(String.valueOf(mSpeedLimitBuilder.getRecommendedSpeedLimit()));
        superCruiseJson.setSpeedLimitAssured(String.valueOf(mSpeedLimitBuilder.getSpeedLimitAssured()));
        superCruiseJson.setConditionalSpeedLimit(String.valueOf(mSpeedLimitBuilder.getConditionalSpeedLimit()));
        superCruiseJson.setConditionalSpeedCategory(String.valueOf(mSpeedLimitBuilder.getConditionalSpeedCategory()));
        superCruiseJson.setConditionalSpeedType(String.valueOf(mSpeedLimitBuilder.getConditionalSpeedType()));
        superCruiseJson.setConditionalSpeedType(String.valueOf(mSpeedLimitBuilder.getConditionalSpeedType()));
        superCruiseJson.setEffectiveSpeedCategory(String.valueOf(mSpeedLimitBuilder.getEffectiveSpeedCategory()));
        superCruiseJson.setEffectiveSpeedType(String.valueOf(mSpeedLimitBuilder.getEffectiveSpeedType()));
        superCruiseJson.setSpeedCategory(String.valueOf(mSpeedLimitBuilder.getSpeedCategory()));
        final String json = GsonUtils.toJson(superCruiseJson);
        JsonLog.saveJsonToCache(AppCache.getInstance().getMContext(), json, "sc.json", "sendLinkData");
        Logger.d(TAG, "sendData: ", json);
//        JsonLog.print(TAG, json);
    }

    private int findMinNonZeroSpeedLimit(int... speeds) {
        int minSpeed = Integer.MAX_VALUE;
        boolean foundNonZero = false;

        for (int speed : speeds) {
            if (speed > 0 && speed < minSpeed) {
                minSpeed = speed;
                foundNonZero = true;
            }
        }

        return foundNonZero ? minSpeed : 0; // 如果没有找到非零值，返回0
    }

    /**
     * 限速值转限速等级
     *
     * @param speed 限速值
     * @return 限速等级
     */
    private NaviLinkProto.SpeedLimit.SpeedCategoryEnum speed2SpeedCategoryEnum(final int speed) {
        if (speed > 130) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_0;
        } else if (speed > 101) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_1;
        } else if (speed > 91) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_2;
        } else if (speed > 71) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_3;
        } else if (speed > 51) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_4;
        } else if (speed > 31) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_5;
        } else if (speed > 11) {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_6;
        } else {
            return NaviLinkProto.SpeedLimit.SpeedCategoryEnum.SPEED_CATEGORY_7;
        }
    }

    /**
     * 限速值转限速等级
     *
     * @param speed 限速值
     * @return 限速等级
     */
    private NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum speed2EffectiveSpeedCategoryEnum(final int speed) {
        if (speed < 5) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_5;
        } else if (speed < 7) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_7;
        } else if (speed < 10) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_10;
        } else if (speed < 15) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_15;
        } else if (speed < 20) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_20;
        } else if (speed < 25) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_25;
        } else if (speed < 30) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_30;
        } else if (speed < 35) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_35;
        } else if (speed < 40) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_40;
        } else if (speed < 45) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_45;
        } else if (speed < 50) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_50;
        } else if (speed < 55) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_55;
        } else if (speed < 60) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_60;
        } else if (speed < 65) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_65;
        } else if (speed < 70) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_70;
        } else if (speed < 75) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_75;
        } else if (speed < 80) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_80;
        } else if (speed < 85) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_85;
        } else if (speed < 90) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_90;
        } else if (speed < 95) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_95;
        } else if (speed < 100) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_100;
        } else if (speed < 105) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_105;
        } else if (speed < 110) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_110;
        } else if (speed < 115) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_115;
        } else if (speed < 120) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_120;
        } else if (speed < 130) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_130;
        } else if (speed < 140) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_140;
        } else if (speed < 150) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_150;
        } else if (speed < 160) {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_BELOW_160;
        } else {
            return NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_UNKNOWN;
        }
    }

    private void setOnlineMapVersion() {
        final LocalDate currentDate = LocalDate.now();
        Logger.i(TAG, "setOnlineMapVersion: ", currentDate);
        setMapVersion(currentDate.getYear(), currentDate.getMonthValue());
    }

    private void setOfflineMapVersion() {
        try {
            final LocInfoBean locInfoBean = PositionPackage.getInstance().getLastCarLocation();
            final int adCode = MapDataPackage.getInstance().getAdCodeByLonLat(locInfoBean.getLongitude(), locInfoBean.getLatitude());
            final String mapDataID = MapDataPackage.getInstance().getDataFileVersion(adCode);
            Logger.i(TAG, "setOfflineMapVersion: ", mapDataID);
            // 例：3_24_12_02_01_111111
            String[] strings = mapDataID.split("_");
            setMapVersion(Integer.parseInt("20" + strings[1]), Integer.parseInt(strings[2]));
        } catch (Exception e) {
            Logger.e(TAG, "setOfflineMapVersion: ", e);
        }
    }

    private void setMapVersion(int year, int month) {
        Logger.i(TAG, "setMapVersion: ", year, ", ", month);
        mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.forNumber(year));
        final int quarter = (month - 1) / 3;
        mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.forNumber(quarter));
    }
    //endregion

    private final Runnable mRouteTask = () -> {
        try {
            sendRouteData();
        } catch (Exception e) {
            Logger.e(TAG, "sendData: ", e.getMessage());
            e.printStackTrace();
        }
    };

    private void sendRouteData() {
        boolean active = Objects.equals(mNaviStatus, NaviStatus.NaviStatusType.NAVING);
        if (!active) {
            return;
        }
        RouteInfo.RouteProto route = generateRouteProto(active);
        try {
            String routeInfo = JsonFormat.printer().printingEnumsAsInts().omittingInsignificantWhitespace().print(route);
//            JsonLog.print("sendRouteData", routeInfo);
            JsonLog.saveJsonToCache(AppCache.getInstance().getMContext(), routeInfo, "sc.json", "sendRouteData");
        } catch (InvalidProtocolBufferException e) {
            e.printStackTrace();
        }
        mAdasManager.sendRouteInfo(route);
        Logger.d(TAG, "sendRouteData: success");
    }

    public RouteInfo.RouteProto generateRouteProto(boolean active) {
        //identifying supplier of Client route data.
        mRouteBuilder.setSource(RouteInfo.Route.DataSource.AMAP);
        mRouteBuilder.setActive(active);
        mRouteBuilder.setRerouting(mReRouting);
        //the current position of the car
        mRouteBuilder.clearDestinations();
        mRouteBuilder.addAllDestinations(mDestinations);
        //add segments
        updateSegmentData();
        mRouteBuilder.clearSegments();
        mRouteBuilder.addAllSegments(mSegments);

        mRouteBuilder.clearManeuver();
        mRouteBuilder.clearNavlaneguidance();
        if (active) {
            mRouteBuilder.addAllManeuver(mManeuvers);
            mRouteBuilder.addAllNavlaneguidance(mNavlaneguidances);
        }

        mRouteBuilder.setGpstime(System.currentTimeMillis());
        mRouteProtoBuilder.setRoute(mRouteBuilder.build());
        return mRouteProtoBuilder.build();
    }

    public RouteInfo.ManeuverType amapLaneToManeuverType(int value) {
        switch (value) {
            case 0:
                return RouteInfo.ManeuverType.STRAIGHT;
            case 1:
                return RouteInfo.ManeuverType.TURN_NORMAL_LEFT;
            case 3:
                return RouteInfo.ManeuverType.TURN_NORMAL_RIGHT;
            case 5:
                return RouteInfo.ManeuverType.U_TURN_LEFT;
            case 8:
                return RouteInfo.ManeuverType.U_TURN_RIGHT;
            default:
                return RouteInfo.ManeuverType.UNKNOWN;
        }
    }

    private void updateSegmentData() {
        int curSegIdx = 0;
        int curLinkIdx = 0;
        if (mNaviEtaInfo != null) {
            curSegIdx = mNaviEtaInfo.getCurSegIdx();
            curLinkIdx = mNaviEtaInfo.getCurLinkIdx();
        }
        ScSegmentInfo scSegmentInfo = L2Package.getInstance().getScSegmentInfo(mRouteLength, curSegIdx, curLinkIdx);
        if (scSegmentInfo == null) {
            Logger.d(TAG, "scSegmentInfo null");
            return;
        }
        long pathId = scSegmentInfo.getPathId();
        if (pathId != mPathId) {
            mAdasManager.removeRouteInfo(String.valueOf(mPathId));
            mPathId = pathId;
        }

        mSegments.clear();
        for (ScSegmentInfo.ScSegment scSegment : scSegmentInfo.getScSegments()) {
            if (scSegment == null) {
                continue;
            }
            RouteInfo.Segment.Builder builder = RouteInfo.Segment.newBuilder();
            builder.setId(String.valueOf(scSegment.getIndex()));
            switch (scSegment.getRoadClass()) {
                case 0, 6:
                    builder.setRoadclass(RouteInfo.RoadClassType.CONTROLLED_ACCESS_DIVIDED_ROAD);
                    break;
                case 10:
                    builder.setRoadclass(RouteInfo.RoadClassType.ROADCLASS_UNKNOWN);
                    break;
                default:
                    builder.setRoadclass(RouteInfo.RoadClassType.LOCAL_ROAD);
            }
            List<RouteInfo.Spdlmt> spdlmtList = new ArrayList<>();
            scSegment.getScSpeedLimits().forEach(scSpeedLimit -> {
                RouteInfo.Spdlmt.Builder spdlmtBuilder = RouteInfo.Spdlmt.newBuilder();
                spdlmtBuilder.setDist(scSpeedLimit.getOffset());
                spdlmtBuilder.setSpeed(scSpeedLimit.getSpeed());
                spdlmtList.add(spdlmtBuilder.build());
            });
            builder.addAllSpdlmt(spdlmtList);

            List<RouteInfo.Point> pointList = new ArrayList<>();
            scSegment.getScPoints().forEach(scPoint -> {
                RouteInfo.Point.Builder pointBuilder = RouteInfo.Point.newBuilder();
                pointBuilder.setLat(scPoint.getLat());
                pointBuilder.setLon(scPoint.getLon());
                pointList.add(pointBuilder.build());
            });
            builder.addAllPoints(pointList);

            List<RouteInfo.Livespd> livespdList = new ArrayList<>();
            scSegment.getScRealTimeSpds().forEach(scRealTimeSpd -> {
                RouteInfo.Livespd.Builder livespdBuilder = RouteInfo.Livespd.newBuilder();
                livespdBuilder.setSpeed(scRealTimeSpd);
                livespdBuilder.setDist(0);
                livespdList.add(livespdBuilder.build());
            });
            builder.addAllLivespd(livespdList);

            builder.setLength(scSegment.getLength());
            mSegments.add(builder.build());
        }
    }
}