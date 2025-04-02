package com.fy.navi.adas;

import android.util.Log;

import com.android.utils.NetWorkUtils;
import com.android.utils.gson.GsonUtils;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.proto.NaviLinkProto;

import java.time.LocalDate;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public final class SuperCruiseManager {
    private static final String TAG = SuperCruiseManager.class.getSimpleName();

    private NaviLinkProto.RoadInfo.Builder mRoadInfoBuilder;
    private NaviLinkProto.SpeedLimit.Builder mSpeedLimitBuilder;
    private AdasManager mAdasManager;
    private ScheduledExecutorService mScheduler;
    private boolean misInit = false;

    public static SuperCruiseManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final SuperCruiseManager INSTANCE = new SuperCruiseManager();
    }

    private SuperCruiseManager() {
    }

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
//            if (isShowLane && laneInfoEntity != null && laneInfoEntity.getBackLane() != null) {
//
//            }
        }

        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            if (naviETAInfo != null) {
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
                        mRoadInfoBuilder.setControlledAccess(true);
                        break;
                    case 6: // 城市快速路
                        mRoadInfoBuilder.setControlledAccess(true);
                        break;
                    default:
                        mRoadInfoBuilder.setControlledAccess(false);
                }
            }
        }

        @Override
        public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedEntity) {
            if (speedEntity == null) {
                return;
            }
            mSpeedLimitBuilder.setPostedSpeedLimit(speedEntity.getSpeedLimit());
            mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(speedEntity.getSpeedLimit()));
            mSpeedLimitBuilder.setSpeedLimitAssured(true);
            mSpeedLimitBuilder.setEffectSpeedLimit(speedEntity.getSpeedLimit());
            mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(speedEntity.getSpeedLimit()));
            mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
        }

        @Override
        public void onNaviCameraInfo(final CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                return;
            }
            mSpeedLimitBuilder.setPostedSpeedLimit(cameraInfo.getSpeed());
            mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(cameraInfo.getSpeed()));
            mSpeedLimitBuilder.setSpeedLimitAssured(true);
            mSpeedLimitBuilder.setEffectSpeedLimit(cameraInfo.getSpeed());
            mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(cameraInfo.getSpeed()));
            mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            mSpeedLimitBuilder.setPostedSpeedLimit(speed);
            mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(speed));
            mSpeedLimitBuilder.setSpeedLimitAssured(false); // 道路限速是false，其他是true
            mSpeedLimitBuilder.setEffectSpeedLimit(0); // 道路限速是0，其他是speedLimit
            // 道路限速是0，其他是speedLimit
            mSpeedLimitBuilder.setEffectiveSpeedCategory(NaviLinkProto.SpeedLimit.EffectiveSpeedCategoryEnum.EFFECTIVE_CATEGORY_UNKNOWN);
            mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.BY_TRAFFIC_SIGN); // 路标限速是1，其他是7
        }
    };

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(final CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null || cruiseInfoEntity.getSpeed() == null) {
                return;
            }
            for (int i = 0; i < cruiseInfoEntity.getSpeed().size(); i++) {
                final Short speed = cruiseInfoEntity.getSpeed().get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    mSpeedLimitBuilder.setPostedSpeedLimit(speed);
                    mSpeedLimitBuilder.setSpeedCategory(speed2SpeedCategoryEnum(speed));
                    mSpeedLimitBuilder.setSpeedLimitAssured(true);
                    mSpeedLimitBuilder.setEffectSpeedLimit(speed);
                    mSpeedLimitBuilder.setEffectiveSpeedCategory(speed2EffectiveSpeedCategoryEnum(speed));
                    mSpeedLimitBuilder.setEffectiveSpeedType(NaviLinkProto.SpeedLimit.EffectiveSpeedTypeEnum.EFFECTIVE_UNKNOWN);
                    break;
                }
            }
        }
    };

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {
            final LocalDate currentDate = LocalDate.now();
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.valueOf(currentDate.getYear()));
            final int monthValue = currentDate.getMonthValue();
            final int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.valueOf(quarter));
        }

        @Override
        public void onNetDisConnect() {
            // TODO 离线地图的年份和季度
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

    private final Runnable mTask = this::sendData;

    /**
     * 初始化
     *
     * @param adasManager AdasManager
     */
    public void init(final AdasManager adasManager) {
        if (misInit) {
            return;
        }
        Log.d(TAG, "init: start");
        mRoadInfoBuilder = NaviLinkProto.RoadInfo.newBuilder();
        mSpeedLimitBuilder = NaviLinkProto.SpeedLimit.newBuilder();
        mAdasManager = adasManager;
        initData();
        initObserver();
        mScheduler = Executors.newScheduledThreadPool(1);
        mScheduler.scheduleWithFixedDelay(mTask, 0, 1, TimeUnit.SECONDS);
        misInit = true;
        Log.d(TAG, "init: end");
    }

    /**
     * 初始化数据
     */
    private void initData() {
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
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            final LocalDate currentDate = LocalDate.now();
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.valueOf(currentDate.getYear()));
            final int monthValue = currentDate.getMonthValue();
            final int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.valueOf(quarter));
        } else {
            // TODO 离线地图的年份和季度
        }
    }

    /**
     * 初始化观察者
     */
    private void initObserver() {
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

    /**
     * 发送数据
     */
    private void sendData() {
        if (mAdasManager == null) {
            return;
        }
        final NaviLinkProto.NaviLink.Builder builder = NaviLinkProto.NaviLink.newBuilder();
        builder.setRoadInfo(mRoadInfoBuilder.build());
        builder.setSpeedLimit(mSpeedLimitBuilder.build());
        final boolean status = EnginePackage.getInstance().engineStatus();
        builder.setDataAvailable(status);
        final NaviLinkProto.NaviLink naviLink = builder.build();
        final SuperCruiseJson superCruiseJson = new SuperCruiseJson();
        superCruiseJson.setDataAvailable(String.valueOf(status));
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
//        JsonLog.d(TAG, GsonUtils.toJson(superCruiseJson), "");
        Log.d(TAG, "sendData: " + GsonUtils.toJson(superCruiseJson));
        mAdasManager.sendNavilink(naviLink);
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
}