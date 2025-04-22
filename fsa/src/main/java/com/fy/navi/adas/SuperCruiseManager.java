package com.fy.navi.adas;

import android.util.Log;

import com.android.utils.NetWorkUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.proto.NaviLinkProto;

import java.time.LocalDate;
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
    private ScheduledFuture<?> mScheduledFuture;
    private boolean mInitialized = false;
    private int mRoadSpeedLimit = 0;
    private int mEleEyeSpeedLimit = 0;
    private int mZoneSpeedLimit = 0;
    private int mCruiseSpeedLimit = 0;
    private String mNaviStatus = "";

    public static SuperCruiseManager getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final SuperCruiseManager INSTANCE = new SuperCruiseManager();
    }

    private SuperCruiseManager() {
    }

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(String naviStatus) {
            mNaviStatus = naviStatus;
        }
    };

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfoEntity) {
//            if (isShowLane && laneInfoEntity != null && laneInfoEntity.getBackLane() != null) {
//
//            }
        }

        //导航信息
        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            if (naviETAInfo == null) {
                Log.w(TAG, "onNaviInfo: null");
                mRoadInfoBuilder.setRoadCategory(NaviLinkProto.RoadInfo.RoadCategoryEnum.UNRECOGNIZED);
                mRoadInfoBuilder.setControlledAccess(false);
                return;
            }
            Log.i(TAG, "onNaviInfo: " + naviETAInfo.curRoadClass);
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
        }

        //高速限速实
        @Override
        public void onNaviSpeedOverallInfo(final SpeedOverallEntity speedEntity) {
            if (speedEntity == null) {
                Log.w(TAG, "onNaviSpeedOverallInfo: null");
                mZoneSpeedLimit = 0;
                return;
            }
            mZoneSpeedLimit = speedEntity.getSpeedLimit();
            Log.i(TAG, "onNaviSpeedOverallInfo: " + mZoneSpeedLimit);
        }

        @Override
        public void onNaviCameraInfo(final CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                Log.w(TAG, "onNaviCameraInfo: null");
                mEleEyeSpeedLimit = 0;
                return;
            }
            mEleEyeSpeedLimit = cameraInfo.getSpeed();
            Log.i(TAG, "onNaviCameraInfo: " + mEleEyeSpeedLimit);
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            mRoadSpeedLimit = speed;
            Log.i(TAG, "onCurrentRoadSpeed: " + mRoadSpeedLimit);
        }
    };

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(final CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null) {
                Log.w(TAG, "onShowCruiseCameraExt: cruiseInfoEntity null");
                mCruiseSpeedLimit = 0;
                return;
            }
            if (cruiseInfoEntity.getSpeed() == null) {
                Log.w(TAG, "onShowCruiseCameraExt: getSpeed null");
                mCruiseSpeedLimit = 0;
                return;
            }
            Log.i(TAG, "onShowCruiseCameraExt: " + cruiseInfoEntity.getSpeed());
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

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {
            Log.i(TAG, "onNetConnectSuccess: ");
            final LocalDate currentDate = LocalDate.now();
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.forNumber(currentDate.getYear()));
            final int monthValue = currentDate.getMonthValue();
            final int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.forNumber(quarter));
        }

        @Override
        public void onNetDisConnect() {
            Log.i(TAG, "onNetDisConnect: ");
            // TODO 离线地图的年份和季度
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.forNumber(2024));
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.forNumber(3));
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

    private final Runnable mTask = new Runnable() {
        @Override
        public void run() {
            try {
                sendData();
            } catch (Exception e) {
                Log.e(TAG, "sendData: ", e);
            }
        }
    };

    /**
     * 初始化
     *
     * @param adasManager AdasManager
     */
    public void init(final AdasManager adasManager) {
        if (CalibrationPackage.getInstance().adasConfigurationType() != 7) {
            Logger.i(TAG, "not GB Arch ACP3.1 configuration");
            return;
        }
        if (mInitialized) {
            Log.w(TAG, "initialized");
            return;
        }
        Log.i(TAG, "init");
        mRoadInfoBuilder = NaviLinkProto.RoadInfo.newBuilder();
        mSpeedLimitBuilder = NaviLinkProto.SpeedLimit.newBuilder();
        mAdasManager = adasManager;
        initData();
        initObserver();
        initScheduler();
        mInitialized = true;
    }

    /**
     * 销毁
     */
    public void uninit() {
        if (!mInitialized) {
            Log.w(TAG, "not initialized");
            return;
        }
        Log.i(TAG, "uninit");
        NaviPackage.getInstance().unregisterObserver(TAG);
        CruisePackage.getInstance().unregisterObserver(TAG);
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(mNetworkObserver);
        mScheduledFuture.cancel(true);
        mInitialized = false;
    }

    /**
     * 初始化数据
     */
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
        Log.i(TAG, "initData: networkCapabilities = " + networkCapabilities);
        if (Boolean.TRUE.equals(networkCapabilities)) {
            final LocalDate currentDate = LocalDate.now();
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.forNumber(currentDate.getYear()));
            final int monthValue = currentDate.getMonthValue();
            final int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.forNumber(quarter));
        } else {
            // TODO 离线地图的年份和季度
            mRoadInfoBuilder.setMapVersionYear(NaviLinkProto.RoadInfo.MapVersionYearEnum.forNumber(2024));
            mRoadInfoBuilder.setMapVersionQuarter(NaviLinkProto.RoadInfo.MapVersionQuarterEnum.forNumber(3));
        }
    }

    /**
     * 初始化观察者
     */
    private void initObserver() {
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

    /**
     * 初始化调度器
     */
    private void initScheduler() {
        mScheduler = Executors.newScheduledThreadPool(1);
        mScheduledFuture = mScheduler.scheduleWithFixedDelay(mTask, 0, 1, TimeUnit.SECONDS);
    }

    /**
     * 发送数据
     */
    private void sendData() {
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
        builder.setDataAvailable(mSpeedLimitDataAvailabl);
        final NaviLinkProto.NaviLink naviLink = builder.build();
        mAdasManager.sendNavilink(naviLink);
        // 下面的部分用于log打印
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
        JsonLog.saveJsonToCache(json, "sc.json");
        Log.d(TAG, "sendData: " + json);
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
}