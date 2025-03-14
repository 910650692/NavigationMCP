package com.fy.navi.supercruise;

import com.android.utils.NetWorkUtils;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.navi.CameraInfoEntity;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.SpeedOverallEntity;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;

import java.time.LocalDate;

import gm.navigation.enums.ConditionalSpeedCategory;
import gm.navigation.enums.ConditionalSpeedType;
import gm.navigation.enums.CountryCode;
import gm.navigation.enums.EffectiveSpeedCategory;
import gm.navigation.enums.EffectiveSpeedType;
import gm.navigation.enums.MapVersionQuarter;
import gm.navigation.enums.MapVersionYear;
import gm.navigation.enums.RoadCategory;
import gm.navigation.enums.SpeedCategory;
import gm.navigation.enums.SpeedLimitUnit;
import gm.navigation.models.RoadInfo;
import gm.navigation.models.SpeedLimit;

public class SuperCruiseManager {
    private static final String TAG = SuperCruiseManager.class.getSimpleName();

    private RoadInfo mRoadInfo = new RoadInfo();
    private SpeedLimit mSpeedLimit = new SpeedLimit();
    private SuperCruisePlugin mSuperCruisePlugin;

    public static SuperCruiseManager getInstance() {
        return SingleHolder.mInstance;
    }

    private static class SingleHolder {
        private static final SuperCruiseManager mInstance = new SuperCruiseManager();
    }

    private SuperCruiseManager() {
    }

    private final IGuidanceObserver mIGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onLaneInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
            if (isShowLane && laneInfoEntity != null && laneInfoEntity.getBackLane() != null) {

            }
        }

        @Override
        public void onNaviInfo(NaviEtaInfo naviETAInfo) {
            if (naviETAInfo != null) {
                switch (naviETAInfo.curRoadClass) {
                    case 0: // 高速公路
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_HIGHWAY);
                        break;
                    case 1: // 国道
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_SFREEWAY);
                        break;
                    case 2: // 省道
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_FREEWAY);
                        break;
                    case 3: // 县道
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_COLLECTOR);
                        break;
                    case 4: // 乡公路
                    case 5: // 县乡村内部道路
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_ALLEY);
                        break;
                    case 6: // 城市快速路
                    case 7: // 主要道路
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_ARTERIAL);
                        break;
                    case 8: // 次要道路
                    case 9: // 普通道路
                    case 10: // 非导航道路
                    default:
                        mRoadInfo.setRoadCategory(RoadCategory.ROAD_CATEGORY_LOCAL);

                }
                switch (naviETAInfo.curRoadClass) {
                    case 0: // 高速公路
                        mRoadInfo.setControlledAccess(true);
                        break;
                    case 6: // 城市快速路
                        mRoadInfo.setControlledAccess(true);
                        break;
                    default:
                        mRoadInfo.setControlledAccess(false);
                }
            }
        }

        @Override
        public void onNaviSpeedOverallInfo(SpeedOverallEntity speedEntity) {
            if (speedEntity == null) {
                return;
            }
            mSpeedLimit.setPostedSpeedLimit(speedEntity.getSpeedLimit());
            mSpeedLimit.setSpeedCategory(SpeedCategory.convertSpeedToCategory(speedEntity.getSpeedLimit()));
            mSpeedLimit.setSpeedLimitAssured(true);
            mSpeedLimit.setEffectiveSpeedLimit(speedEntity.getSpeedLimit());
            mSpeedLimit.setEffectiveSpeedCategory(EffectiveSpeedCategory.convertSpeedToCategory(speedEntity.getSpeedLimit(), false, true));
            mSpeedLimit.setEffectiveSpeedType(EffectiveSpeedType.UNKNOWN); // TODO 判断是否是路标限速
        }

        @Override
        public void onNaviCameraInfo(CameraInfoEntity cameraInfo) {
            if (cameraInfo == null) {
                return;
            }
            mSpeedLimit.setPostedSpeedLimit(cameraInfo.getSpeed());
            mSpeedLimit.setSpeedCategory(SpeedCategory.convertSpeedToCategory(cameraInfo.getSpeed()));
            mSpeedLimit.setSpeedLimitAssured(true);
            mSpeedLimit.setEffectiveSpeedLimit(cameraInfo.getSpeed());
            mSpeedLimit.setEffectiveSpeedCategory(EffectiveSpeedCategory.convertSpeedToCategory(cameraInfo.getSpeed(), false, true));
            mSpeedLimit.setEffectiveSpeedType(EffectiveSpeedType.UNKNOWN); // TODO 判断是否是路标限速
        }

        @Override
        public void onCurrentRoadSpeed(int speed) {
            mSpeedLimit.setPostedSpeedLimit(speed);
            mSpeedLimit.setSpeedCategory(SpeedCategory.convertSpeedToCategory(speed));
            mSpeedLimit.setSpeedLimitAssured(false);
            mSpeedLimit.setEffectiveSpeedLimit(0);
            mSpeedLimit.setEffectiveSpeedCategory(EffectiveSpeedCategory.CATEGORY_UNKNOWN);
            mSpeedLimit.setEffectiveSpeedType(EffectiveSpeedType.UNKNOWN);
        }
    };

    private final IEngineObserver mIEngineObserver = new IEngineObserver() {
        @Override
        public void onInitEngineSuccess() {
            mRoadInfo.setDataAvailable(true);
        }

        @Override
        public void onInitEngineFail(int code, String msg) {
            mRoadInfo.setDataAvailable(false);
        }
    };

    private final ICruiseObserver mICruiseObserver = new ICruiseObserver() {
        @Override
        public void onShowCruiseCameraExt(CruiseInfoEntity cruiseInfoEntity) {
            if (cruiseInfoEntity == null || cruiseInfoEntity.getSpeed() == null) {
                return;
            }
            for (int i = 0; i < cruiseInfoEntity.getSpeed().size(); i++) {
                Short speed = cruiseInfoEntity.getSpeed().get(i);
                if (speed != null && speed != 0 && speed != 0xFF) {
                    mSpeedLimit.setPostedSpeedLimit(speed);
                    mSpeedLimit.setSpeedCategory(SpeedCategory.convertSpeedToCategory(speed));
                    mSpeedLimit.setSpeedLimitAssured(true);
                    mSpeedLimit.setEffectiveSpeedLimit(speed);
                    mSpeedLimit.setEffectiveSpeedCategory(EffectiveSpeedCategory.convertSpeedToCategory(speed, false, true));
                    mSpeedLimit.setEffectiveSpeedType(EffectiveSpeedType.UNKNOWN); // TODO 判断是否是路标限速
                    break;
                }
            }
        }
    };

    private final NetWorkUtils.NetworkObserver mNetworkObserver = new NetWorkUtils.NetworkObserver() {
        @Override
        public void onNetConnectSuccess() {
            LocalDate currentDate = LocalDate.now();
            mRoadInfo.setMapVersionYear(MapVersionYear.valueOf(currentDate.getYear()));
            int monthValue = currentDate.getMonthValue();
            int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfo.setMapVersionQuarter(MapVersionQuarter.valueOf(quarter));
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

    public void init() {
        mSuperCruisePlugin = new SuperCruisePlugin(AppContext.mContext);
        mSuperCruisePlugin.init();
        initData();
        initObserver();
    }

    private void initData() {
        // 车道数量
        mRoadInfo.setLaneCount(0);
        // 数据是否可用
        mRoadInfo.setDataAvailable(EnginePackage.getInstance().engineStatus());
        // 推荐限速
        mSpeedLimit.setRecommendedSpeedLimit(0);
        // 限速值单位
        mSpeedLimit.setSpeedLimitUnit(SpeedLimitUnit.KM_PER_HOUR);
        // 国家代号
        mRoadInfo.setCountryCode(CountryCode.CN);
        // 条件限速
        mSpeedLimit.setConditionalSpeedLimit(0);
        // 条件限速
        mSpeedLimit.setConditionalSpeedCategory(ConditionalSpeedCategory.CATEGORY_UNKNOWN);
        // 条件限速类型
        mSpeedLimit.setConditionalSpeedType(ConditionalSpeedType.OTHER_UNKNOWN);
        // China and China Taiwan, navi should send 1
        // Hongkong and Aomen, navi should send 0
        mRoadInfo.setDrivingSideCategory(1);
        // 地图数据的日期
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            LocalDate currentDate = LocalDate.now();
            mRoadInfo.setMapVersionYear(MapVersionYear.valueOf(currentDate.getYear()));
            int monthValue = currentDate.getMonthValue();
            int quarter = (monthValue - 1) / 3 + 1;
            mRoadInfo.setMapVersionQuarter(MapVersionQuarter.valueOf(quarter));
        } else {
            // TODO 离线地图的年份和季度
        }
        mSuperCruisePlugin.sendData(mRoadInfo);
    }

    private void initObserver() {
        EnginePackage.getInstance().addEngineObserver(TAG, mIEngineObserver);
        NaviPackage.getInstance().registerObserver(TAG, mIGuidanceObserver);
        CruisePackage.getInstance().registerObserver(TAG, mICruiseObserver);
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(mNetworkObserver);
    }

}