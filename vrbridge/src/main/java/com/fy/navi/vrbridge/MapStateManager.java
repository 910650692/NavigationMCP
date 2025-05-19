package com.fy.navi.vrbridge;

import android.text.TextUtils;

import com.android.utils.log.Logger;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.position.LocParallelInfoEntity;
import com.fy.navi.service.define.route.RequestRouteResult;
import com.fy.navi.service.define.route.RouteLineInfo;
import com.fy.navi.service.define.route.RouteParam;
import com.fy.navi.service.define.route.RoutePreferenceID;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.define.user.account.AccountUserInfo;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.user.account.AccountCallBack;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.behavior.FavoriteStatusCallback;
import com.fy.navi.vrbridge.bean.MapLocation;
import com.fy.navi.vrbridge.bean.MapState;
import com.fy.navi.vrbridge.impl.VoiceSearchManager;

import java.util.ArrayList;
import java.util.List;
import java.util.Timer;
import java.util.TimerTask;

public final class MapStateManager {

    private static final String TAG = MapStateManager.class.getSimpleName();
    private String mCurNaviStatus; //Map当前状态
    private List<RouteLineInfo> mRouteList; //路线规划信息
    private NaviEtaInfo mEtaInfo = null; //TBT信息
    private int mLimitSpeed = 0; //当前道路限速
    private LocParallelInfoEntity mParallelInfo = null; //平行路信息
    private Timer mLocationIntervalTimer; //定位信息倒计时
    private LocationIntervalTask mLocationIntervalTask;
    private int mLocationInterval = 10; //语音定位信息最多10s更新一次


    private final MapState.Builder mBuilder;

    public static MapStateManager getInstance() {
        return MapStateManager.Holder.INSTANCE;
    }

    private static final class Holder {
        private static final MapStateManager INSTANCE = new MapStateManager();
    }

    private MapStateManager() {
        mBuilder = new MapState.Builder();
        mRouteList = new ArrayList<>();
    }

    /**
     * 初始化.
     */
    public void init() {
        final boolean foreground = NaviPackage.getInstance().getIsAppInForeground();
        Logger.d(IVrBridgeConstant.TAG, "MapStateInit, foreground: " + foreground);
        mBuilder.setHasPrivacyPermission(true);
        mBuilder.setOpenStatus(foreground);
        mBuilder.setFront(foreground);
        mBuilder.setMaxZoomLevel(19);
        mBuilder.setMinZoomLevel(3);
        mBuilder.setViaPointsMaxCount(5);
        mBuilder.setListPage(false);

        mBuilder.setCurrZoomLevel((int) MapPackage.getInstance().getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP));

        mCurNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        updateNaviStatus(mCurNaviStatus);

        updateMapMode(MapPackage.getInstance().getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP));

        final int muteMode = SettingPackage.getInstance().getConfigKeyMute();
        Logger.d(IVrBridgeConstant.TAG, "init muteMode: " + muteMode);
        if (muteMode == 1) {
            mBuilder.setMute(true);
            mBuilder.setBroadcastMode(2);
        } else {
            mBuilder.setMute(false);
            final int broadcastMode = SettingPackage.getInstance().getConfigKeyBroadcastMode();
            updateBroadcastMode(broadcastMode);
        }

        final RoutePreferenceID routePreference = SettingPackage.getInstance().getRoutePreference();
        updatePreference(SettingPackage.getInstance().formatPreferenceToDB(routePreference));

        updateRoadEvent(SettingPackage.getInstance().getConfigKeyRoadEvent());

        final AccountProfileInfo userInfo = AccountPackage.getInstance().getUserInfo();
        mBuilder.setLogin(userInfo != null && !TextUtils.isEmpty(userInfo.getUid()));

        mBuilder.setSetHome(BehaviorPackage.getInstance().getHomeFavoriteInfo() != null);
        mBuilder.setSetCompany(BehaviorPackage.getInstance().getCompanyFavoriteInfo() != null);

        AMapStateUtils.saveMapState(mBuilder.build());

        registerCallback();
    }

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(final String naviStatus) {
            mCurNaviStatus = naviStatus;
            updateNaviStatus(naviStatus);
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final IMapPackageCallback mIMapPackageCallback = new IMapPackageCallback() {

        @Override
        public void onMapLevelChanged(final MapType mapTypeId, final float mapLevel) {
            Logger.d(TAG, "onMapLevelChanged: " + mapLevel);
            mBuilder.setCurrZoomLevel((int) mapLevel);
            AMapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void onMapModeChange(final MapType mapTypeId, final MapMode mapMode) {
            updateMapMode(mapMode);
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {

        @Override
        public void onRouteFail(final MapType mapTypeId, final String errorMsg) {
            mRouteList.clear();
            VoiceSearchManager.getInstance().playRouteResult(0);
        }

        @Override
        public void onRouteResult(final RequestRouteResult requestRouteResult) {
            if (null != requestRouteResult && null != requestRouteResult.getMRouteLineInfos()
                    && !requestRouteResult.getMRouteLineInfos().isEmpty()) {
                final List<RouteLineInfo> routeLineInfos = requestRouteResult.getMRouteLineInfos();
                final int size = routeLineInfos.size();
                mRouteList.addAll(routeLineInfos);
                mBuilder.setPathCount(size);
                AMapStateUtils.saveMapState(mBuilder.build());
                VoiceSearchManager.getInstance().playRouteResult(size);
            } else {
                mRouteList.clear();
                VoiceSearchManager.getInstance().playRouteResult(0);
            }
        }

        @Override
        public void onSpeechViaNum(final int size) {
            mBuilder.setViaPointsCount(size);
            AMapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void onSpeechEndCityName(final String cityName, final String endName) {
            boolean updated = false;
            if (!TextUtils.isEmpty(cityName)) {
                mBuilder.setEndPoiCity(cityName);
                updated = true;
            }
            if (!TextUtils.isEmpty(endName)) {
                mBuilder.setEndPoiName(endName);
                updated = true;
            }

            if (updated) {
                AMapStateUtils.saveMapState(mBuilder.build());
            }
        }
    };

    private final SettingPackage.SettingChangeCallback mSettingChangeCallback = new SettingPackage.SettingChangeCallback() {
        @Override
        public void onSettingChanged(final String key, final String value) {
            switch (key) {
                case SettingController.KEY_SETTING_NAVI_BROADCAST:
                    //引导播报
                    updateBroadcastMode(value);
                    break;
                case SettingController.KEY_SETTING_VOICE_MUTE:
                    //播报静音
                    switch (value) {
                        case SettingController.VALUE_VOICE_MUTE_ON:
                            //声音打开
                            mBuilder.setMute(false);
                            updateBroadcastMode(SettingPackage.getInstance().getConfigKeyBroadcastMode());
                            break;
                        case SettingController.VALUE_VOICE_MUTE_OFF:
                            //声音关闭
                            mBuilder.setMute(true);
                            mBuilder.setBroadcastMode(2);
                            break;
                        default:
                            return;
                    }
                    break;
                case SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE:
                    //路线偏好
                    updatePreference(value);
                    break;
                case SettingController.KEY_SETTING_ROAD_CONDITION:
                    //路况开关
                    updateRoadEvent(value);
                    break;
                default:
                    return;
            }
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final AccountCallBack mAccountCallBack = new AccountCallBack() {
        @Override
        public void notifyMobileLogin(final int errCode, final int taskId, final AccountUserInfo result) {
            mBuilder.setLogin(result != null && result.getCode() == 1);
            AMapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void notifyQRCodeLogin(final int errCode, final int taskId, final AccountUserInfo result) {
            mBuilder.setLogin(result != null && result.getCode() == 1);
            AMapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {
            if (result != null && result.getCode() == 1) {
                mBuilder.setLogin(false);
                AMapStateUtils.saveMapState(mBuilder.build());
            }
        }
    };

    private final IPositionPackageCallback mIPositionPackageCallback = new IPositionPackageCallback() {

        @Override
        public void onLocationInfo(final LocInfoBean locationInfo) {
            if (mLocationInterval == 0) {
                final MapLocation mapLocation = new MapLocation();
                mapLocation.setLon(locationInfo.getLongitude());
                mapLocation.setLat(locationInfo.getLatitude());
                mapLocation.setBearing((int) locationInfo.getBearing());
                mapLocation.setProvider(locationInfo.getProvider());
                mapLocation.setSpeed((int) locationInfo.getSpeed());
                AMapStateUtils.saveMapLocation(mapLocation);
                mLocationInterval = 10;
                mLocationIntervalTimer = new Timer();
                mLocationIntervalTask = new LocationIntervalTask();
                mLocationIntervalTimer.schedule(mLocationIntervalTask, 1000, 1000);
            }
        }

        @Override
        public void onParallelRoadUpdate(final LocParallelInfoEntity entity) {
            //平行路状态改变
            mParallelInfo = entity;
            if (null != entity) {
                updateParallelRoadStatus(entity);
                AMapStateUtils.saveMapState(mBuilder.build());
            }
        }
    };

    //收到定位信息后，开始倒计时，10s才允许再次更新位置新
    private final class LocationIntervalTask extends TimerTask {
        @Override
        public void run() {
            countDownInterval();
        }
    }

    /**
     * 定位信息倒计时.
     */
    private void countDownInterval() {
        mLocationInterval--;
        if (mLocationInterval == 0) {
            if (null != mLocationIntervalTimer) {
                mLocationIntervalTimer.cancel();
            }
            mLocationIntervalTask = null;
        }
    }

    private final IGuidanceObserver mGuidanceObserver = new IGuidanceObserver() {
        @Override
        public void onNaviInfo(final NaviEtaInfo naviETAInfo) {
            mEtaInfo = naviETAInfo;
        }

        @Override
        public void onCurrentRoadSpeed(final int speed) {
            //当前道路限速
            mLimitSpeed = speed;
        }

        @Override
        public void onNaviStop() {
            mLimitSpeed = 0;
            mEtaInfo = null;
            mRouteList.clear();
        }
    };

    private final NaviPackage.OnPreViewStatusChangeListener mPreViewStatusChangeListener = new NaviPackage.OnPreViewStatusChangeListener() {
        @Override
        public void onPreViewStatusChange(final boolean isPreView) {
            mBuilder.setOverView(isPreView);
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final NaviPackage.IsInForegroundCallback mForeGroundCallback = new NaviPackage.IsInForegroundCallback() {
        @Override
        public void onAppInForeground(final boolean isInForeground) {
            Logger.w(IVrBridgeConstant.TAG, "appInForeground: " + isInForeground);
            mBuilder.setFront(isInForeground);
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final FavoriteStatusCallback mFavoriteStatusCallback = new FavoriteStatusCallback() {
        @Override
        public void notifyFavoriteHomeChanged(final boolean isSet) {
            mBuilder.setSetHome(isSet);
            AMapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void notifyFavoriteCompanyChanged(final boolean isSet) {
            mBuilder.setSetCompany(isSet);
            AMapStateUtils.saveMapState(mBuilder.build());
        }
    };

    /**
     * 注册回调
     */
    private void registerCallback() {
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        MapPackage.getInstance().registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, mIMapPackageCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        SettingPackage.getInstance().setSettingChangeCallback(TAG, mSettingChangeCallback);
        AccountPackage.getInstance().registerCallBack(TAG, mAccountCallBack);
        PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
        NaviPackage.getInstance().registerObserver(TAG, mGuidanceObserver);
        NaviPackage.getInstance().addIsAppInForegroundCallback(mForeGroundCallback);
        NaviPackage.getInstance().addOnPreviewStatusChangeListener(mPreViewStatusChangeListener);
        BehaviorPackage.getInstance().registerFavoriteStatusCallback(mFavoriteStatusCallback);
    }

    /**
     * updateNaviStatus
     * @param naviStatus naviStatus
     */
    private void updateNaviStatus(final String naviStatus) {
        switch (naviStatus) {
            case NaviStatus.NaviStatusType.NAVING:
            case NaviStatus.NaviStatusType.LIGHT_NAVING:
                mBuilder.setGPSNavi(true);
                mBuilder.setCruiseNavi(false);
                mBuilder.setRoutePage(false);
                break;
            case NaviStatus.NaviStatusType.CRUISE:
                mBuilder.setGPSNavi(false);
                mBuilder.setCruiseNavi(true);
                mBuilder.setRoutePage(false);
                break;
            case NaviStatus.NaviStatusType.SELECT_ROUTE:
                mBuilder.setGPSNavi(false);
                mBuilder.setCruiseNavi(false);
                mBuilder.setRoutePage(true);
                break;
            default:
                mBuilder.setGPSNavi(false);
                mBuilder.setCruiseNavi(false);
                mBuilder.setRoutePage(false);
                break;
        }
    }

    /**
     * updateMapMode
     * @param mapMode mapMode
     */
    private void updateMapMode(final MapMode mapMode) {
        switch (mapMode) {
            case NORTH_2D:
                mBuilder.setCurrMapMode(1);
                break;
            case UP_2D:
                mBuilder.setCurrMapMode(0);
                break;
            case UP_3D:
                mBuilder.setCurrMapMode(2);
                break;
            default:
                mBuilder.setCurrMapMode(-1);
                break;
        }
    }


    /**
     * updateBroadcastMode
     * @param mode mode 1：经典简洁播报； 2：新手详细播报，默认态； 3：极简播报.
     */
    private void updateBroadcastMode(final int mode) {
        switch (mode) {
            case 1:
                mBuilder.setBroadcastMode(1);
                break;
            case 2:
                mBuilder.setBroadcastMode(0);
                break;
            case 3:
                mBuilder.setBroadcastMode(5);
                break;
            default:
                break;
        }
    }

    /**
     * updateBroadcastMode
     * @param mode mode
     */
    private void updateBroadcastMode(final String mode) {
        switch (mode) {
            case SettingController.VALUE_NAVI_BROADCAST_DETAIL:
                mBuilder.setBroadcastMode(0);
                break;
            case SettingController.VALUE_NAVI_BROADCAST_CONCISE:
                mBuilder.setBroadcastMode(1);
                break;
            case SettingController.VALUE_NAVI_BROADCAST_SIMPLE:
                mBuilder.setBroadcastMode(5);
                break;
            default:
                break;
        }
    }

    /**
     * updateRoadEvent
     * @param value value
     */
    private void updateRoadEvent(final String value) {
        switch (value) {
            case SettingController.VALUE_GENERIC_TRUE:
                mBuilder.setRoadEvent(true);
                break;
            case SettingController.VALUE_GENERIC_FALSE:
                mBuilder.setRoadEvent(false);
                break;
            default:
        }
    }

    /**
     * updateRoadEvent
     * @param value value
     */
    private void updateRoadEvent(final boolean value) {
        mBuilder.setRoadEvent(value);
    }

    /**
     * updatePreference
     * @param routePreference routePreference
     */
    private void updatePreference(final String routePreference) {
        switch (routePreference) {
            case SettingController.VALUE_ROUTE_PREFERENCE_RECOMMEND:
                mBuilder.setCurrPlanPref(1);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION:
                mBuilder.setCurrPlanPref(16);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE:
                mBuilder.setCurrPlanPref(8);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_NOT_HIGHWAY:
                mBuilder.setCurrPlanPref(4);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_FIRST_HIGHWAY:
                mBuilder.setCurrPlanPref(512);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_FIRST_MAIN_ROAD:
                mBuilder.setCurrPlanPref(1024);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_FASTEST_SPEED:
                mBuilder.setCurrPlanPref(256);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE:
                mBuilder.setCurrPlanPref(8 | 16);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_NOT_HIGHWAY:
                mBuilder.setCurrPlanPref(16 | 4);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_HIGHWAY:
                mBuilder.setCurrPlanPref(16 | 512);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_LESS_CHARGE_AND_NOT_HIGHWAY:
                mBuilder.setCurrPlanPref(8 | 4);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_LESS_CHARGE_AND_NOT_HIGHWAY:
                mBuilder.setCurrPlanPref(16 | 8 | 4);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FIRST_MAIN_ROAD:
                mBuilder.setCurrPlanPref(16 | 1024);
                break;
            case SettingController.VALUE_ROUTE_PREFERENCE_AVOID_CONGESTION_AND_FASTEST_SPEED:
                mBuilder.setCurrPlanPref(16 | 256);
                break;
            default:
        }
    }

    /**
     * updateParallelRoadStatus
     * @param entity LocParallelInfoEntity
     */
    private void updateParallelRoadStatus(final LocParallelInfoEntity entity) {
        final int status = entity.getStatus();
        final int flag = entity.getFlag();
        final int hwFlag = entity.getHwFlag();
        if (status == 1) {
            switch (flag) {
                case 1:
                    mBuilder.setParallelFlagMain(0);
                    mBuilder.setSwitchParallelFlag(true);
                    break;
                case 2:
                    mBuilder.setParallelFlagMain(1);
                    mBuilder.setSwitchParallelFlag(true);
                    break;
                default:
                    mBuilder.setParallelFlagMain(-1);
                    mBuilder.setSwitchParallelFlag(false);
            }
            switch (hwFlag) {
                case 1:
                    mBuilder.setParallelBridge(0);
                    mBuilder.setSwitchParallelBridge(true);
                    break;
                case 2:
                    mBuilder.setParallelBridge(1);
                    mBuilder.setSwitchParallelBridge(true);
                    break;
                default:
                    mBuilder.setParallelBridge(-1);
                    mBuilder.setSwitchParallelBridge(false);
                    break;
            }
        } else {
            //非平行路切换期间
            mBuilder.setParallelFlagMain(-1);
            mBuilder.setSwitchParallelFlag(false);
            mBuilder.setParallelBridge(-1);
            mBuilder.setSwitchParallelBridge(false);
        }
    }

    /**
     * 更新搜索结果列表页面展示状态.
     *
     * @param opened  true:打开  false:关闭
     */
    public void updateListPageState(final boolean opened) {
        mBuilder.setListPage(opened);
        AMapStateUtils.saveMapState(mBuilder.build());
    }


    /**
     * 获取TBT信息，用于返回当前行程信息查询信息.
     *
     * @return NaviEtaInfo，引导信息.
     */
    public NaviEtaInfo getEtaInfo() {
        return mEtaInfo;
    }

    /**
     * 当前道路限速
     *
     * @return int，大于0-有限速值，0-没有限速.
     */
    public int getLimitSpeed() {
        return mLimitSpeed;
    }

    /**
     * 当前是否处于引导态.
     * @return boolean
     */
    public boolean isNaviStatus() {
        mCurNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        final boolean isNavi = NaviStatus.NaviStatusType.NAVING.equals(mCurNaviStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(mCurNaviStatus);
        Logger.d(IVrBridgeConstant.TAG, "naviStatus: " + isNavi);
        return isNavi;
    }

    public LocParallelInfoEntity getParallelInfo() {
        return mParallelInfo;
    }

    /**
     * 当前是否处于选路页面.
     *
     * @return true:选路态  false：非选路态.
     */
    public boolean inSelectRoute() {
        mCurNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        return NaviStatus.NaviStatusType.SELECT_ROUTE.equals(mCurNaviStatus);
    }

    /**
     * 获取路线规划结果.
     *
     * @return mRouteList, 路线信息列表.
     */
    public List<RouteLineInfo> getRouteList() {
        return mRouteList;
    }

    /**
     * 当前是否处于巡航状态.
     *
     * @return true:处于巡航态  false：非巡航态.
     */
    public boolean inCruiseStatus() {
        mCurNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        return NaviStatus.NaviStatusType.CRUISE.equals(mCurNaviStatus);
    }

}
