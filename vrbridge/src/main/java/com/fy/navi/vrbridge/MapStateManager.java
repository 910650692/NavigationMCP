package com.fy.navi.vrbridge;


import android.content.res.Configuration;
import android.text.TextUtils;
import android.util.Log;

import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapTypeId;
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
import com.fy.navi.vrbridge.bean.MapLocation;
import com.fy.navi.vrbridge.bean.MapState;

import java.util.ArrayList;
import java.util.List;

public final class MapStateManager {

    private static final String TAG = MapStateManager.class.getSimpleName();
    private String mCurNaviStatus; //Map当前状态
    private List<RouteLineInfo> mRouteList; //路线规划信息
    private NaviEtaInfo mEtaInfo = null; //TBT信息
    private int mLimitSpeed = 0; //当前道路限速
    private LocParallelInfoEntity mParallelInfo = null; //平行路信息

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

    // 暂无此功能
    private int maxVolumeLevel; //最大导航音量
    private int mCurrentVolumeLevel; //当前导航音量
    private boolean mIsAgreeTeamAgreement; //是否已同意组队协议条款，暂不支持组队，false
    private boolean mIsInTeam;  //是否已在组队队伍，同上
    private boolean mIsTeamLeader; //是否组队队伍的队长，同上
    // 待对应模块添加接口 回调
    private boolean mIsOverView; //全览模式是否开启，boolean值 - true:是 - false:否（默认）
    private int mIsParallelFlagMain; //是否在主路，int值 - 0:主路 - 1:辅路 - -1（默认）
    private boolean mSwitchParallelFlag; //是否可以切换主辅路，boolean值 - true:是 - false:否（默认）
    private int mIsParallelBridge; //是否在桥上，int值 - 0:桥上 - 1:桥下 - -1（默认）
    private boolean mSwitchParallelBridge; //是否可以切换桥上桥下，boolean值 - true:是 - false:否（默认）
    private String mEndPoiCity; //导航目的地所在城市
    private boolean mIsSetHome; //是否设置家的地址，boolean值 - true:是 - false:否（默认）
    private boolean mIsSetCompany; //是否设置工作的地址，boolean值 - true:是 - false:否（默认）
    // 待对应模块添加接口 回调和get
    private boolean mIsFront; //是否是最上层app，boolean值 - true:是 - false:否（默认）
    private boolean mOpenStatus; //导航app是否打开，boolean值 - true:是 - false:否（默认）

    /**
     * 初始化
     */
    public void init() {
        Log.d(IVrBridgeConstant.TAG, "MapStateInit");
        mBuilder.setOpenStatus(true); // TODO
        mBuilder.setFront(NaviPackage.getInstance().getIsAppInForeground());
        mBuilder.setMaxZoomLevel(19);
        mBuilder.setMinZoomLevel(3);
        mBuilder.setHasPrivacyPermission(true);
        mBuilder.setViaPointsMaxCount(5);

        mBuilder.setCurrZoomLevel((int) MapPackage.getInstance().getZoomLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP));

        mCurNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        updateNaviStatus(mCurNaviStatus);

        updateMapMode(MapPackage.getInstance().getCurrentMapMode(MapTypeId.MAIN_SCREEN_MAIN_MAP));

        if (SettingPackage.getInstance().getConfigKeyMute() == 1) {
            mBuilder.setMute(true);
            mBuilder.setBroadcastMode(2);
        } else {
            mBuilder.setMute(false);
            updateBroadcastMode(SettingPackage.getInstance().getConfigKeyBroadcastMode());
        }

        final RoutePreferenceID routePreference = SettingPackage.getInstance().getRoutePreference();
        updatePreference(SettingPackage.getInstance().formatPreferenceToDB(routePreference));

        switch (SettingPackage.getInstance().getConfigKeyDayNightMode()) {
            case 16:
                // TODO 自动模式
                break;
            case 17:
                mBuilder.setDayWithMapStyle(true);
                break;
            case 18:
                mBuilder.setDayWithMapStyle(false);
                break;
            default:
        }

        updateRoadEvent(SettingPackage.getInstance().getConfigKeyRoadEvent());

        final AccountProfileInfo userInfo = AccountPackage.getInstance().getUserInfo();
        mBuilder.setLogin(userInfo != null && !TextUtils.isEmpty(userInfo.uid));

        mBuilder.setSetHome(BehaviorPackage.getInstance().getFavoriteHomeData(1) != null);
        mBuilder.setSetCompany(BehaviorPackage.getInstance().getFavoriteHomeData(2) != null);

        AmapStateUtils.saveMapState(mBuilder.build());

        registerCallback();
    }

    private final NaviStatusCallback mNaviStatusCallback = new NaviStatusCallback() {
        @Override
        public void onNaviStatusChange(final String naviStatus) {
            mCurNaviStatus = naviStatus;
            updateNaviStatus(naviStatus);
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final IMapPackageCallback mIMapPackageCallback = new IMapPackageCallback() {

        @Override
        public void onMapLevelChanged(final MapTypeId mapTypeId, final float mapLevel) {
            Log.d(TAG, "onMapLevelChanged: " + mapLevel);
            mBuilder.setCurrZoomLevel((int) mapLevel);
            AmapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void onUiModeChanged(final int uiMode) {
            final boolean isNightMode = (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
            if (isNightMode) {
                mBuilder.setDayWithMapStyle(false);
            } else {
                mBuilder.setDayWithMapStyle(true);
            }
            AmapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void onMapModeChange(final MapTypeId mapTypeId, final MapMode mapMode) {
            updateMapMode(mapMode);
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final IRouteResultObserver mIRouteResultObserver = new IRouteResultObserver() {

        @Override
        public void onRouteFail(final MapTypeId mapTypeId, final String errorMsg) {
            mRouteList.clear();
        }

        @Override
        public void onRouteResult(final RequestRouteResult requestRouteResult) {
            if (null != requestRouteResult && null != requestRouteResult.getMRouteLineInfos()
                    && !requestRouteResult.getMRouteLineInfos().isEmpty()) {
                final List<RouteLineInfo> routeLineInfos = requestRouteResult.getMRouteLineInfos();
                mRouteList.addAll(routeLineInfos);
                mBuilder.setPathCount(routeLineInfos.size());
                final RouteParam endPoint = RoutePackage.getInstance().getEndPoint(MapTypeId.MAIN_SCREEN_MAIN_MAP);
                if (endPoint != null) {
                    mBuilder.setEndPoiName(endPoint.getName());
                }
                AmapStateUtils.saveMapState(mBuilder.build());
            } else {
                mRouteList.clear();
            }
        }

        @Override
        public void onSpeechViaNum(final int size) {
            mBuilder.setViaPointsCount(size);
            AmapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void onSpeechEndCityName(final String cityName) {
            mBuilder.setEndPoiCity(cityName);
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final SettingPackage.SettingChangeCallback mSettingChangeCallback = new SettingPackage.SettingChangeCallback() {
        @Override
        public void onSettingChanged(final String key, final String value) {
            Log.d(TAG, "registerCallback: key : " + key);
            switch (key) {
                case SettingController.KEY_SETTING_NAVI_BROADCAST:
                    updateBroadcastMode(value);
                    break;
                case SettingController.KEY_SETTING_VOICE_MUTE:
                    switch (value) {
                        case SettingController.VALUE_VOICE_MUTE_ON:
                            mBuilder.setMute(true);
                            mBuilder.setBroadcastMode(2);
                            break;
                        case SettingController.VALUE_VOICE_MUTE_OFF:
                            mBuilder.setMute(false);
                            updateBroadcastMode(SettingPackage.getInstance().getConfigKeyBroadcastMode());
                            break;
                        default:
                            return;
                    }
                    break;
                case SettingController.KEY_SETTING_GUIDE_ROUTE_PREFERENCE:
                    updatePreference(value);
                    break;
                case SettingController.KEY_SETTING_ROAD_CONDITION:
                    updateRoadEvent(value);
                    break;
                case SettingController.KEY_SETTING_DISPLAY_MODE:
                    updateDayNightMode(value);
                    break;
                default:
                    return;
            }
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final AccountCallBack mAccountCallBack = new AccountCallBack() {
        @Override
        public void notifyMobileLogin(final int errCode, final int taskId, final AccountUserInfo result) {
            mBuilder.setLogin(result != null && result.code == 1);
            AmapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void notifyQRCodeLogin(final int errCode, final int taskId, final AccountUserInfo result) {
            mBuilder.setLogin(result != null && result.code == 1);
            AmapStateUtils.saveMapState(mBuilder.build());
        }

        @Override
        public void notifyAccountLogout(final int errCode, final int taskId, final AccountUserInfo result) {
            if (result != null && result.code == 1) {
                mBuilder.setLogin(false);
                AmapStateUtils.saveMapState(mBuilder.build());
            }
        }
    };

    private final IPositionPackageCallback mIPositionPackageCallback = new IPositionPackageCallback() {

        @Override
        public void onLocationInfo(final LocInfoBean locationInfo) {
            // TODO 要求10s一次
            Log.d(TAG, "onLocationInfo: ");
            final MapLocation mapLocation = new MapLocation();
            mapLocation.setLon(locationInfo.getLongitude());
            mapLocation.setLat(locationInfo.getLatitude());
            mapLocation.setBearing((int) locationInfo.getBearing());
            mapLocation.setProvider(locationInfo.getProvider());
            mapLocation.setSpeed((int) locationInfo.getSpeed());
            AmapStateUtils.saveMapLocation(mapLocation);
        }

        @Override
        public void onParallelRoadUpdate(final LocParallelInfoEntity entity) {
            //平行路状态改变
            mParallelInfo = entity;
            if (null != entity) {
                updateParallelRoadStatus(entity);
                AmapStateUtils.saveMapState(mBuilder.build());
            }
        }
    };

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
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    private final NaviPackage.IsInForegroundCallback mForeGroundCallback = new NaviPackage.IsInForegroundCallback() {
        @Override
        public void onAppInForeground(final boolean isInForeground) {
            mBuilder.setFront(isInForeground);
            AmapStateUtils.saveMapState(mBuilder.build());
        }
    };

    /**
     * 注册回调
     */
    private void registerCallback() {
        NaviStatusPackage.getInstance().registerObserver(TAG, mNaviStatusCallback);
        MapPackage.getInstance().registerCallback(MapTypeId.MAIN_SCREEN_MAIN_MAP, mIMapPackageCallback);
        RoutePackage.getInstance().registerRouteObserver(TAG, mIRouteResultObserver);
        SettingPackage.getInstance().setSettingChangeCallback(TAG, mSettingChangeCallback);
        AccountPackage.getInstance().registerCallBack(TAG, mAccountCallBack);
        PositionPackage.getInstance().registerCallBack(mIPositionPackageCallback);
        NaviPackage.getInstance().registerObserver(TAG, mGuidanceObserver);
        NaviPackage.getInstance().addIsAppInForegroundCallback(mForeGroundCallback);
        NaviPackage.getInstance().addOnPreviewStatusChangeListener(mPreViewStatusChangeListener);
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
        }
    }

    /**
     * updateBroadcastMode
     * @param mode mode
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
     * updateDayNightMode
     * @param mode mode
     */
    private void updateDayNightMode(final String mode) {
        switch (mode) {
            case SettingController.VALUE_DISPLAY_MODE_DAYTIME:
                mBuilder.setDayWithMapStyle(true);
                break;
            case SettingController.VALUE_DISPLAY_MODE_NIGHT:
                mBuilder.setDayWithMapStyle(false);
                break;
            case SettingController.VALUE_DISPLAY_MODE_AUTO:
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
        final boolean isNavi = NaviStatus.NaviStatusType.NAVING.equals(mCurNaviStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(mCurNaviStatus);
        Log.d(IVrBridgeConstant.TAG, "naviStatus: " + isNavi);
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

}
