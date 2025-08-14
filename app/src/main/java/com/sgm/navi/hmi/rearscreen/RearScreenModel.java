package com.sgm.navi.hmi.rearscreen;

import android.content.Intent;

import androidx.core.app.ActivityCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.NaviService;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.adapter.map.MapAdapter;
import com.sgm.navi.service.adapter.navi.NaviConstant;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.layer.RouteLineLayerParam;
import com.sgm.navi.service.define.layer.refix.CarModeType;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RouteParam;
import com.sgm.navi.service.logicpaket.clusterorhud.RearScreenRouteHelper;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.base.BaseModel;

import java.util.ArrayList;

public class RearScreenModel extends BaseModel<BaseRearScreenViewModel> implements IMapPackageCallback,
        IRouteResultObserver, ISceneCallback, IGuidanceObserver, ICruiseObserver, StartService.ISdkInitCallback , SettingPackage.SettingChangeCallback {
    private static final String TAG = RearScreenModel.class.getSimpleName();

    private static final float MAP_ZOOM_LEVEL_DEFAULT = 17F;
    private boolean isInitMapView = false;

    private final static String VALUE_NAVI_CAR_LOGO_DEFAULT = "setting_car_logo_default";
    private final static String VALUE_NAVI_CAR_LOGO_BRAND = "setting_car_logo_brand";
    private final static String VALUE_NAVI_CAR_LOGO_SPEED = "setting_car_logo_speed";
    private final static String KEY_SETTING_TEXT_SIZE = "setting_text_size";
    private final static String KEY_SETTING_CAR_LOGO = "setting_car_logo";

    private final MapPackage mMapPackage;
    private final LayerPackage mLayerPackage;
    private final RoutePackage mRoutePackage;
    private final CruisePackage mCruisePackage;
    private final NaviPackage mNaviPackage;
    private final SettingPackage mSettingPackage;
    private final StartService mStartService;

    private boolean isPreview = false; // 是否全览状态，true代表全览

    public RearScreenModel() {
        Logger.d(TAG, "RearScreenModel");
        mMapPackage = MapPackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mCruisePackage = CruisePackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mSettingPackage = SettingPackage.getInstance();
        mStartService = StartService.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "----onCreate");
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.d(TAG, "----onDestroy");
        mMapPackage.unRegisterCallback(getMapId(), this);
        mRoutePackage.unRegisterRouteObserver(mViewModel.mScreenId);
        mCruisePackage.unregisterObserver(mViewModel.mScreenId);
        mStartService.unregisterSdkCallback(TAG, this);
        mNaviPackage.unregisterObserver(mViewModel.mScreenId);
        mSettingPackage.unRegisterSettingChangeCallback(getMapId().name());
        mLayerPackage.unInitLayer(mViewModel.getMapView().provideMapTypeId());
    }

    private MapType getMapId() {
        Logger.d(TAG, "getMapId:" + mViewModel.mScreenId);
        return MapType.valueOf(mViewModel.mScreenId);
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.d(TAG, "Sdk init success");
        if (!isInitMapView) {
            initRearScreenMapAndObservers("RearScreenModel onSdkInitSuccess");
        }
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        Logger.d(TAG, "Sdk init fail");
    }

    @Override
    public void onMapLoadSuccess(final MapType mapTypeId) {
        if (mapTypeId == MapType.REAR_SCREEN_MAP) {
            LayerAdapter.getInstance().initLayer(MapType.REAR_SCREEN_MAP);
            Logger.d(TAG, "后排屏底图加载完成", mapTypeId.name());
            gotoCarPosition();
            mLayerPackage.setCarPosition(mapTypeId, new GeoPoint(PositionPackage.getInstance().getLastCarLocation().getLongitude(),
                    PositionPackage.getInstance().getLastCarLocation().getLatitude(), 0,
                    PositionPackage.getInstance().getLastCarLocation().getCourse()));
            mLayerPackage.setCarMode(mapTypeId, mLayerPackage
                    .getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP));
            mLayerPackage.initCarLogoByFlavor(mapTypeId,  BuildConfig.FLAVOR);
            mLayerPackage.setFollowMode(mapTypeId, true);
            switchMapMode();
            mMapPackage.setZoomLevel(mapTypeId, MAP_ZOOM_LEVEL_DEFAULT);
            MapAdapter.getInstance().updateUiStyle(MapType.REAR_SCREEN_MAP, ThemeUtils.INSTANCE
                    .isNightModeEnabled(AppCache.getInstance().getMContext()) ? ThemeType.NIGHT : ThemeType.DAY);
            LayerAdapter.getInstance().setStartPointVisible(MapType.REAR_SCREEN_MAP,false);
            if (mSettingPackage.getMapViewTextSize()) {
                mMapPackage.setMapViewTextSize(MapType.REAR_SCREEN_MAP, 1.3f);
            } else {
                mMapPackage.setMapViewTextSize(MapType.REAR_SCREEN_MAP, 1.7f);
            }
            showRouteLine();
            mLayerPackage.setStartPointVisible(MapType.REAR_SCREEN_MAP, false);
            initNaviInfo();
        }
    }

    /**
     * 是否正在导航中
     * @return true-导航中, false-非导航中
     */
    public boolean isNavigating() {
        return ConvertUtils.equals(NaviStatusPackage.getInstance()
                .getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING);
    }

    public String getDestinationName() {
        String endName = null;
        RouteParam endPoint = mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP);
        if (endPoint != null) {
            endName = endPoint.getMName();
        }
        return !ConvertUtils.isEmpty(endName) ? endName : "";
    }

    public NaviEtaInfo getCurrentNaviEtaInfo() {
        return mNaviPackage.getCurrentNaviEtaInfo();
    }

    public void initNaviInfo() {
        mViewModel.updateNaviStatus(isNavigating());
        if (isNavigating()) {
            NaviEtaInfo currentNaviEtaInfo = mNaviPackage.getCurrentNaviEtaInfo();
            if (currentNaviEtaInfo != null) {
                mViewModel.updateEta(currentNaviEtaInfo);
            }
        }
    }

    private void showRouteLine() {
        if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)
                || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.LIGHT_NAVING)){
            Logger.d(TAG, "导航中显示导航路线");
            mRoutePackage.showOnlyOneRouteLine(getMapId());
        } else {
            Logger.d(TAG, "非导航中不显示导航路线");
            mRoutePackage.clearRouteLine(getMapId());
        }
    }

    @Override
    public void onNaviStart() {
        Logger.d(TAG, "onNaviStart");
        mViewModel.updateNaviStatus(true);
        switchMapMode();
        mRoutePackage.showOnlyOneRouteLine(getMapId());
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        if (mViewModel == null) return;
        if (!checkNaviInfoPanelLegal(naviETAInfo)) return;
        mViewModel.updateEta(naviETAInfo);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        mViewModel.onUpdateTMCLightBar(naviTmcInfo, false);
    }

    @Override
    public void onNaviStop() {
        Logger.d(TAG, "onNaviStop");
        mRoutePackage.clearRouteLine(getMapId());
        mViewModel.updateNaviStatus(false);
        switchMapMode();
        gotoCarPosition();
    }

    /**
     * 检查导航信息是否合法
     *
     * @param naviEtaInfo 导航信息对象
     * @return boolean 是否合法
     */
    public static boolean checkNaviInfoPanelLegal(final NaviEtaInfo naviEtaInfo) {
        if (naviEtaInfo == null) {
            Logger.d(TAG, "Navi info is null", "ETA info invalid or illegal");
            return false;
        }
        if (naviEtaInfo.NaviInfoData == null || naviEtaInfo.NaviInfoData.isEmpty()) {
            Logger.d(TAG, "Navi info data is null or empty");
            return false;
        }
        if (naviEtaInfo.NaviInfoFlag >= naviEtaInfo.NaviInfoData.size()) {
            Logger.d(TAG, "Navi info flag out of bounds");
            return false;
        }
        if (naviEtaInfo.NaviInfoData.get(naviEtaInfo.NaviInfoFlag) == null) {
            Logger.d(TAG, "Navi info data at flag index is null");
            return false;
        }
        return true;
    }

    /**
     * 设置监听回调
     * @param key   设置项的key值
     * @param value 设置项对应的value值
     */
    @Override
    public void onSettingChanged(String key, String value) {
        Logger.d(TAG, "onSettingChanged:" + key + "-:-" + value);
        // 处理地图文字大小设置
        if (KEY_SETTING_TEXT_SIZE.equals(key)) {
            boolean mapViewTextSize = mSettingPackage.getMapViewTextSize();
            float textSize = mapViewTextSize ? 1.3f : 1.7f;
            mMapPackage.setMapViewTextSize(MapType.REAR_SCREEN_MAP, textSize);
        } else if (KEY_SETTING_CAR_LOGO.equals(key)) {// 处理车标样式设置
            CarModeType carMode;
            if (VALUE_NAVI_CAR_LOGO_DEFAULT.equals(value)) {
                carMode = CarModeType.CAR_MODE_DEFAULT;
            } else if (VALUE_NAVI_CAR_LOGO_BRAND.equals(value)) {
                carMode = CarModeType.CAR_MODEL_BRAND;
                mLayerPackage.initCarLogoByFlavor(MapType.REAR_SCREEN_MAP, BuildConfig.FLAVOR);
            } else if (VALUE_NAVI_CAR_LOGO_SPEED.equals(value)) {
                carMode = CarModeType.CAR_MODEL_SPEED;
            } else {
                carMode = mLayerPackage.getCarModeType(MapType.MAIN_SCREEN_MAIN_MAP);
            }
            Logger.d(TAG, "车标样式仪表获取", carMode);
            mLayerPackage.setCarMode(MapType.REAR_SCREEN_MAP, carMode);
        }
    }

    /**
     * 初始化 RearScreen 地图及相关观察者回调
     */
    public void initRearScreenMapAndObservers(String from) {
        Logger.d(TAG, "sdk 成功",from);
        mMapPackage.registerCallback(getMapId(), this);
        mRoutePackage.registerRouteObserver(mViewModel.mScreenId, this);
        mNaviPackage.registerObserver(mViewModel.mScreenId, this);
        mCruisePackage.registerObserver(mViewModel.mScreenId, this);
        // 监听设置包变化
        mSettingPackage.setSettingChangeCallback(getMapId().name(), this);
        ThreadManager.getInstance().postUi(() -> {
            boolean mapViewInitResult = mMapPackage.createMapView(MapType.REAR_SCREEN_MAP);
            Logger.d(TAG, "mapViewInitResult: ==" , mapViewInitResult);
            if (!mapViewInitResult) return;
            mViewModel.loadMapView();
            isInitMapView = true;
        });
    }

    public void registerRearScreenMap() {
        Logger.d(TAG, "registerRearScreenMap");
        Logger.d(TAG, "mStartService.getSdkActivation()==",mStartService.getSdkActivation());
        if (mStartService.getSdkActivation() == 0) {
            initRearScreenMapAndObservers("registerRearScreenMap");
        } else {
            Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
            mStartService.registerSdkCallback(TAG, this);
        }
    }

    @Override
    public void onRouteDrawLine(RouteLineLayerParam routeLineLayerParam) {
        String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        Logger.d(TAG, "onRouteDrawLine", "currentNaviStatus = ", currentNaviStatus);
        if (NaviStatus.NaviStatusType.NAVING.equals(currentNaviStatus)
                || NaviStatus.NaviStatusType.LIGHT_NAVING.equals(currentNaviStatus)) {
            mRoutePackage.showOnlyOneRouteLine(getMapId());
        }
    }

    //备选路线
    @Override
    public void onSelectMainPathStatus(long pathID, int result) {
        Logger.i(TAG, "onSelectMainPathStatus pathID = ", pathID, " result = ", result);
        if (result == NaviConstant.ChangeNaviPathResult.CHANGE_NAVI_PATH_RESULT_SUCCESS
                && NaviStatusPackage.getInstance().getCurrentNaviStatus()
                .equals(NaviStatus.NaviStatusType.NAVING)) {
            RearScreenRouteHelper.onlyShowCurrentPath(MapType.REAR_SCREEN_MAP);
        }
    }

    @Override
    public void onDeletePath(ArrayList<Long> pathIDList) {
        if (!ConvertUtils.isEmpty(pathIDList)) {
            for (long pathId : pathIDList) {
                Logger.i(TAG, "onDeletePath pathId = ", pathId);
                mRoutePackage.removeRouteLineInfo(MapType.REAR_SCREEN_MAP, pathId);
            }
            RearScreenRouteHelper.refreshPathList();
        }
    }

    @Override
    public void onChangeNaviPath(long oldPathId, long pathID) {
        Logger.i(TAG, "onChangeNaviPath oldPathId = ", oldPathId, " pathID = ", pathID);
        RearScreenRouteHelper.showSelectPatch(pathID);
    }

    public void showPreview() {
        Logger.i(TAG, "showPreview");
        isPreview = true;
//        openOrCloseImmersive(false);
//        mNaviPackage.setPreviewStatus(true);
        switchMapMode();
        mLayerPackage.setFollowMode(getMapId(), false);
        mRoutePackage.showPreview(getMapId(), DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    public void closePreview() {
        Logger.i(TAG, "closePreview");
        isPreview = false;
//        openOrCloseImmersive(true);
//        mNaviPackage.setPreviewStatus(false);
        switchMapMode();
        mLayerPackage.setFollowMode(getMapId(), true);
        mMapPackage.exitPreview(getMapId(), DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
    }

    // 回自车位
    private void gotoCarPosition() {
        mMapPackage.setMapCenter(getMapId(), new GeoPoint(PositionPackage.getInstance()
                .getLastCarLocation().getLongitude(), PositionPackage.getInstance()
                .getLastCarLocation().getLatitude()));
        mMapPackage.goToCarPosition(getMapId());
        mMapPackage.setMapCenterInScreen(getMapId(), mViewModel.getLogoPosition()[0],
                mViewModel.getLogoPosition()[1]);
    }

    private void switchMapMode() {
        if (isNavigating() && !isPreview) {
            mMapPackage.switchMapMode(MapType.REAR_SCREEN_MAP, MapMode.UP_2D,false);
        } else {
            mMapPackage.switchMapMode(MapType.REAR_SCREEN_MAP, MapMode.NORTH_2D,false);
        }
    }

}
