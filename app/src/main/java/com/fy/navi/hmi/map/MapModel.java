package com.fy.navi.hmi.map;

import static com.fy.navi.hmi.utils.AiWaysGestureManager.AiwaysGestureListener;
import static com.fy.navi.hmi.utils.AiWaysGestureManager.GestureEvent;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Bundle;
import android.os.Looper;
import android.os.Parcelable;
import android.provider.Settings;
import android.text.TextUtils;
import android.view.MotionEvent;
import android.view.WindowManager;

import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.exportservice.ExportIntentParam;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.launcher.FloatViewManager;
import com.fy.navi.hmi.navi.NaviGuidanceFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.hmi.splitscreen.SplitScreenManager;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.layer.refix.DynamicLevelMode;
import com.fy.navi.utils.ThreeFingerFlyingScreenManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.account.AccountQRCodeLoginFragment;
import com.fy.navi.hmi.message.MessageCenterHelper;
import com.fy.navi.hmi.navi.AuthorizationRequestDialog;
import com.fy.navi.hmi.navi.ContinueNaviDialog;
import com.fy.navi.hmi.navi.PhoneAddressDialog;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.hmi.utils.AiWaysGestureManager;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.aos.RestrictedEndNumberParam;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.aos.TrafficRestrictResponseParam;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.MapVisibleAreaDataManager;
import com.fy.navi.service.define.map.MapVisibleAreaInfo;
import com.fy.navi.service.define.map.MapVisibleAreaType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.message.MessageCenterInfo;
import com.fy.navi.service.define.mfc.MfcController;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.SoundInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.position.LocInfoBean;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.screen.ScreenType;
import com.fy.navi.service.define.search.FavoriteInfo;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.search.SearchResultEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
import com.fy.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.fy.navi.service.define.user.msgpush.MsgPushInfo;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.fy.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.fy.navi.service.logicpaket.calibration.CalibrationPackage;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.message.MessageCenterCallBack;
import com.fy.navi.service.logicpaket.message.MessageCenterManager;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.IRouteResultObserver;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.search.SearchResultCallback;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.fy.navi.service.logicpaket.signal.SignalCallback;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.user.account.AccountPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.forecast.ForecastCallBack;
import com.fy.navi.service.logicpaket.user.forecast.ForecastPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushCallBack;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.fy.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseModel;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;
import com.fy.navi.vrbridge.IVrBridgeConstant;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapModel extends BaseModel<MapViewModel> implements IMapPackageCallback,
        ImmersiveStatusScene.IImmersiveStatusCallBack, IAosRestrictedObserver, IPositionPackageCallback,
        SignalCallback, SpeedMonitor.CallBack, ICruiseObserver, SettingPackage.SettingChangeCallback,
        MsgPushCallBack, IGuidanceObserver, MessageCenterCallBack, IRouteResultObserver, ILayerPackageCallBack
        , ForecastCallBack, SearchResultCallback, SplitScreenManager.OnScreenModeChangedListener, INaviStatusCallback {
    private final MapPackage mapPackage;
    private final LayerPackage layerPackage;
    private final PositionPackage positionPackage;
    private final MapDataPackage mapDataPackage;
    private final HistoryManager mHistoryManager;
    private final AosRestrictedPackage restrictedPackage;
    private AiWaysGestureManager aiwaysGestureManager;
    private CommonManager commonManager;
    private static final String TAG = "MapModel";
    private long limitQueryTaskId;
    private long limitEndNumberTaskId;
    private int mCurrentCityCode;
    private int mCompanyOrHomeType;
    private String mLicense = "";
    private boolean mAvoidLimit = false;
    private String mFilename = "";
    private ScheduledFuture mSelfParkingTimer;//回车位倒计时
    private ScheduledFuture mCloseTmcTimer;//离线关闭TMC倒计时
    private SettingManager settingManager;
    private CruisePackage cruisePackage;
    private SignalPackage signalPackage;
    private MsgPushPackage msgPushPackage;
    private SpeedMonitor speedMonitor;
    private NaviPackage naviPackage;
    private MapModelHelp mapModelHelp;
    private MessageCenterManager messageCenterManager;
    private MessageCenterHelper messageCenterHelper;
    private final CalibrationPackage mCalibrationPackage;
    private final SettingPackage mSettingPackage;
    private final RoutePackage mRoutePackage;
    private BehaviorPackage behaviorPackage;
    private SearchPackage searchPackage;
    private final String mCallbackId;
    private ForecastPackage mforCastPackage;
    private final AccountPackage mAccountPackage;
    private NaviStatusPackage mNaviStatusPackage;
    private boolean mLoadMapSuccess = true;  //只加载一次
    // 24小时对应的毫秒数：24 * 60 * 60 * 1000 = 86,400,000
    private final long MILLIS_IN_24_HOURS = 86400000;
    private History mUncompletedNavi;
    private MapVisibleAreaDataManager mapVisibleAreaDataManager;
    private AuthorizationRequestDialog authorizationRequestDialog = null;

    public MapModel() {
        mCallbackId = UUID.randomUUID().toString();
        mapPackage = MapPackage.getInstance();
        layerPackage = LayerPackage.getInstance();
        positionPackage = PositionPackage.getInstance();
        mapDataPackage = MapDataPackage.getInstance();
        restrictedPackage = AosRestrictedPackage.getInstance();
        restrictedPackage.addRestrictedObserver(IAosRestrictedObserver.KEY_OBSERVER_LIMIT, this);
        commonManager = CommonManager.getInstance();
        commonManager.init();
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
        settingManager = SettingManager.getInstance();
        settingManager.init();
        cruisePackage = CruisePackage.getInstance();
        signalPackage = SignalPackage.getInstance();
        msgPushPackage = MsgPushPackage.getInstance();
        speedMonitor = new SpeedMonitor();
        speedMonitor.registerCallBack(this);
        msgPushPackage.registerCallBack(TAG, this);
        naviPackage = NaviPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mapModelHelp = new MapModelHelp(MapType.MAIN_SCREEN_MAIN_MAP);
        messageCenterManager = MessageCenterManager.getInstance();
        messageCenterManager.registerCallBack(MessageCenterManager.MESSAGECENTERKEY, this);
        messageCenterHelper = new MessageCenterHelper();
        mSettingPackage = SettingPackage.getInstance();
        mRoutePackage = RoutePackage.getInstance();
        mRoutePackage.registerRouteObserver("Map Activity", this);
        behaviorPackage = BehaviorPackage.getInstance();
        searchPackage = SearchPackage.getInstance();
        mforCastPackage = ForecastPackage.getInstance();
        mforCastPackage.registerCallBack(this);
        searchPackage.registerCallBack(mCallbackId, this);
        mAccountPackage = AccountPackage.getInstance();
        mNaviStatusPackage = NaviStatusPackage.getInstance();
        mapVisibleAreaDataManager = MapVisibleAreaDataManager.getInstance();
        addGestureListening();//添加收拾监听
        NavistatusAdapter.getInstance().registerCallback(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        SplitScreenManager.getInstance().registerListener(this, TAG);
        mViewModel.initVisibleAreaPoint();
        Logger.d("MapViewModelonCreate1");
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mapPackage.unBindMapView(mViewModel.getMapView());
        speedMonitor.unInit();
        mapModelHelp.unInit();
        cruisePackage.unregisterObserver(mViewModel.mScreenId);
        NavistatusAdapter.getInstance().unRegisterCallback(this);
        SplitScreenManager.getInstance().unRegisterListener(this, TAG);
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        boolean mapViewInitResult = MapPackage.getInstance().createMapView(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!mapViewInitResult) return;
        mapPackage.bindMapView(mapSurfaceView);
        layerPackage.initLayer(MapType.MAIN_SCREEN_MAIN_MAP);
        layerPackage.initCarLogoByFlavor(MapType.MAIN_SCREEN_MAIN_MAP, BuildConfig.FLAVOR);
        mapPackage.registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        layerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        ImmersiveStatusScene.getInstance().registerCallback("MapModel", this);
    }

    public void startListenMsg() {
        String uid = "";
        String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i(TAG, "getUserInfo valueJson = " , valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            Logger.i(TAG, "Login = " , uid);
            msgPushPackage.startListen(uid);
        } else {
            Logger.i(TAG, "Logout");
        }
    }

    public void loadVisibleAreaJson(String jsonPath) {
        Logger.d("loadVisibleAreaJson", "loadVisibleAreaJson:" , SplitScreenManager.getInstance().isInMultiWindow());
        if (SplitScreenManager.getInstance().isInMultiWindow()) {
            return;
        }
        mapVisibleAreaDataManager.loadData(jsonPath);
    }

    public MapVisibleAreaInfo getVisibleArea(MapVisibleAreaType mapVisibleAreaType) {
        MapVisibleAreaInfo mapVisibleAreaInfo = mapVisibleAreaDataManager.getDataByKey(mapVisibleAreaType);
        if (mViewModel.showNdGoHomeView()) {
            //如果是ND  需要把数据转换为dp
            int left = ScreenUtils.Companion.getInstance().dp2px(mapVisibleAreaInfo.getMleftscreenoffer());
            int top = ScreenUtils.Companion.getInstance().dp2px(mapVisibleAreaInfo.getMtopscreenoffer());
            MapVisibleAreaInfo ndMapArea = new MapVisibleAreaInfo(left, top);
            return ndMapArea;
        }
        return mapVisibleAreaInfo;
    }

    /***
     * 获取设置限行政策
     */
    public void getCurrentCityLimit() {
        final boolean avoidLimit = getAvoidLimit();
        final String license = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (license == null || license.isEmpty() || !avoidLimit) {
            Logger.d(TAG, "The limit switch is turned on but not turned on");
            mViewModel.setLimitDriverVisibility(false);
            return;
        }

        if (positionPackage.getLastCarLocation() == null) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "Limit get position failed");
            return;
        }
        final int currentCityCode = mapDataPackage.getAdCodeByLonLat(positionPackage.getLastCarLocation().getLongitude(),
                positionPackage.getLastCarLocation().getLatitude());

        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "get CurrentCity Limit " , currentCityCode);
        mCurrentCityCode = currentCityCode;
        mLicense = license;
        final RestrictedParam restrictedParam = new RestrictedParam();
        // 请求根据车型等信息获取的规则 type = 7 请求城市全部规则 type = 8 请求城市列表 type = 9 根据规则请求数据
        restrictedParam.setRestrict_type(7);
        restrictedParam.setPlate(mLicense);
        restrictedParam.setAdcodes(String.valueOf(mCurrentCityCode));
        limitQueryTaskId = restrictedPackage.queryRestrictedInfo(restrictedParam);
    }

    /***
     * 获取设置限行尾号政策
     */
    public void getCurrentCityLimitEndNumber() {
        final boolean avoidLimit = getAvoidLimit();
        final String license = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (license == null || license.isEmpty() || !avoidLimit) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "The limit switch is turned on but not turned on");
            mViewModel.setLimitDriverVisibility(false);
            return;
        }

        if (positionPackage.getLastCarLocation() == null) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "Limit get position failed");
            return;
        }
        final int currentCityCode = mapDataPackage.getAdCodeByLonLat(positionPackage.getLastCarLocation().getLongitude(),
                positionPackage.getLastCarLocation().getLatitude());

        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "get CurrentCity Limit " , currentCityCode);
        mCurrentCityCode = currentCityCode;
        mLicense = license;

        final RestrictedEndNumberParam restrictedParam = new RestrictedEndNumberParam();
        restrictedParam.setAdcodes(mCurrentCityCode);
        restrictedParam.setPlate(mLicense);
        limitEndNumberTaskId = restrictedPackage.queryRestrictedEndNumber(restrictedParam);
    }

    /***
     * 获取设置限行开关状态
     * @return 限行开关状态值
     */
    public boolean getAvoidLimit() {
        final String avoidLimitString = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT);
        final String avoidLimit = avoidLimitString.trim().toLowerCase();
        return "true".equals(avoidLimit);
    }

    /**
     * 设置底图中心点相对于屏幕偏移量.
     *
     */
    public void setMapCenterInScreen() {
        Logger.i(TAG, "setMapCenterInScreen");
        MapVisibleAreaInfo mapVisibleAreaInfo = getVisibleArea(MapVisibleAreaType.MAIN_AREA_CAR);
        if(!mViewModel.isFragmentStackNull()){
            if (mViewModel.getTopFragment(SettingFragment.class)){
                mapVisibleAreaInfo = getVisibleArea(MapVisibleAreaType.MAIN_AREA_SETTING);
            } else {
                mapVisibleAreaInfo = getVisibleArea(MapVisibleAreaType.MAIN_AREA_NAVING);
            }
        }
        MapMode mapModel = mapPackage.getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "setMapCenterInScreen (" , mapVisibleAreaInfo.getMleftscreenoffer() , "," , mapVisibleAreaInfo.getMtopscreenoffer() , ")" , ", mapMode: " , mapModel.ordinal());
        if(mapModel == MapMode.NORTH_2D){
            Logger.i(TAG, "setMapCenterInScreen NORTH_2D");
            mapPackage.setMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, mapVisibleAreaInfo.getMleftscreenoffer(), (mapVisibleAreaInfo.getMtopscreenoffer() * 3) / 4);
            return;
        }
        mapPackage.setMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, mapVisibleAreaInfo.getMleftscreenoffer(), mapVisibleAreaInfo.getMtopscreenoffer());
    }


    /**
     * 恢复底图中心点在屏幕上的位置.
     */
    public void resetMapCenterInScreen() {
        Logger.i(TAG, "resetMapCenterInScreen");
        setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
        goToCarPosition();
        setMapCenterInScreen();
        mViewModel.showOrHideSelfParkingView(false);
        stopCruise();
    }

    public void setFollowMode(MapType mapType, boolean bFollow){
        layerPackage.setFollowMode(mapType, bFollow);
    }

    @Override
    public void onMapClickBlank(MapType mapTypeId, float px, float py) {
        stopCruise();
    }

    @Override
    public void onMapClickLabel(MapType mapTypeId, ArrayList<MapLabelItemBean> labelItemBeans) {
        stopCruise();
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        mViewModel.updateOnMapScaleChanged(currentScale);
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP) {
            mSettingPackage.setSettingChangeCallback(mapTypeId.name(), this);
            layerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(positionPackage.getLastCarLocation().getLongitude(),
                    positionPackage.getLastCarLocation().getLatitude()));
            signalPackage.registerObserver(mapTypeId.name(), this);
            mapPackage.goToCarPosition(mapTypeId);
            MapVisibleAreaInfo mapVisibleAreaInfo = getVisibleArea(MapVisibleAreaType.MAIN_AREA_CAR);
            mapPackage.setMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, mapVisibleAreaInfo.getMleftscreenoffer(), mapVisibleAreaInfo.getMtopscreenoffer());
            naviPackage.registerObserver(mViewModel.mScreenId, this);
            // 注册监听
            cruisePackage.registerObserver(mViewModel.mScreenId, this);
            // 恢复偏好设置
            mapModelHelp.restoreSetting();
            mViewModel.initTimer();
            addFavoriteToMap();
            speedMonitor.registerSpeedCallBack();
            processExportCommand();
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        //触控态开始回车位倒计时
        startSelfParkingTimer();
        // 退出巡航
        stopCruise();

        //For Bury Point
        if (NaviStatusPackage.getInstance().isGuidanceActive()) {
            sendBuryPointForWakeup();
        }

        //三指飞屏 并将MapActivity推至后台
        openThreeFingerFlyingScreen(touchEvent);
    }

    private void openThreeFingerFlyingScreen(MotionEvent touchEvent) {
        //多指左滑打开仪表地图   多指右滑关闭仪表地图
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                if (touchEvent != null && aiwaysGestureManager != null) {
                    aiwaysGestureManager.onTouchEvent(touchEvent);
                }
            }
        });
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
        //选路页面不显示回自车位
        Logger.i("onMapClickPoi", NaviStatusPackage.getInstance().getCurrentNaviStatus());
        if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)) {
            return;
        }

        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.showOrHideSelfParkingView(true);
        }, 600);
    }

    @Override
    public void onReversePoiClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
    }

    @Override
    public void onMapModeChange(MapType mapTypeId, MapMode mapMode) {
        switch (mapMode) {
            case NORTH_2D -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_2d_north);
            case UP_3D -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_3d_north);
            default -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_2d_head);
        }
    }

    @Override
    public void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus currentImersiveStatus) {
        if (Logger.openLog) {
            Logger.d(TAG, "onImmersiveStatusChange: ", parkingViewExist(), ", currentImersiveStatus: ", currentImersiveStatus);
        }
        //是触控态的时候显示回车位   否则隐藏
//        if (Boolean.FALSE.equals(mViewModel.bottomNaviVisibility.get())) return;
        if (parkingViewExist()) {
            if (currentImersiveStatus == ImersiveStatus.TOUCH) {
                mViewModel.showOrHideSelfParkingView(true);
                layerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            } else if (currentImersiveStatus == ImersiveStatus.IMERSIVE) {
                mViewModel.showOrHideSelfParkingView(false);
                if (getTopFragment(PoiDetailsFragment.class)) {
                    closeFragment(true);
                }
                goToCarPosition();
                layerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, true);
                Logger.i(TAG, "---" + "goToCarPosition");
            }
        }

        if (getNaviStatus() == NaviStatus.NaviStatusType.NAVING && mSettingPackage.getAutoScale()) {
            Logger.i(TAG, "锁定比例尺，触摸态关闭动态比例尺，沉浸态开启比例尺！");
            boolean isOpen = !naviPackage.getFixedOverViewStatus() &&
                    !naviPackage.getClusterFixOverViewStatus() && currentImersiveStatus ==
                    ImersiveStatus.IMERSIVE && !naviPackage.getPreviewStatus();
            if (isOpen) {
                layerPackage.openDynamicLevel(mapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
            } else {
                layerPackage.closeDynamicLevel(mapTypeId);
            }
        }

        //导航中偶现 回车位出现问题
        if (TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.ROUTING) ||
                TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.NAVING) ||
                TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
            mViewModel.showOrHideSelfParkingView(false);
        }
    }

    private boolean parkingViewExist() {
        return getBottomNaviVisibility() || getTopFragment(PoiDetailsFragment.class) &&
                NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NO_STATUS)
                || getTopFragment(TrafficEventFragment.class);
    }

    private boolean getTopFragment(Class<? extends Fragment> targetClass) {
        return mViewModel.getTopFragment(targetClass);
    }


    public boolean getBottomNaviVisibility() {
        return mViewModel.bottomNaviVisibility.get();
    }

    private void startSelfParkingTimer() {
        cancelSelfParkingTimer();
        if (parkingViewExist()) {
            mSelfParkingTimer = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                cancelSelfParkingTimer();
                if (parkingViewExist()) {
                    ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
                    Logger.d("onFinish-startSelfParkingTimer-true");
                    if (getTopFragment(PoiDetailsFragment.class)) {
                        closeFragment(true);
                    }
                }
                Logger.d("onFinish-startSelfParkingTimer");
            }, 15, 15);
        }
    }

    public void showParkingView() {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        startSelfParkingTimer();
    }

    public void cancelSelfParkingTimer() {
        if (!ConvertUtils.isEmpty(mSelfParkingTimer)) {
            ThreadManager.getInstance().cancelDelayRun(mSelfParkingTimer);
            mSelfParkingTimer = null;
        }
    }

    public void goToCarPosition() {
        mapPackage.goToCarPosition(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType type) {
        // TODO 现在是跟随系统主题变化，只有黑夜和白天
        mapPackage.updateUiStyle(mapTypeId, type);
    }

    public void saveLastLocationInfo() {
        positionPackage.saveLocStorage();
    }

    @Override
    public void queryLimitResult(RouteRestrictionParam param) {
        if (Logger.openLog) {
            Logger.d(TAG, "queryLimitResult success!", "isMainThread:", (Looper.getMainLooper() == Looper.myLooper()));
        }
        // 限行信息查询成功后更新UI
        if (limitQueryTaskId == param.getMRestrictedArea().getMRequestId()) {
            mViewModel.updateLimitInfo(param);
        }
    }

    @Override
    public void queryLimitEndNumberResult(final TrafficRestrictResponseParam param) {
        if (Logger.openLog) {
            Logger.d(TAG, "queryLimitResult success!", "isMainThread:", (Looper.getMainLooper() == Looper.myLooper()));
        }
        // 限行信息查询成功后更新UI
        if (limitEndNumberTaskId == param.getTaskId()) {
            mViewModel.updateLimitEndNum(param);
        }
    }

    @Override
    public void onNaviStatusChange(String naviStatus) {
        mViewModel.onNaviStatusChange();
        mapModelHelp.onNaviStatusChange(naviStatus);
    }

    @Override
    public void onQueryTrafficEvent(MapType mapTypeId, PoiInfoEntity poiInfo) {
        if (TextUtils.equals(mapTypeId.name(), mViewModel.mScreenId)) {
            IMapPackageCallback.super.onQueryTrafficEvent(mapTypeId, poiInfo);
            mViewModel.openTrafficDetailFragment(poiInfo);
            ThreadManager.getInstance().postDelay(() -> {
                mViewModel.showOrHideSelfParkingView(true);
            }, 600);
        }
    }

    @Override
    public void onVoiceOpenPage(MapType mapTypeId, Bundle bundle) {
        if (MapType.MAIN_SCREEN_MAIN_MAP == mapTypeId && null != bundle && null != mViewModel) {
            final int voicePage = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE);
            switch (voicePage) {
                case IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH:
                    //关键字搜索
                    final String keyword = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, "");
                    if (null == keyword || keyword.isEmpty()) {
                        return;
                    }
                    final boolean isEnd = bundle.getBoolean(IVrBridgeConstant.VoiceIntentParams.IS_END, false);
                    mViewModel.toSearchResultFragment(keyword, isEnd);
                    break;
                case IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH:
                    //周边搜
                    final String aroundKey = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, "");
                    if (TextUtils.isEmpty(aroundKey)) {
                        return;
                    }
                    //半径
                    final int radius = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.AROUND_RADIUS, 5000);
                    final Parcelable parcelable = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT);
                    if (parcelable instanceof GeoPoint centerPoint) {
                        //中心点
                        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                        poiInfoEntity.setPoint(centerPoint);
                        mViewModel.toSearchAroundFragment(aroundKey, radius, poiInfoEntity);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.ROUTING:
                    //路线规划
                    final Serializable serializable = bundle.getSerializable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST);
                    if (serializable instanceof RouteSpeechRequestParam param) {
                        mViewModel.toRouteFragment(param);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.SEARCH_HISTORY:
                    //搜索历史记录
                    mViewModel.openSearchFragment.call();
                    break;
                case IVrBridgeConstant.VoiceIntentPage.FAVORITE_PAGE:
                    //收藏夹
                    mViewModel.openCollectFragment();
                    break;
                case IVrBridgeConstant.VoiceIntentPage.START_NAVIGATION:
                    //开始导航
                    mViewModel.startNaviForRouteOver();
                    break;
                case IVrBridgeConstant.VoiceIntentPage.POI_DETAIL:
                    //打开poi详情
                    final PoiInfoEntity poiInfo = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.POI_DETAIL_INFO);
                    if (null != poiInfo) {
                        mViewModel.toPoiDetailFragment(poiInfo);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.HOME_COMPANY_SET:
                    //设置家和公司，传入语音搜索关键字，直接发起搜索
                    final int type = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.HOME_COMPANY_TYPE);
                    final String homeCompanyKeyword = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD);
                    if (!TextUtils.isEmpty(homeCompanyKeyword)) {
                        mViewModel.toFavoriteFragment(type, homeCompanyKeyword);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE:
                    //选择路线
                    ThreadManager.getInstance().execute(() -> {
                        if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)
                                || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
                            final int routeIndex = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, 0);
                            mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, routeIndex);
                        }
                    });
                    break;
                case IVrBridgeConstant.VoiceIntentPage.ALONG_SEARCH:
                    final String passBy = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD);
                    if (!TextUtils.isEmpty(passBy)) {
                        mViewModel.toAlongWayFragment(passBy);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.CLOSE_CURRENT_PAGE:
                    //关闭当前页面
                    closeFragment(true);
                    break;
                case IVrBridgeConstant.VoiceIntentPage.MOVE_TO_BACK:
                    //应用退到后台
                    mViewModel.moveToBack();
                    break;
                case IVrBridgeConstant.VoiceIntentPage.COLLECT_COMMON:
                    //收藏普通poi
                    final String favoritePoi = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, "");
                    if (null == favoritePoi || favoritePoi.isEmpty()) {
                        return;
                    }
                    mViewModel.toFavoriteFragment(0, favoritePoi);
                    break;
                default:
                    break;
            }
        }
    }

    public String getNaviStatus() {
        return mNaviStatusPackage.getCurrentNaviStatus();
    }

    private ContinueNaviDialog mContinueNaviDialog;

    /***校验继续导航***/
    public void checkContinueNavi(Context context) {
        try {
            if (mHistoryManager != null) {
                mUncompletedNavi = mHistoryManager.getUncompletedNavi();
                Logger.i(TAG, "uncompletedNavi " , mUncompletedNavi);
                if (mUncompletedNavi == null) return;
                Date mUpdateTime = mUncompletedNavi.getMUpdateTime();
                if (mUpdateTime == null) {
                    return;
                }
                long targetTimeMillis = mUpdateTime.getTime();
                // 获取当前时间的毫秒值
                long currentTimeMillis = System.currentTimeMillis();
                // 计算时间差的绝对值（毫秒）
                long timeDifference = Math.abs(currentTimeMillis - targetTimeMillis);
                // 判断是否恰好相差24小时
                if (timeDifference >= MILLIS_IN_24_HOURS) {
                    Logger.i(TAG, "uncompletedNavi over 24h");
                    onCancelContinueNaviClick();
                    return;
                }
                if (TextUtils.isEmpty(mUncompletedNavi.getMEndPoint())) {
                    Logger.e(TAG, "getMEndPoint null");
                    return;
                }
                if (TextUtils.isEmpty(mUncompletedNavi.getMEndPoiName())) {
                    Logger.e(TAG, "getMEndPoiName null");
                    return;
                }
                mViewModel.continueNavi(mUncompletedNavi.getMEndPoiName());
            }
        } catch (WindowManager.BadTokenException e) {
            Logger.e(TAG, "showNavTipDialog e-->" + e.getMessage());
            onCancelContinueNaviClick();
        } catch (Exception exception) {
            Logger.e(TAG, "showNavTipDialog exception-->" + exception.getMessage());
            onCancelContinueNaviClick();
        }
    }

    public void onContinueNaviClick() {
        if (mUncompletedNavi == null) {
            return;
        }
        onCancelContinueNaviClick();
        String midPoint = mUncompletedNavi.getMViaPoint();
        if (!ConvertUtils.isEmpty(midPoint)) {
            RouteSpeechRequestParam routeSpeechRequestParam = GsonUtils.fromJson(midPoint, RouteSpeechRequestParam.class);
            routeSpeechRequestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
            bundle.putSerializable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, routeSpeechRequestParam);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            RoutePackage.getInstance().requestRouteFromSpeech(routeSpeechRequestParam);
        }
    }

    public void onCancelContinueNaviClick() {
        if (mUncompletedNavi == null) {
            return;
        }
        mUncompletedNavi.setMIsCompleted(true);
        mHistoryManager.insertOrReplace(mUncompletedNavi);
    }

    @Override
    public void onCarClick(MapType mapType, GeoPoint geoPoint) {
        PoiInfoEntity poiInfo = new PoiInfoEntity();
        poiInfo.setPoiType(AutoMapConstant.SearchType.GEO_SEARCH);
        poiInfo.setPoint(geoPoint);

        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CAR_CLICK);
        PoiDetailsFragment fragment = new PoiDetailsFragment();
        addPoiDetailsFragment(fragment, bundle);
        Logger.i("onCarClick", ThreadManager.getInstance().getCurrentThread().getName());
    }

    @Override
    public void onFavoriteClick(MapType mapTypeId, PoiInfoEntity poiInfo) {
        Logger.i("onFavoriteClick", ThreadManager.getInstance().getCurrentThread().getName());
        ThreadManager.getInstance().postUi(() -> {
            Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
            bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
            PoiDetailsFragment fragment = new PoiDetailsFragment();
            addPoiDetailsFragment(fragment, bundle);
        });
    }

    @Override
    public void onFlyLineMoveEnd(MapType mapTypeId, GeoPoint descPoint) {
        Logger.d(TAG, "onMapMoveEnd-MapModel");
        PoiInfoEntity entity = new PoiInfoEntity();
        entity.setPoint(descPoint);
        mViewModel.toPoiDetailFragment(entity);
    }

    @Override
    public void startCruise() {
        // 判断是否满足进入巡航的条件 1.是否有其它View正在显示 2.是否已开启巡航
        if (!StackManager.getInstance().isFragmentStackNull(mViewModel.mScreenId)) {
            Logger.i(TAG, "当前有正在显示的Fragment,开启巡航失败！");
            // 如果此刻已处于巡航态，结束巡航，静默结束
            if (TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
                mNaviStatusPackage.setNaviStatus(NaviStatus.NaviStatusType.NO_STATUS);
            }
            return;
        }
        if (TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
            Logger.i(TAG, "巡航已开启，无需重复开启！");
            mViewModel.showOrHiddenCruise(true);
            return;
        }
        final boolean isSuccess = cruisePackage.startCruise();
        if (isSuccess) {
            String isOpen = mSettingPackage.getValueFromDB(SettingController.KEY_SETTING_IS_AUTO_RECORD);
            if (isOpen != null && isOpen.equals("true")) {
                Logger.i(TAG, "开始打点");
                @SuppressLint("HardwareIds") final String androidId = Settings.Secure.getString(
                        AppCache.getInstance().getMContext().getContentResolver(),
                        Settings.Secure.ANDROID_ID
                );
                final long curTime = System.currentTimeMillis();
                mFilename = curTime + "_" + 0 + "_" + androidId;
                UserTrackPackage.getInstance().startGpsTrack(GBLCacheFilePath.SYNC_PATH + "/403", mFilename, 2000);
            }
            final SoundInfoEntity soundInfo = new SoundInfoEntity();
            soundInfo.setText(AppCache.getInstance().getMApplication().getString(R.string.step_into_cruise));
            naviPackage.onPlayTTS(soundInfo);
            mViewModel.showToast(R.string.step_into_cruise);
            mViewModel.setCruiseMuteOrUnMute(
                    Boolean.parseBoolean(mSettingPackage.getValueFromDB(SettingController.KEY_SETTING_CRUISE_BROADCAST))
            );
            mapModelHelp.setCruiseScale();
        }
        mViewModel.showOrHiddenCruise(isSuccess);
    }

    public void stopCruise() {
        // 判断是否已开启巡航，已开启再去关闭
        if (!TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
//            Logger.i(TAG, "巡航未开启，无需关闭！");
            // 如果巡航UI正在显示，关闭一下
            if (mViewModel.isCruiseUiVisible()) {
                mViewModel.showOrHiddenCruise(false);
            }
            return;
        }
        final boolean isSuccess = cruisePackage.stopCruise();
        if (isSuccess) {
            UserTrackPackage.getInstance().closeGpsTrack(GBLCacheFilePath.SYNC_PATH + "/403", mFilename);
            final SoundInfoEntity soundInfo = new SoundInfoEntity();
            soundInfo.setText(AppCache.getInstance().getMApplication().getString(R.string.step_exit_cruise));
            naviPackage.onPlayTTS(soundInfo);
            mViewModel.showToast(R.string.step_exit_cruise);
            mViewModel.showOrHiddenCruise(false);
        }
    }

    @Override
    public void onGearChanged(int gear) {
        Logger.i(TAG, "onGearChanged:" , gear);
        switch (gear) {
            case 0 -> {
                // 停车
                stopCruise();
            }
            default -> {

            }
        }
    }

    @Override
    public void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(cruiseInfoEntity);
        // 巡航-电子眼信息
        mViewModel.updateCruiseRoadName(cruiseInfoEntity);
    }

    @Override
    public void onUpdateCruiseInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(isShowLane, laneInfoEntity);
        Logger.w(TAG, "onUpdateCruiseInfo:" + isShowLane);
        // 巡航-车道信息
        mViewModel.updateCruiseLanInfo(isShowLane, laneInfoEntity);
    }

    /***
     * 巡航下设置是否播报
     * @param isOpen
     */
    public void setCruiseVoice(boolean isOpen) {
        mSettingPackage.setCruiseBroadcastOpen(isOpen);
    }

    @Override
    public void onSettingChanged(String key, String value) {
        switch (key) {
            case SettingController.KEY_SETTING_CRUISE_BROADCAST:
                final boolean isOpen = Boolean.parseBoolean(value);
                mViewModel.setCruiseMuteOrUnMute(isOpen);
                naviPackage.setCruiseVoiceIsOpen(isOpen);
                break;
            case SettingController.KEY_SETTING_GUIDE_AVOID_LIMIT:
                getCurrentCityLimit();
                break;
            case SettingController.KEY_SETTING_FAVORITE_POINT:
                showOrHideFavoriteToMap(value);
                break;
            default:
                break;

        }
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        int scale = mapPackage.getCurrentZoomScale(MapType.MAIN_SCREEN_MAIN_MAP);
        float size = mapPackage.getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "onNaviInfo:", "scale:" , scale, "size:" , size);
    }

    @Override
    public void onNaviArrive(long traceId, int naviType) {
        Logger.i(TAG, "onNaviArrive");
    }

    @Override
    public void onNaviStop() {
        Logger.i(TAG, "onNaviStop:");
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
    }

    @Override
    public void notifyAimPoiPushMessage(final MsgPushInfo msg) {
        if (msg == null) {
            Logger.e(TAG, "notifyAimPoiPushMessage is null");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            if (Logger.openLog) {
                Logger.i(TAG, "notifyAimPoiPushMessage ", GsonUtils.toJson(msg));
            }
            final PhoneAddressDialog phoneAddressDialog = new PhoneAddressDialog(
                    StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
            phoneAddressDialog.setTitle(msg.getName());
            phoneAddressDialog.setDialogClickListener(new IBaseDialogClickListener() {
                @Override
                public void onCommitClick() {
                    mViewModel.startRoute(getPoiInfoEntityFromPushMessage(msg));
                }
            });
            phoneAddressDialog.showDialog();
        });
    }

    @Override
    public void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.i(TAG, "notifyAimRoutePushMessage " , routeMsgPushInfo.getMName());
            switch (getNaviStatus()) {
                case NaviStatus.NaviStatusType.SELECT_ROUTE,
                     NaviStatus.NaviStatusType.ROUTING,
                     NaviStatus.NaviStatusType.NAVING:
                    mRoutePackage.requestRouteRestoration(routeMsgPushInfo, MapType.MAIN_SCREEN_MAIN_MAP);
                    break;
                case NaviStatus.NaviStatusType.NO_STATUS,
                     NaviStatus.NaviStatusType.CRUISE,
                     NaviStatus.NaviStatusType.LIGHT_NAVING:
                    Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                    Bundle bundle = new Bundle();
                    bundle.putSerializable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE, routeMsgPushInfo);
                    addFragment((BaseFragment) fragment, bundle);
                    break;
                default:
                    break;
            }
        });
    }

    /**
     * 预测服务
     * result result
     */
    @Override
    public void onInit(int result) {

    }

    /**
     * 预测服务
     * result result
     */
    @Override
    public void onSetLoginInfo(int result) {

    }

    /**
     * 预测服务
     * data data
     */
    @Override
    public void onForecastArrivedData(ForecastArrivedDataInfo data) {
        //判断是否有家或者公司的数据
        if (AutoMapConstant.HomeCompanyType.HOME == mCompanyOrHomeType) {
            OftenArrivedItemInfo mHomeInfo = data.getHome();
            if (!ConvertUtils.isEmpty(mHomeInfo) && !ConvertUtils.isEmpty(mHomeInfo.getWstrAddress())) {
                mViewModel.showForecastDialog(mCompanyOrHomeType, mHomeInfo);
            } else {
                mViewModel.toHomeFragment();
            }
        } else {
            OftenArrivedItemInfo mCompanyInfo = data.getCompany();
            if (!ConvertUtils.isEmpty(mCompanyInfo) && !ConvertUtils.isEmpty(mCompanyInfo.getWstrAddress())) {
                mViewModel.showForecastDialog(mCompanyOrHomeType, mCompanyInfo);
            } else {
                mViewModel.toCompanyFragment();
            }
        }
    }


    public void getOnlineForecastArrivedData(int type) {
        mCompanyOrHomeType = type;
        //登陆了才去预测数据
        if (AccountPackage.getInstance().reallyLogin()) {
            // 获取在线预测常去地点
            ForecastArrivedDataInfo param = new ForecastArrivedDataInfo();
            param.setLevel((int) mapPackage.getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP)); // 图面比例尺级别
            //先拿到经纬度
            LocInfoBean locInfoBean = positionPackage.getLastCarLocation();
            if (!ConvertUtils.isEmpty(locInfoBean)) {
                param.setUserLoc(new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude()));
                int adCode = mapDataPackage.getAdCodeByLonLat(locInfoBean.getLongitude(), locInfoBean.getLatitude());
                param.setAdCode(String.valueOf(adCode)); // 所在城市对应 adcode
            }
            AccountProfileInfo accountProfileInfo = AccountPackage.getInstance().getUserInfo();
            if (!ConvertUtils.isEmpty(accountProfileInfo)) {
                param.setUserId(accountProfileInfo.getUid()); // 登录用户UID
            }
            mforCastPackage.getOnlineForecastArrivedData(param);
        } else {
            if (AutoMapConstant.HomeCompanyType.HOME == type) {
                mViewModel.toHomeFragment();
            } else {
                mViewModel.toCompanyFragment();
            }
        }
    }

    public void addHomeOrCompanyInfoToSetting(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        int favoriteType = (type == AutoMapConstant.HomeCompanyType.HOME) ? 1 : 2;
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(oftenArrivedItemInfo.getWstrPoiID());
        poiInfoEntity.setName(oftenArrivedItemInfo.getWstrPoiName());
        poiInfoEntity.setAddress(oftenArrivedItemInfo.getWstrAddress());
        poiInfoEntity.setPoint(oftenArrivedItemInfo.getStDisplayCoord());
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setMCommonName(favoriteType);
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        behaviorPackage.addFavorite(poiInfoEntity, favoriteType);
        sendBuryPointForAddFavorite(poiInfoEntity.getName(), favoriteType);

        if (favoriteType == 1) {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_home));
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_company));
        }

    }

    @HookMethod
    private void sendBuryPointForAddFavorite(final String name, final int type) {
        String eventName = "";
        String key = BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION;
        switch (type) {
            case 1:
                eventName = BuryConstant.EventName.AMAP_HOME_SAVE;
                break;
            case 2:
                eventName = BuryConstant.EventName.AMAP_WORK_SAVE;
                break;
        }
        BuryPointController.getInstance().setEventName(eventName);
        BuryProperty buryProperty = new BuryProperty.Builder()
                .setParams(key, name)
                .build();
        BuryPointController.getInstance().setBuryProps(buryProperty);
    }


    /**
     * 手机推送地址转化成PoiInfo
     *
     * @param msg
     * @return entity
     */
    private PoiInfoEntity getPoiInfoEntityFromPushMessage(final MsgPushInfo msg) {
        final PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setName(msg.getName());
        poiInfoEntity.setAddress(msg.getAddress());
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        poiInfoEntity.setPid(msg.getPoiId());
        GeoPoint geoPoint = new GeoPoint();
        geoPoint.setLon(ConvertUtils.transCityLatAndLon(msg.getLon()));
        geoPoint.setLat(ConvertUtils.transCityLatAndLon(msg.getLat()));
        poiInfoEntity.setPoint(geoPoint);
        return poiInfoEntity;
    }

    public boolean switchMapMode() {
        return mapPackage.switchMapMode(mapModelHelp.getMapTypeId());
    }

    public String getCurrentMapModelText() {
        MapMode mode = mapPackage.getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        String carModel = "";
        switch (mode) {
            case NORTH_2D:
                carModel = ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_north_2d);
                break;
            case UP_2D:
                carModel = ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_up_2d);
                break;
            case UP_3D:
                carModel = ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_up_3d);
                break;
        }
        return carModel;
    }


    /**
     * 地图是否15天未更新
     */
    public boolean offlineMap15Day() {
        final String time = commonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_MAP_CHECK);
        if (!TextUtils.isEmpty(time)) {
            //超过15天
            final boolean after15Day = messageCenterHelper.isNotConnectedFor15Days(Long.parseLong(time));
            if (after15Day) {
                commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK, String.valueOf(System.currentTimeMillis()));
                Logger.i("offlineMap15Day", "+++");
                return true;
            }
        } else {
            commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK, String.valueOf(System.currentTimeMillis()));
            Logger.i("offlineMap15Day", "----");
        }
        return false;
    }

    public boolean showNdGoHomeView() {
        return mViewModel.showNdGoHomeView();
    }

    public void sendReqHolidayList() {
        restrictedPackage.sendReqHolidayList();
    }

    @Override
    public void isHoliday(boolean holiday) {
        //不是节假日 才弹出view
        Logger.i("isHoliday", "----");
        if (!holiday) {
            loadNdGoHomeView();
        }
    }

    private void loadNdGoHomeView() {
        if (mViewModel.showNdGoHomeView()) {
            String key = commonManager.getValueByKey(UserDataCode.MAP_ND_GO_HOME_KEY);
            String currentTime = TimeUtils.getInstance().getCurrentDate().replace("-", "");
            if (ConvertUtils.isEmpty(key) || !ConvertUtils.equals(key, currentTime)) {
                commonManager.insertOrReplace(UserDataCode.MAP_ND_GO_HOME_KEY, currentTime);

                //是否在上班时间段内  在家附近
                LocInfoBean locInfoBean = positionPackage.getLastCarLocation();
                boolean workHours = TimeUtils.isCurrentTimeInSpecialRange(true);
                GeoPoint nearByHome = mViewModel.nearByHome(true);
                GeoPoint nearByCompany = mViewModel.nearByHome(false);
                if (workHours && !ConvertUtils.isEmpty(nearByHome) && !ConvertUtils.isEmpty(nearByCompany) && !ConvertUtils.isEmpty(locInfoBean)) {
                    //判断距离是否大于等于1km 小于等于50km 去公司
                    boolean distanceCompany = calcStraightDistance(nearByHome, locInfoBean);
                    if (distanceCompany) {
                        mViewModel.loadNdOfficeTmc(false);
                    }
                    return;
                }

                //是否在下班时间段内  在公司附近
                boolean endofWorkHours = TimeUtils.isCurrentTimeInSpecialRange(false);
                if (endofWorkHours && !ConvertUtils.isEmpty(nearByHome) && !ConvertUtils.isEmpty(nearByCompany) && !ConvertUtils.isEmpty(locInfoBean)) {
                    //判断距离是否大于等于1km 小于等于50km 回家
                    boolean distanceHome = calcStraightDistance(nearByCompany, locInfoBean);
                    if (distanceHome) {
                        mViewModel.loadNdOfficeTmc(true);
                    }
                }
            }
        }
    }

    private boolean calcStraightDistance(GeoPoint nearByHome, LocInfoBean locInfoBean) {
        GeoPoint locGeoPoint = new GeoPoint(locInfoBean.getLongitude(), locInfoBean.getLatitude());
        double distance = layerPackage.calcStraightDistance(nearByHome, locGeoPoint);
        return (distance >= 1000 && distance <= 50000);
    }

    /**
     * 离线地图是否45天未更新
     */
    public boolean offlineMap45Day() {
        if (Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())) {
            messageCenterHelper.saveLast45ConnectedTime();
            return false;
        } else {
            //45天未联网
            return messageCenterHelper.isNotConnectedFor45Days();
        }
    }

    /**
     * 是否显示限行
     */
    public boolean showSameDayLimit() {
        final String limitTime = commonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_LIMIT_TIME);
        if (!TextUtils.isEmpty(limitTime)) {
            //在判断是否是同一天  同一天的也不显示
            final String currentDay = TimeUtils.convertYMD();
            if (!currentDay.equals(limitTime)) {
                commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, currentDay);
                return true;
            }
        } else {
            commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, TimeUtils.convertYMD());
            return true;
        }
        return false;
    }


    /**
     * 推送消息
     *
     * @param messageCenterInfo 消息
     */
    public void managerMessage(MessageCenterInfo messageCenterInfo) {
//        final MessageCenterInfo messageCenterInfo = messageCenterHelper.manageMessage(messageCenterType);
        if (messageCenterInfo != null) {
            ThreadManager.getInstance().postUi(() -> {
                messageCenterManager.pushMessage(messageCenterInfo);
            });
        }
    }

    /**
     * 删除消息
     */
    public void deleteMessage() {
        ThreadManager.getInstance().postUi(() -> {
            messageCenterManager.deleteMessage();
        });
    }


    private void addFavoriteToMap() {
        //如果设置不显示收藏点  不需要加载数据耗性能
        final String isFavoritePointStr = settingManager.getValueByKey(SettingController.KEY_SETTING_FAVORITE_POINT);
        final boolean isFavoritePoint = TextUtils.equals(isFavoritePointStr, "true");
        if (!isFavoritePoint) {
            return;
        }
        showOrHideFavoriteToMap(String.valueOf(isFavoritePoint));
    }

    /**
     * 添加收藏
     */
    public void showOrHideFavoriteToMap(String value) {
        //如果设置不显示收藏点  需要隐藏
        if (Boolean.parseBoolean(value)) {
            if (behaviorPackage.getFavoriteDataUpdatedStatus()) {
                LayerItemUserFavorite layerItemUserFavorite = new LayerItemUserFavorite();
                ArrayList<PoiInfoEntity> allPoiInfoEntities = new ArrayList<>();

                ArrayList<PoiInfoEntity> poiInfoEntities = behaviorPackage.getFavoritePoiData();
                PoiInfoEntity homePoiInfoEntity = behaviorPackage.getHomeFavoriteInfo();
                PoiInfoEntity companyFavoriteInfo = behaviorPackage.getCompanyFavoriteInfo();

                if (!ConvertUtils.isEmpty(poiInfoEntities)) {
                    allPoiInfoEntities.addAll(poiInfoEntities);
                }
                if (!ConvertUtils.isEmpty(homePoiInfoEntity)) {
                    allPoiInfoEntities.add(homePoiInfoEntity);
                }
                if (!ConvertUtils.isEmpty(companyFavoriteInfo)) {
                    allPoiInfoEntities.add(companyFavoriteInfo);
                }
                if (ConvertUtils.isEmpty(allPoiInfoEntities)) {
                    return;
                }
                layerPackage.clearFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP);

                layerItemUserFavorite.setMSimpleFavoriteList(allPoiInfoEntities);
                layerPackage.addLayerItemOfFavorite(MapType.MAIN_SCREEN_MAIN_MAP, layerItemUserFavorite);
                layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
                behaviorPackage.setFavoriteDataUpdatedStatus(false);
                Logger.d("showOrHideFavoriteToMap", "showall");
            } else {
                layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP, true);
                Logger.d("showOrHideFavoriteToMap", "showvisible");
            }

        } else {
            layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP, false);
        }
    }

    @Override
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo) {
        mViewModel.onMessageInfoNotifyCallback(messageCenterInfo);

        sendBuryPointForPopup(messageCenterInfo != null ? messageCenterInfo.getMsgTitle() : "");
    }

    @Override
    public void onMessageInfoRemoveCallback() {
        mViewModel.onMessageInfoRemoveCallback();
    }

    @Override
    public void onRouteTMCInfo(RouteTMCParam param) {
        if (ConvertUtils.isEmpty(param)) {
            return;
        }
        Logger.d(TAG, "onRouteTMCInfo： " , GsonUtils.toJson(param));
        mViewModel.setTMCView(param);
    }

    public boolean getHomeCompanyDisplay() {
        String value = settingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
        if (TextUtils.isEmpty(value)) {
            value = "true";
        }
        return Boolean.parseBoolean(value);
    }

    public void refreshHomeOfficeTMC(final boolean isHome) {
        mRoutePackage.refreshHomeOfficeTMC(MapType.MAIN_SCREEN_MAIN_MAP, isHome);
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
     *
     * @return 动力类型
     */
    public int powerType() {
        return mCalibrationPackage.powerType();
    }

    @Override
    public void onTipDialog(String status) {
        Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "status: " , status);
        if (ConvertUtils.isNull(mViewModel)) return;
        mViewModel.chargePreTipDialog(status);
    }

    public void cancelTimeTick() {
        searchPackage.cancelTimeTick();
    }

    /**
     * 检查高德地图权限是否申请或过期
     */
    public void checkAuthorizationExpired() {
        if (authorizationRequestDialog != null && authorizationRequestDialog.isShowing()) {
            return;
        }
        final String endDate = mSettingPackage.getEndDate();
        boolean isShowDialog = false;
        if (!TextUtils.isEmpty(endDate)) {
            final SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy年M月d日", Locale.getDefault());
            try {
                final Date date = dateFormat.parse(endDate);
                if (date != null) {
                    isShowDialog = date.getTime() < System.currentTimeMillis();
                }
            } catch (ParseException e) {
                throw new RuntimeException(e);
            }
        } else {
            isShowDialog = true;
        }
        if (!isShowDialog) {
            return;
        }
        authorizationRequestDialog = new AuthorizationRequestDialog(
                StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
        authorizationRequestDialog.setEndDate(endDate);
        authorizationRequestDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                final LocalDate currentDate = LocalDate.now();
                final LocalDate oneYearLater = currentDate.plusYears(1);
                final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy年M月d日");
                mSettingPackage.setPrivacyStatus(true);
                mSettingPackage.setEndDate(oneYearLater.format(formatter));
                SettingUpdateObservable.getInstance().notifySettingChanged(SettingController.KEY_SETTING_PRIVACY_STATUS, true);
            }
        });
        mViewModel.showAuthorizationRequestDialog(authorizationRequestDialog);
    }

    @Override
    public void onSearchResult(int taskId, int errorCode, String message, SearchResultEntity searchResultEntity) {

    }

    @Override
    public void onSilentSearchResult(final int taskId, final int errorCode, final String message,
                                     final SearchResultEntity searchResultEntity) {
    }

    @Override
    public void onMarkClickCallBack(final PoiInfoEntity poiInfoEntity) {
        SearchResultCallback.super.onMarkClickCallBack(poiInfoEntity);
        if (ConvertUtils.isEmpty(poiInfoEntity)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "onMarkClickCallBack: poiInfoEntity is null");
            return;
        }

        final Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfoEntity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_KEYWORD);
        addPoiDetailsFragment(new PoiDetailsFragment(), bundle);
    }

    @Override
    public void onVoicePoiSort(final MapType mapTypeId, final String sortValue) {
    }

    public boolean checkPopGuideLogin() {
        if (mAccountPackage.isLogin() || settingManager.getValueByKey(SettingController.GUIDE_LOGIN_IS_CANCEL).equals("1")) {
            return false;
        }

        String lastTimeStr = commonManager.getValueByKey(UserDataCode.GUIDE_LOGIN_LAST_TIME);
        if (TextUtils.isEmpty(lastTimeStr)) {
            return true;
        }

        long lastTime = Long.parseLong(lastTimeStr);
        long currentTime = System.currentTimeMillis();
        return TimeUtils.isTimeDifferenceGreaterThanOneWeek(lastTime, currentTime);
    }

    public void guideLoginBind() {
        addFragment(new AccountQRCodeLoginFragment(), null);
    }

    public void guideLoginClose() {
        commonManager.insertOrReplace(UserDataCode.GUIDE_LOGIN_LAST_TIME, String.valueOf(System.currentTimeMillis()));
    }

    public void guideLoginCancel() {
        settingManager.insertOrReplace(SettingController.GUIDE_LOGIN_IS_CANCEL, "1");
        commonManager.insertOrReplace(UserDataCode.GUIDE_LOGIN_LAST_TIME, "");
    }

    public void mfcChangeZoom(boolean zoom) {
        if (Boolean.TRUE.equals(zoom)) {
            mapPackage.amplifyLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        } else {
            mapPackage.reduceLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    }

    public void mfcMoveMap(MfcController mfcController, int moveDis) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        //触控态开始回车位倒计时
        startSelfParkingTimer();
        // 退出巡航
        stopCruise();
        mapPackage.mfcMoveMap(MapType.MAIN_SCREEN_MAIN_MAP, mfcController, moveDis);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_NAVI_MAP_MANUAL_WAKEUP)
    private void sendBuryPointForWakeup() {
        //Empty body
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_POPUP)
    private void sendBuryPointForPopup(final String msg) {
        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, msg)
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }

    public void addSceneGoHomeCallBack(int type) {
        mViewModel.addSceneGoHomeCallBackVieModel(type);
    }

    public void addGestureListening() {
        aiwaysGestureManager = new AiWaysGestureManager(AppCache.getInstance().getMContext(), new AiwaysGestureListener() {
            @Override
            public boolean mutiFingerSlipAction(GestureEvent gestureEvent, float startX, float startY, float endX, float endY, float velocityX, float velocityY) {
                if ((gestureEvent == GestureEvent.THREE_GINGER_LEFT_SLIP)) {
                    Logger.d(TAG, "三指左滑=====||||");
                    ThreeFingerFlyingScreenManager.getInstance().triggerFlyingScreen(true);
                    if (StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()).isTaskRoot() && ProcessManager.isAppInForeground()){
                        StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()).moveTaskToBack(true);
                    }
                    return true;
                } else if ((gestureEvent == GestureEvent.THREE_GINGER_RIGHT_SLIP)) {
                    Logger.d(TAG, "三指右滑=====||||");
                    ThreeFingerFlyingScreenManager.getInstance().triggerFlyingScreen(false);
                    return true;
                }
                return false;
            }
        });
    }

    @Override
    public void onScreenModeChanged(ScreenType screenType, String jsonPath) {
        mapVisibleAreaDataManager.loadData(jsonPath);
        mViewModel.onScreenModeChanged(screenType);
    }

    /**
     * 主图加载完成后执行外部应用通过对外交互传递过来的指令.
     */
    private void processExportCommand() {
        final int intentPage = ExportIntentParam.getIntentPage();
        if(INaviConstant.OpenIntentPage.NONE == intentPage) {
            return;
        }

        ExportIntentParam.setIntentPage(INaviConstant.OpenIntentPage.NONE);
        if (null != mViewModel) {
            mViewModel.processPageIntent(intentPage);
        }
    }

    private void startCloseTmcTimerWithoutNetwork() {
        cancelCloseTmcTimerWithoutNetwork();
        mCloseTmcTimer = ThreadManager.getInstance().asyncDelayWithResult(() -> {
            cancelCloseTmcTimerWithoutNetwork();
            Logger.d("onFinish-startCloseTmcTimerWithoutNetwork-true");
            mapPackage.setTrafficStatesWithoutNetwork(MapType.MAIN_SCREEN_MAIN_MAP, false);
            Logger.d("onFinish-startCloseTmcTimerWithoutNetwork");
        }, 300);
    }

    private void cancelCloseTmcTimerWithoutNetwork(){
        if (!ConvertUtils.isEmpty(mCloseTmcTimer)) {
            ThreadManager.getInstance().cancelDelayRun(mCloseTmcTimer);
            mCloseTmcTimer = null;
        }
    }

    public void openGuideFragment() {
        if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(),
                NaviStatus.NaviStatusType.NAVING)) {
            // Activity被意外destroy需要恢复页面的时候Fragment栈一定是空的
            if (mViewModel.isFragmentStackNull()) {
                addFragment(new NaviGuidanceFragment(), null);
            }
        }
    }
}