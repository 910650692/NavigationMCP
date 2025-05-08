package com.fy.navi.hmi.map;


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
import com.android.utils.DeviceUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.fsa.FsaConstant;
import com.fy.navi.fsa.MyFsaService;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.account.AccountQRCodeLoginFragment;
import com.fy.navi.hmi.message.MessageCenterHelper;
import com.fy.navi.hmi.navi.AuthorizationRequestDialog;
import com.fy.navi.hmi.navi.ContinueNaviDialog;
import com.fy.navi.hmi.navi.PhoneAddressDialog;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.GBLCacheFilePath;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapType;
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
import com.fy.navi.utils.ClusterActivityOffOnUtils;
import com.fy.navi.vrbridge.IVrBridgeConstant;

import java.io.Serializable;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
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
        MsgPushCallBack, IGuidanceObserver, MessageCenterCallBack, IRouteResultObserver , ILayerPackageCallBack
        , ForecastCallBack , SearchResultCallback {
    private final MapPackage mapPackage;
    private final LayerPackage layerPackage;
    private final PositionPackage positionPackage;
    private final MapDataPackage mapDataPackage;
    private final HistoryManager mHistoryManager;
    private final AosRestrictedPackage restrictedPackage;
    private CommonManager commonManager;
    private static final String TAG = "MapModel";
    private long limitQueryTaskId;
    private String mExtraKeyword;
    private int mCurrentCityCode;
    private int mCompanyOrHomeType;
    private String mLicense = "";
    private boolean mAvoidLimit = false;
    private String mFilename = "";
    private ScheduledFuture mSelfParkingTimer;//回车位倒计时
    private SettingPackage settingPackage;
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
    public MapModel() {
        mCallbackId = UUID.randomUUID().toString();
        mapPackage = MapPackage.getInstance();
        layerPackage = LayerPackage.getInstance();
        positionPackage = PositionPackage.getInstance();
        mapDataPackage = MapDataPackage.getInstance();
        restrictedPackage = AosRestrictedPackage.getInstance();
        settingPackage = SettingPackage.getInstance();
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
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mapPackage.unBindMapView(mViewModel.getMapView());
        speedMonitor.unInit();
        mapModelHelp.unInit();
        cruisePackage.unregisterObserver(mViewModel.mScreenId);
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "loadMapView");
        mapPackage.initMapView(mapSurfaceView);
        mapPackage.registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        layerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        ImmersiveStatusScene.getInstance().registerCallback("MapModel", this);
    }

    public void startListenMsg() {
        String uid = "";
        CommonManager commonManager = CommonManager.getInstance();
        commonManager.init();
        String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i(TAG, "getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            Logger.i(TAG, "Login = " + uid);
            msgPushPackage.startListen(uid);
        } else {
            Logger.i(TAG, "Logout");
        }
    }

    /***
     * 获取设置限行政策
     */
    public void getCurrentCityLimit() {
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

        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "get CurrentCity Limit " + currentCityCode);
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
     * @param frameLayoutWidth fragment的宽度
     */
    public void setMapCenterInScreen(int frameLayoutWidth) {
        Logger.i(TAG, "setMapCenterInScreen: " + frameLayoutWidth);
        mapPackage.changMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, MapVisibleAreaType.MAIN_AREA_NAVING);
    }

    /**
     * 恢复底图中心点在屏幕上的位置.
     */
    public void resetMapCenterInScreen() {
        Logger.i(TAG, "resetMapCenterInScreen");
        goToCarPosition();
        mapPackage.changMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, MapVisibleAreaType.MAIN_AREA_CAR);
        mViewModel.showOrHideSelfParkingView(false);
        stopCruise();
    }

    @Override
    public void onMapCenterChanged(MapType mapTypeId, double lon, double lat) {

    }

    @Override
    public void onMapLevelChanged(MapType mapTypeId, float mapLevel) {

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
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
    }

    @Override
    public void onMapScaleChanged(MapType mapTypeId, int currentScale) {
        mViewModel.updateOnMapScaleChanged(currentScale);
    }

    @Override
    public void onMapInitSuccess(MapType mapTypeId, boolean success) {
        //地图加载成功后发起关键字搜
        if (!ConvertUtils.isEmpty(mExtraKeyword)) {
            if (null != mViewModel) {
                String keyword = new String(mExtraKeyword);
                mViewModel.searchForExtraKeyword(keyword);
            }
            mExtraKeyword = null;
        }
    }

    @Override
    public void onMapLoadSuccess(MapType mapTypeId) {
        if (mapTypeId == MapType.MAIN_SCREEN_MAIN_MAP && mLoadMapSuccess) {
            mLoadMapSuccess = false;
            settingPackage.setSettingChangeCallback(mapTypeId.name(), this);
            layerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(positionPackage.getLastCarLocation().getLongitude(),
                    positionPackage.getLastCarLocation().getLatitude()));
            signalPackage.registerObserver(mapTypeId.name(), this);
            mapPackage.goToCarPosition(mapTypeId);
            mapPackage.changMapCenterInScreen(MapType.MAIN_SCREEN_MAIN_MAP, MapVisibleAreaType.MAIN_AREA_CAR);
            naviPackage.registerObserver(mViewModel.mScreenId, this);
            // 注册监听
            cruisePackage.registerObserver(mViewModel.mScreenId, this);
            // 恢复偏好设置
            mapModelHelp.restoreSetting();
            mViewModel.initTimer();
            addFavoriteToMap();
            speedMonitor.registerSpeedCallBack();
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
        if(NaviStatusPackage.getInstance().isGuidanceActive()){
            sendBuryPointForWakeup();
        }

        openCluterMap(mapTypeId, touchEvent);
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
        //选路页面不显示回自车位
        Logger.i("onMapClickPoi",NaviStatusPackage.getInstance().getCurrentNaviStatus());
        if(NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)){
            return;
        }

        ThreadManager.getInstance().postDelay(() -> {
            mViewModel.showOrHideSelfParkingView(true);
        },600);
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
        //是触控态的时候显示回车位   否则隐藏
//        if (Boolean.FALSE.equals(mViewModel.bottomNaviVisibility.get())) return;
        if(parkingViewExist()){
            if (currentImersiveStatus == ImersiveStatus.TOUCH) {
                mViewModel.showOrHideSelfParkingView(true);
            } else if (currentImersiveStatus == ImersiveStatus.IMERSIVE) {
                mViewModel.showOrHideSelfParkingView(false);
                goToCarPosition();
            }
        }

        if (getNaviStatus() == NaviStatus.NaviStatusType.NAVING && settingPackage.getAutoScale()) {
            Logger.i(TAG, "锁定比例尺，触摸态关闭动态比例尺，沉浸态开启比例尺！");
            boolean isOpen = !naviPackage.getFixedOverViewStatus() && currentImersiveStatus == ImersiveStatus.IMERSIVE && !naviPackage.getPreviewStatus();
            layerPackage.openDynamicLevel(mapTypeId, isOpen);
        }

        //导航中偶现 回车位出现问题
        if(TextUtils.equals(getNaviStatus(),NaviStatus.NaviStatusType.ROUTING) ||
                TextUtils.equals(getNaviStatus(),NaviStatus.NaviStatusType.NAVING) ||
                TextUtils.equals(getNaviStatus(),NaviStatus.NaviStatusType.CRUISE)){
            mViewModel.showOrHideSelfParkingView(false);
        }
    }

    private boolean parkingViewExist(){
        return getBottomNaviVisibility() || getTopFragment(PoiDetailsFragment.class) &&
                NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NO_STATUS)
                || getTopFragment(TrafficEventFragment.class);
    }

    private boolean getTopFragment(Class<? extends Fragment> targetClass){
      return mViewModel.getTopFragment(targetClass);
    }


    public boolean getBottomNaviVisibility(){
        return mViewModel.bottomNaviVisibility.get();
    }


    private void startSelfParkingTimer() {
        cancelSelfParkingTimer();
        if(parkingViewExist()){
            mSelfParkingTimer = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                cancelSelfParkingTimer();
                if(parkingViewExist()){
                    ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
                }
                Logger.d("onFinish-startSelfParkingTimer");
            },15,15);
        }
    }

    public void showParkingView(){
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        startSelfParkingTimer();
    }

    public void cancelSelfParkingTimer(){
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
        mSettingPackage.setConfigKeyDayNightMode(type);
    }

    public void saveLastLocationInfo() {
        mapPackage.saveLastLocationInfo();
    }

    @Override
    public void queryLimitResult(RouteRestrictionParam param) {
        Logger.d(TAG, "queryLimitResult success!", "isMainThread:" + (Looper.getMainLooper() == Looper.myLooper()));
        // 限行信息查询成功后更新UI
        if (limitQueryTaskId == param.getMRestrictedArea().getMRequestId()) {
            mViewModel.updateLimitInfo(param);
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
            },600);
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
                    mViewModel.toSearchResultFragment(keyword);
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
                        mViewModel.toHomeCompanyFragment(type, homeCompanyKeyword);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE:
                    //选择路线
                    if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)
                            || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
                        final int routeIndex = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, 0);
                        mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, routeIndex);
                    }
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
                default:
                    break;
            }
        }
    }

    public String getNaviStatus() {
        return mNaviStatusPackage.getCurrentNaviStatus();
    }

    /**
     * 外部应用打开地图传递的搜索关键字.
     *
     * @param keyword String.
     */
    public void setSearchKeyword(String keyword) {
        mExtraKeyword = keyword;
    }

    private ContinueNaviDialog mContinueNaviDialog;

    /***校验继续导航***/
    public void checkContinueNavi(Context context) {
        try {
            if (mHistoryManager != null) {
                History uncompletedNavi = mHistoryManager.getUncompletedNavi();
                Logger.i(TAG, "uncompletedNavi " + uncompletedNavi);
                if (uncompletedNavi == null) return;
                if (TextUtils.isEmpty(uncompletedNavi.getMStartPoint())) return;
                if (TextUtils.isEmpty(uncompletedNavi.getMEndPoint())) return;
                if (TextUtils.isEmpty(uncompletedNavi.getMEndPoiName())) return;

                mContinueNaviDialog = new ContinueNaviDialog.Build(context)
                        .setContent(uncompletedNavi.getMEndPoiName())
                        .setDialogObserver(new IBaseDialogClickListener() {
                            @Override
                            public void onCommitClick() {
                                dismissContinueNaviDialog();
                                uncompletedNavi.setMIsCompleted(true);
                                mHistoryManager.insertOrReplace(uncompletedNavi);
                                mViewModel.startRoute(getPoiInfoEntity(uncompletedNavi));
                            }

                            @Override
                            public void onCancelClick() {
                                uncompletedNavi.setMIsCompleted(true);
                                mHistoryManager.insertOrReplace(uncompletedNavi);
                                dismissContinueNaviDialog();
                            }
                        }).build();
                mContinueNaviDialog.show();
            }
        } catch (WindowManager.BadTokenException e) {
            Logger.e(TAG, "showNavTipDialog e-->" + e.getMessage());
            dismissContinueNaviDialog();
        } catch (Exception exception) {
            Logger.e(TAG, "showNavTipDialog exception-->" + exception.getMessage());
            dismissContinueNaviDialog();
        }
    }

    private PoiInfoEntity getPoiInfoEntity(History history) {
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setName(history.getMEndPoiName());
        poiInfoEntity.setAddress(history.getMEndPoiName());
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        poiInfoEntity.setPid(history.getMPoiId());
        GeoPoint historyPoint = mapModelHelp.parseGeoPoint(history.getMEndPoint());
        GeoPoint geoPoint = new GeoPoint();
        geoPoint.setLon(historyPoint.getLon());
        geoPoint.setLat(historyPoint.getLat());
        poiInfoEntity.setPoint(geoPoint);
        return poiInfoEntity;
    }

    private void dismissContinueNaviDialog() {
        if (mContinueNaviDialog != null) {
            mContinueNaviDialog.dismiss();
        }
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
        Logger.i("onCarClick",ThreadManager.getInstance().getCurrentThread().getName()+"");
    }

    @Override
    public void onFavoriteClick(MapType mapTypeId,PoiInfoEntity poiInfo) {
        Logger.i("onFavoriteClick",ThreadManager.getInstance().getCurrentThread().getName()+"");
        ThreadManager.getInstance().postUi(() -> {
            Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, poiInfo);
            bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, AutoMapConstant.PoiType.POI_MAP_CLICK);
            PoiDetailsFragment fragment = new PoiDetailsFragment();
            addPoiDetailsFragment(fragment, bundle);
        });
    }

    @Override
    public void onFlyLineMoveEnd(MapType mapTypeId,GeoPoint descPoint) {
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
            String isOpen = settingPackage.getValueFromDB(SettingController.KEY_SETTING_IS_AUTO_RECORD);
            if (isOpen != null && isOpen.equals("true")) {
                Logger.i(TAG, "开始打点");
                @SuppressLint("HardwareIds") final String androidId = Settings.Secure.getString(
                        AppContext.getInstance().getMContext().getContentResolver(),
                        Settings.Secure.ANDROID_ID
                );
                final long curTime = System.currentTimeMillis();
                mFilename = curTime + "_" + 0 + "_" + androidId;
                UserTrackPackage.getInstance().startGpsTrack(GBLCacheFilePath.SYNC_PATH + "/403", mFilename, 2000);
            }
            final SoundInfoEntity soundInfo = new SoundInfoEntity();
            soundInfo.setText(AppContext.getInstance().getMApplication().getString(R.string.step_into_cruise));
            naviPackage.onPlayTTS(soundInfo);
            mViewModel.showToast(R.string.step_into_cruise);
            mViewModel.setCruiseMuteOrUnMute(
                    Boolean.parseBoolean(settingPackage.getValueFromDB(SettingController.KEY_SETTING_CRUISE_BROADCAST))
            );
            mapModelHelp.setCruiseScale();
        }
        mViewModel.showOrHiddenCruise(isSuccess);
    }

    public void stopCruise() {
        // 判断是否已开启巡航，已开启再去关闭
        if (!TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
            Logger.i(TAG, "巡航未开启，无需关闭！");
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
            soundInfo.setText(AppContext.getInstance().getMApplication().getString(R.string.step_exit_cruise));
            naviPackage.onPlayTTS(soundInfo);
            mViewModel.showToast(R.string.step_exit_cruise);
            mViewModel.showOrHiddenCruise(false);
        }
    }

    @Override
    public void onGearChanged(int gear) {
        Logger.i(TAG, "onGearChanged:" + gear);
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
        settingPackage.setCruiseBroadcastOpen(isOpen);
    }

    @Override
    public void onSettingChanged(String key, String value) {
        switch (key) {
            case SettingController.KEY_SETTING_CRUISE_BROADCAST :
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
        Logger.i(TAG, "onNaviInfo:", "scale:" + scale, "size:" + size);
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
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            Logger.i(TAG, "notifyAimPoiPushMessage " + msg.getName());
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
            Logger.i(TAG, "notifyAimRoutePushMessage " + routeMsgPushInfo.getMName());
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
        if(AutoMapConstant.HomeCompanyType.HOME == mCompanyOrHomeType){
            OftenArrivedItemInfo mHomeInfo = data.getHome();
            if(!ConvertUtils.isEmpty(mHomeInfo) && !ConvertUtils.isEmpty(mHomeInfo.getWstrAddress())){
                mViewModel.showForecastDialog(mCompanyOrHomeType,mHomeInfo);
            }else {
                mViewModel.toHomeFragment();
            }
        }else {
            OftenArrivedItemInfo mCompanyInfo = data.getCompany();
            if(!ConvertUtils.isEmpty(mCompanyInfo) && !ConvertUtils.isEmpty(mCompanyInfo.getWstrAddress())){
                mViewModel.showForecastDialog(mCompanyOrHomeType,mCompanyInfo);
            }else {
                mViewModel.toCompanyFragment();
            }
        }
    }


    public void getOnlineForecastArrivedData(int type){
        mCompanyOrHomeType = type;
        //登陆了才去预测数据
        if(AccountPackage.getInstance().reallyLogin()){
            // 获取在线预测常去地点
            ForecastArrivedDataInfo param = new ForecastArrivedDataInfo();
            param.setLevel((int) mapPackage.getZoomLevel(MapType.MAIN_SCREEN_MAIN_MAP)); // 图面比例尺级别
            //先拿到经纬度
            LocInfoBean locInfoBean = positionPackage.getLastCarLocation();
            if(!ConvertUtils.isEmpty(locInfoBean)){
                param.setUserLoc(new GeoPoint(locInfoBean.getLongitude(),locInfoBean.getLatitude()));
                int adCode = mapDataPackage.getAdCodeByLonLat(locInfoBean.getLongitude(),locInfoBean.getLatitude());
                param.setAdCode(String.valueOf(adCode)); // 所在城市对应 adcode
            }
            AccountProfileInfo accountProfileInfo = AccountPackage.getInstance().getUserInfo();
            if(!ConvertUtils.isEmpty(accountProfileInfo)){
                param.setUserId(accountProfileInfo.getUid()); // 登录用户UID
            }
            mforCastPackage.getOnlineForecastArrivedData(param);
        }else {
            if(AutoMapConstant.HomeCompanyType.HOME == type){
                mViewModel.toHomeFragment();
            }else {
                mViewModel.toCompanyFragment();
            }
        }
    }

    public void addHomeOrCompanyInfoToSetting(int type,OftenArrivedItemInfo oftenArrivedItemInfo){
        int favoriteType = (type == AutoMapConstant.HomeCompanyType.HOME) ? 1 : 2;
        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
        poiInfoEntity.setPid(oftenArrivedItemInfo.getWstrPoiID());
        poiInfoEntity.setName(oftenArrivedItemInfo.getWstrPoiName());
        poiInfoEntity.setAddress(oftenArrivedItemInfo.getWstrAddress());
        poiInfoEntity.setPoint(oftenArrivedItemInfo.getStDisplayCoord());
        FavoriteInfo favoriteInfo = new FavoriteInfo();
        favoriteInfo.setMCommonName(favoriteType);
        poiInfoEntity.setFavoriteInfo(favoriteInfo);
        behaviorPackage.addFavorite(poiInfoEntity,favoriteType);
        sendBuryPointForAddFavorite(poiInfoEntity.getName(), favoriteType);

        if(favoriteType == 1){
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_home));
        }else {
            ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.forecast_set_company));
        }

    }

    @HookMethod
    private void sendBuryPointForAddFavorite(final String name, final int type) {
        String eventName = "";
        String key = BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION;
        switch (type){
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
        geoPoint.setLon(msg.getLon());
        geoPoint.setLat(msg.getLat());
        poiInfoEntity.setPoint(geoPoint);
        return poiInfoEntity;
    }

    public void switchMapMode() {
        mapPackage.switchMapMode(mapModelHelp.getMapTypeId());
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
        if(!isFavoritePoint){
            return;
        }
        showOrHideFavoriteToMap(String.valueOf(isFavoritePoint));
    }

    /**
     * 添加收藏
     */
    public void showOrHideFavoriteToMap(String value) {
        //如果设置不显示收藏点  需要隐藏
        if(Boolean.parseBoolean(value)){
            if(behaviorPackage.getFavoriteDataUpdatedStatus()){
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
                if(ConvertUtils.isEmpty(allPoiInfoEntities)){
                    return;
                }
                layerPackage.clearFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP);

                layerItemUserFavorite.setMSimpleFavoriteList(allPoiInfoEntities);
                layerPackage.addLayerItemOfFavorite(MapType.MAIN_SCREEN_MAIN_MAP, layerItemUserFavorite);
                layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP,true);
                behaviorPackage.setFavoriteDataUpdatedStatus(false);
                Logger.d("showOrHideFavoriteToMap","showall");
            }else {
                layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP,true);
                Logger.d("showOrHideFavoriteToMap","showvisible");
            }

        }else {
            layerPackage.setFavoriteVisible(MapType.MAIN_SCREEN_MAIN_MAP,false);
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
        mViewModel.setTMCView(param);
    }

    public boolean getHomeCompanyDisplay() {
        final String value = settingManager.getValueByKey(SettingController.KEY_SETTING_HOME_COMPANY_DISPLAYED);
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

    /**
     * 检查高德地图权限是否申请或过期
     */
    public void checkAuthorizationExpired() {
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
        final AuthorizationRequestDialog authorizationRequestDialog = new AuthorizationRequestDialog(
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
    private void sendBuryPointForWakeup(){
        //Empty body
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_POPUP)
    private void sendBuryPointForPopup(final String msg){
        BuryProperty property = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_HOME_PREDICTION, msg)
                .build();
        BuryPointController.getInstance().setBuryProps(property);
    }


    private float mStartX;
    private boolean isOpenClusterFlag = false;
    private static final int OPEN_TWO = 2;
    private static final int OPEN_THREE = 3;
    private void openCluterMap(MapType mapTypeId, MotionEvent touchEvent) {
        int pointerCount = touchEvent.getPointerCount();
        if (pointerCount == OPEN_TWO || pointerCount == OPEN_THREE){
            isOpenClusterFlag = true;
        }
        Logger.d(TAG, "onTouchEvent pointerCount = " + pointerCount);
        switch (touchEvent.getActionMasked()) {
            case MotionEvent.ACTION_DOWN:
                Logger.d(TAG, "onTouchEvent ACTION_DOWN");
                // 记录按下时的坐标
                mStartX = touchEvent.getX();
                break;
            case MotionEvent.ACTION_UP:
                Logger.d(TAG, "onTouchEvent ACTION_UP");
                if (isOpenClusterFlag) {
                    float endX = touchEvent.getX();
                    float deltaX = endX - mStartX;
                    final float MIN_DISTANCE = 50; // 最小滑动距离认定为有效滑动
                    if (Math.abs(deltaX) > MIN_DISTANCE) {
                        if (deltaX > 0) {
                            // 右滑
                            Logger.d(TAG, "Right swipe detected"+DeviceUtils.isCar(AppContext.getInstance().getMContext()));
                            if (!DeviceUtils.isCar(AppContext.getInstance().getMContext())){
                                ClusterActivityOffOnUtils.offOnClusterActivity(false);
                                MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD, "3");
                            }
                        } else {
                            // 左滑
                            Logger.d(TAG, "Left swipe detected"+DeviceUtils.isCar(AppContext.getInstance().getMContext()));
                            if (DeviceUtils.isCar(AppContext.getInstance().getMContext())){
                                ClusterActivityOffOnUtils.offOnClusterActivity(true);
                                MyFsaService.getInstance().sendEvent(FsaConstant.FsaFunction.ID_FINGER_FLYING_HUD, "2");
                            }
                        }
                    }
                    // 重置起始坐标
                    mStartX = 0;
                    isOpenClusterFlag = false;
                }
                break;
        }
    }
}