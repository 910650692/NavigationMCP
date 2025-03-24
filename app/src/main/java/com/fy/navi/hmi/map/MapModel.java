package com.fy.navi.hmi.map;


import android.content.Context;
import android.content.res.Configuration;
import android.os.Bundle;
import android.os.Looper;
import android.os.Parcelable;
import android.text.TextUtils;
import android.view.MotionEvent;
import android.view.WindowManager;

import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.TimeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.message.MessageCenterHelper;
import com.fy.navi.hmi.navi.AuthorizationRequestDialog;
import com.fy.navi.hmi.navi.ContinueNaviDialog;
import com.fy.navi.hmi.navi.PhoneAddressDialog;
import com.fy.navi.scene.RoutePath;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.define.aos.RestrictedParam;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.bean.MapLabelItemBean;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.layer.GemBaseLayer;
import com.fy.navi.service.define.layer.GemLayerItem;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapMode;
import com.fy.navi.service.define.map.MapSurfaceViewSizeParams;
import com.fy.navi.service.define.map.MapTypeId;
import com.fy.navi.service.define.message.MessageCenterInfo;
import com.fy.navi.service.define.message.MessageCenterType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RouteMsgPushInfo;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.setting.SettingController;
import com.fy.navi.service.define.user.account.AccountProfileInfo;
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
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.message.MessageCenterCallBack;
import com.fy.navi.service.logicpaket.message.MessageCenterPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.position.IPositionPackageCallback;
import com.fy.navi.service.logicpaket.position.PositionPackage;
import com.fy.navi.service.logicpaket.route.RoutePackage;
import com.fy.navi.service.logicpaket.setting.SettingPackage;
import com.fy.navi.service.logicpaket.signal.SignalCallback;
import com.fy.navi.service.logicpaket.signal.SignalPackage;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushCallBack;
import com.fy.navi.service.logicpaket.user.msgpush.MsgPushPackage;
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

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapModel extends BaseModel<MapViewModel> implements IMapPackageCallback,
        ImmersiveStatusScene.IImmersiveStatusCallBack, IAosRestrictedObserver, IPositionPackageCallback,
        SignalCallback, SpeedMonitor.CallBack, ICruiseObserver, SettingPackage.SettingChangeCallback,
        MsgPushCallBack, IGuidanceObserver , MessageCenterCallBack {
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
    private String mLicense = "";
    private SettingPackage settingPackage;
    private SettingManager settingManager;
    private CruisePackage cruisePackage;
    private SignalPackage signalPackage;
    private MsgPushPackage msgPushPackage;
    private SpeedMonitor speedMonitor;
    private NaviPackage naviPackage;
    private MapModelHelp mapModelHelp;
    private MessageCenterPackage messageCenterPackage;
    private MessageCenterHelper messageCenterHelper;
    private final CalibrationPackage mCalibrationPackage;
    private final SettingPackage mSettingPackage;

    public MapModel() {
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
        cruisePackage = CruisePackage.getInstance();
        signalPackage = SignalPackage.getInstance();
        msgPushPackage = MsgPushPackage.getInstance();
        speedMonitor = new SpeedMonitor();
        speedMonitor.registerCallBack(this);
        msgPushPackage.registerCallBack(TAG, this);
        naviPackage = NaviPackage.getInstance();
        mCalibrationPackage = CalibrationPackage.getInstance();
        mapModelHelp = new MapModelHelp(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        messageCenterPackage = MessageCenterPackage.getInstance();
        messageCenterPackage.initMessageCenter();
        messageCenterPackage.registerCallBack(MessageCenterPackage.MESSAGECENTERKEY,this);
        messageCenterHelper = new MessageCenterHelper();
        mSettingPackage = SettingPackage.getInstance();
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        mapPackage.unInitMapView(mViewModel.getMapView());
        speedMonitor.unInit();
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "loadMapView");
        mapPackage.initMapView(mapSurfaceView);
        mapPackage.registerCallback(MapTypeId.MAIN_SCREEN_MAIN_MAP, this);
        ImmersiveStatusScene.getInstance().registerCallback("MapModel", this);
    }

    public void startListenMsg() {
        String uid = "";
        CommonManager commonManager = CommonManager.getInstance();
        commonManager.init();
        AccountProfileInfo info = new AccountProfileInfo();
        String valueJson = commonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i(TAG,"getUserInfo valueJson = " + valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            Logger.i(TAG,"Login = " + uid);
            msgPushPackage.startListen(uid);
        }else {
            Logger.i(TAG, "Logout");
        }
    }

    public void getCurrentCityLimit() {
        String license = settingManager.getValueByKey(SettingController.KEY_SETTING_GUIDE_VEHICLE_NUMBER);
        if (license == null || license.isEmpty()) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "No license plate set");
            mViewModel.setLimitDriverVisibility(false);
            return;
        }

        if (positionPackage.getLastCarLocation() == null) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "Limit get position failed");
            return;
        }
        int currentCityCode = mapDataPackage.getAdCodeByLonLat(positionPackage.getLastCarLocation().getLongitude(),
                positionPackage.getLastCarLocation().getLatitude());
        if (mCurrentCityCode == currentCityCode && mLicense.equals(license)) {
            Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "The information has not changed");
            return;
        }

        Logger.d(MapDefaultFinalTag.MAP_SERVICE_TAG, "get CurrentCity Limit " + currentCityCode);
        mCurrentCityCode = currentCityCode;
        mLicense = license;
        RestrictedParam restrictedParam = new RestrictedParam();
        // 请求根据车型等信息获取的规则 type = 7 请求城市全部规则 type = 8 请求城市列表 type = 9 根据规则请求数据
        restrictedParam.setRestrict_type(7);
        restrictedParam.setPlate(mLicense);
        restrictedParam.setAdcodes(String.valueOf(mCurrentCityCode));
        limitQueryTaskId = restrictedPackage.queryRestrictedInfo(restrictedParam);
    }

    /**
     * 设置底图中心点相对于屏幕偏移量.
     *
     * @param frameLayoutWidth fragment的宽度
     */
    public void setMapCenterInScreen(int frameLayoutWidth) {
        Logger.i(TAG, "setMapCenterInScreen: " + frameLayoutWidth);
        MapSurfaceViewSizeParams mapSurfaceViewSizeParams = mapPackage.getMapSurfaceParam(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mapSurfaceViewSizeParams.screenLeftOffset = frameLayoutWidth;
        int surfaceWidth = ConvertUtils.ln2int(mapSurfaceViewSizeParams.width);
        int surfaceHeight = ConvertUtils.ln2int(mapSurfaceViewSizeParams.height);
        int left = (surfaceWidth - frameLayoutWidth) / 2 + frameLayoutWidth;
        int top = surfaceHeight * 3 / 4;
        mapPackage.setMapCenterInScreen(MapTypeId.MAIN_SCREEN_MAIN_MAP, left, top);
    }

    /**
     * 恢复底图中心点在屏幕上的位置.
     */
    public void resetMapCenterInScreen() {
        Logger.i(TAG, "resetMapCenterInScreen");
        MapSurfaceViewSizeParams mapSurfaceViewSizeParams = mapPackage.getMapSurfaceParam(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        mapSurfaceViewSizeParams.screenLeftOffset = 100;
        int surfaceWidth = ConvertUtils.ln2int(mapSurfaceViewSizeParams.width);
        int surfaceHeight = ConvertUtils.ln2int(mapSurfaceViewSizeParams.height);
        mapPackage.setMapCenterInScreen(MapTypeId.MAIN_SCREEN_MAIN_MAP, surfaceWidth / 2, surfaceHeight / 2);
    }

    @Override
    public void onMapCenterChanged(MapTypeId mapTypeId, double lon, double lat) {

    }

    @Override
    public void onMapLevelChanged(MapTypeId mapTypeId, float mapLevel) {

    }

    @Override
    public void onMapClickBlank(MapTypeId mapTypeId, float px, float py) {
        stopCruise();
    }

    @Override
    public void onMapClickLabel(MapTypeId mapTypeId, ArrayList<MapLabelItemBean> pLabels) {
        stopCruise();
    }

    @Override
    public void onMapMove(MapTypeId mapTypeId, long px, long py, boolean moveEnd) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapTypeId.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
    }

    @Override
    public void onMapScaleChanged(MapTypeId mapTypeId, int currentScale) {
        mViewModel.updateOnMapScaleChanged(currentScale);
    }

    @Override
    public void onMapInitSuccess(MapTypeId mapTypeId, boolean success) {
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
    public void onMapLoadSuccess(MapTypeId mapTypeId) {
        if (mapTypeId == MapTypeId.MAIN_SCREEN_MAIN_MAP) {
            settingPackage.setSettingChangeCallback(mapTypeId.name(), this);
            layerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(positionPackage.getLastCarLocation().getLongitude(),
                    positionPackage.getLastCarLocation().getLatitude()));
            signalPackage.registerObserver(mapTypeId.name(), this);
            mapPackage.goToCarPosition(mapTypeId);
            naviPackage.registerObserver(mViewModel.mScreenId, this);
            // 恢复偏好设置
            mapModelHelp.restoreSetting();
        }
    }

    @Override
    public void onMapTouchEvent(MapTypeId mapTypeId, MotionEvent touchEvent) {

    }

    @Override
    public void onMapClickPoi(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
    }

    @Override
    public void onReversePoiClick(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
    }

    @Override
    public void onMapModeChange(MapTypeId mapTypeId, MapMode mapMode) {
        switch (mapMode) {
            case NORTH_2D -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_2d_north);
            case UP_3D -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_3d_north);
            default -> mViewModel.carModeImgId.set(R.drawable.img_car_mode_2d_head);
        }
    }

    @Override
    public void onImmersiveStatusChange(MapTypeId mapTypeId, ImersiveStatus currentImersiveStatus) {
        //是触控态的时候显示回车位   否则隐藏
        if (Boolean.FALSE.equals(mViewModel.bottomNaviVisibility.get())) return;
        if(currentImersiveStatus == ImersiveStatus.TOUCH){
            mViewModel.showOrHideSelfParkingView(true);
        }else if(currentImersiveStatus == ImersiveStatus.IMERSIVE){
            mViewModel.showOrHideSelfParkingView(false);
            goToCarPosition();
        }
    }

    public void goToCarPosition() {
        mapPackage.goToCarPosition(MapTypeId.MAIN_SCREEN_MAIN_MAP);
    }

    public void updateUiStyle(MapTypeId mapTypeId, int uiMode) {
        final boolean isNightMode = (uiMode & Configuration.UI_MODE_NIGHT_MASK) == Configuration.UI_MODE_NIGHT_YES;
        final String value = isNightMode ? SettingController.VALUE_DISPLAY_MODE_NIGHT : SettingController.VALUE_DISPLAY_MODE_DAYTIME;
        mapPackage.updateUiStyle(mapTypeId, uiMode);
        commonManager.insertOrReplace(SettingController.KEY_SETTING_DISPLAY_MODE, value);
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
    }

    @Override
    public void onQueryTrafficEvent(MapTypeId mapTypeId, PoiInfoEntity poiInfo) {
        if (TextUtils.equals(mapTypeId.name(), mViewModel.mScreenId)) {
            IMapPackageCallback.super.onQueryTrafficEvent(mapTypeId, poiInfo);
            mViewModel.openTrafficDetailFragment(poiInfo);
        }
    }

    @Override
    public void onVoiceOpenPage(MapTypeId mapTypeId, Bundle bundle) {
        if (MapTypeId.MAIN_SCREEN_MAIN_MAP == mapTypeId && null != bundle && null != mViewModel) {
            int voicePage = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE);
            switch (voicePage) {
                case IVrBridgeConstant.VoiceIntentPage.KEYWORD_SEARCH:
                    //关键字搜索
                    String keyword = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, "");
                    if (null == keyword || keyword.isEmpty()) {
                        return;
                    }
                    mViewModel.toSearchResultFragment(keyword);
                    break;
                case IVrBridgeConstant.VoiceIntentPage.AROUND_SEARCH:
                    //周边搜
                    String aroundKey = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD, "");
                    if (TextUtils.isEmpty(aroundKey)) {
                        return;
                    }
                    Parcelable parcelable = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.AROUND_POINT);
                    if (parcelable instanceof GeoPoint centerPoint) {
                        PoiInfoEntity poiInfoEntity = new PoiInfoEntity();
                        poiInfoEntity.setPoint(centerPoint);
                        mViewModel.toSearchAroundFragment(aroundKey, poiInfoEntity);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.ROUTING:
                    //路线规划
                    Serializable serializable = bundle.getSerializable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST);
                    if (serializable instanceof RouteSpeechRequestParam) {
                        RouteSpeechRequestParam param = (RouteSpeechRequestParam) serializable;
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
                    PoiInfoEntity poiInfo = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.POI_DETAIL_INFO);
                    if (null != poiInfo) {
                        mViewModel.toPoiDetailFragment(poiInfo);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.HOME_COMPANY_SET:
                    //设置家和公司，传入语音搜索关键字，直接发起搜索
                    int type = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.HOME_COMPANY_TYPE);
                    String homeCompanyKeyword = bundle.getString(IVrBridgeConstant.VoiceIntentParams.KEYWORD);
                    if (!TextUtils.isEmpty(homeCompanyKeyword)) {
                        mViewModel.toHomeCompanyFragment(type, homeCompanyKeyword);
                    }
                    break;
                case IVrBridgeConstant.VoiceIntentPage.SELECT_ROUTE:
                    if (NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.SELECT_ROUTE)
                    || NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NAVING)) {
                        final int routeIndex = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, 0);
                        RoutePackage.getInstance().selectRoute(MapTypeId.MAIN_SCREEN_MAIN_MAP, routeIndex);
                    }
                    break;
                default:
                    break;
            }
        }
    }

    public String getNaviStatus() {
        return mapPackage.getNaviStatus();
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
    public void onNotifyClick(MapTypeId mapTypeId, GemBaseLayer layer, GemLayerItem pItem) {
        IMapPackageCallback.super.onNotifyClick(mapTypeId, layer, pItem);
        // TODO 图层点击回调
    }

    @Override
    public void startCruise() {
        boolean isNoStatus = Objects.equals(getNaviStatus(), NaviStatus.NaviStatusType.NO_STATUS);
        Logger.i(TAG, "startCruise", "isNoStatus:" + isNoStatus);
        if (isNoStatus) {
            boolean isSuccess = cruisePackage.startCruise();
            if (isSuccess) {
                mViewModel.showToast(R.string.step_into_cruise);
                mViewModel.showOrHiddenCruise(true);
                mViewModel.setCruiseMuteOrUnMute(
                        Boolean.parseBoolean(settingPackage.getValueFromDB(SettingController.KEY_SETTING_CRUISE_BROADCAST))
                );
                mapModelHelp.setCruiseScale();
            }
        }
    }

    public void stopCruise() {
        Logger.i(TAG, "stopCruise:" + getNaviStatus());
        if (getNaviStatus() == NaviStatus.NaviStatusType.CRUISE) {
            Logger.i(TAG, "stopCruise");
            boolean isSuccess = cruisePackage.stopCruise();
            if (isSuccess) {
                mViewModel.showToast(R.string.step_exit_cruise);
                mViewModel.showOrHiddenCruise(false);
            }
        }
    }

    @Override
    public void onSpeedChanged(float speed) {
        // speed 单位：m/s 转换成km/h
        speedMonitor.updateSpeed(speed * 3.6f);
    }

    @Override
    public void onGearChanged(int gear) {

    }

    @Override
    public void onUpdateCruiseInfo(CruiseInfoEntity cruiseInfoEntity) {
        ICruiseObserver.super.onUpdateCruiseInfo(cruiseInfoEntity);
        // 巡航-电子眼信息
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
            case SettingController.KEY_SETTING_CRUISE_BROADCAST -> {
                mViewModel.setCruiseMuteOrUnMute(Boolean.parseBoolean(value));
            }
        }
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviETAInfo) {
        int scale = mapPackage.getCurrentZoomScale(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        float size = mapPackage.getZoomLevel(MapTypeId.MAIN_SCREEN_MAIN_MAP);
        Logger.i(TAG, "onNaviInfo:", "scale:" + scale, "size:" + size);
    }

    @Override
    public void onNaviArrive(long traceId, int naviType) {
        Logger.i(TAG, "onNaviArrive");
    }

    @Override
    public void onNaviStop() {
        Logger.i(TAG, "onNaviStop:");
    }

    @Override
    public void notifyAimPoiPushMessage(final MsgPushInfo msg) {
        if (msg == null){
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            Logger.i(TAG, "notifyAimPoiPushMessage " + msg.getName());
            final PhoneAddressDialog phoneAddressDialog = new PhoneAddressDialog(
                StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
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
                    RoutePackage.getInstance().requestRouteRestoration(routeMsgPushInfo, MapTypeId.MAIN_SCREEN_MAIN_MAP);
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
     * 手机推送地址转化成PoiInfo
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
    public boolean offlineMap15Day(){
        final String time = commonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_MAP_CHECK);
        if(!TextUtils.isEmpty(time)){
            //超过15天
            final boolean after15Day = messageCenterHelper.isNotConnectedFor15Days(Long.parseLong(time));
            if(after15Day) {
                commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK,String.valueOf(System.currentTimeMillis()));
                Logger.i("offlineMap15Day","+++");
                return true;
            }
        }else {
            commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK,String.valueOf(System.currentTimeMillis()));
            Logger.i("offlineMap15Day","----");
        }
        return false;
    }

    /**
     * 离线地图是否45天未更新
     */
    public boolean offlineMap45Day(){
        if(Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork())){
            messageCenterHelper.saveLast45ConnectedTime();
            return false;
        }else {
            //45天未联网
            return messageCenterHelper.isNotConnectedFor45Days();
        }
    }

    /**
     * 是否显示限行
     */
    public boolean showSameDayLimit(){
      final String limitTime = commonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_LIMIT_TIME);
      if(!TextUtils.isEmpty(limitTime)){
            //在判断是否是同一天  同一天的也不显示
            final String currentDay = TimeUtils.convertYMD();
            if(!currentDay.equals(limitTime)){
                commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, currentDay);
                return true;
            }
      }else {
            commonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, TimeUtils.convertYMD());
            return true;
      }
      return false;
    }


    /**
     * 推送消息
     * @param messageCenterType 类型
     * @param title 显示的title
     */
    public void managerMessage(final MessageCenterType messageCenterType,final String title){
        final MessageCenterInfo messageCenterInfo = messageCenterHelper.manageMessage(messageCenterType,title);
        if(messageCenterInfo!=null){
            ThreadManager.getInstance().postUi(() -> {
                messageCenterPackage.pushMessage(messageCenterInfo);
            });
        }
    }

    @Override
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo) {
        mViewModel.onMessageInfoNotifyCallback(messageCenterInfo);
    }

    @Override
    public void onMessageInfoRemoveCallback() {
        mViewModel.onMessageInfoRemoveCallback();
    }

    /**
     * 动力类型标定
     * -1 无效值
     * 0 汽油车
     * 1 纯电动车
     * 2 插电式混动汽车
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
            StackManager.getInstance().getCurrentActivity(MapTypeId.MAIN_SCREEN_MAIN_MAP.name()));
        authorizationRequestDialog.setEndDate(endDate);
        authorizationRequestDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                final LocalDate currentDate = LocalDate.now();
                final LocalDate oneYearLater = currentDate.plusYears(1);
                final DateTimeFormatter formatter = DateTimeFormatter.ofPattern("yyyy年M月d日");
                mSettingPackage.setEndDate(oneYearLater.format(formatter));
            }
        });
        authorizationRequestDialog.show();
    }

}