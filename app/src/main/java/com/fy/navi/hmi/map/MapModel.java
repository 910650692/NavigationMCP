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
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.navi.ContinueNaviDialog;
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
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.greendao.history.History;
import com.fy.navi.service.greendao.history.HistoryManager;
import com.fy.navi.service.greendao.setting.SettingManager;
import com.fy.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.fy.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.fy.navi.service.logicpaket.cruise.CruisePackage;
import com.fy.navi.service.logicpaket.cruise.ICruiseObserver;
import com.fy.navi.service.logicpaket.layer.LayerPackage;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.mapdata.MapDataPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
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
import com.fy.navi.ui.dialog.IBaseDialogClickListener;
import com.fy.navi.vrbridge.IVrBridgeConstant;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Objects;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */
public class MapModel extends BaseModel<MapViewModel> implements IMapPackageCallback,
        ImmersiveStatusScene.IImmersiveStatusCallBack, IAosRestrictedObserver, IPositionPackageCallback,
        SignalCallback, SpeedMonitor.CallBack, ICruiseObserver, SettingPackage.SettingChangeCallback,
        MsgPushCallBack, IGuidanceObserver {
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
        mapModelHelp = new MapModelHelp(MapTypeId.MAIN_SCREEN_MAIN_MAP);
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
        if (limitQueryTaskId == param.getRestrictedArea().getRequestId()) {
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
                    PoiInfoEntity poiInfo = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.POI_DETAIL_INFO);
                    if (null != poiInfo) {
                        mViewModel.toPoiDetailFragment(poiInfo);
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
                if (TextUtils.isEmpty(uncompletedNavi.startPoint)) return;
                if (TextUtils.isEmpty(uncompletedNavi.endPoint)) return;
                if (TextUtils.isEmpty(uncompletedNavi.endPoiName)) return;

                mContinueNaviDialog = new ContinueNaviDialog.Build(context)
                        .setContent(uncompletedNavi.endPoiName)
                        .setDialogObserver(new IBaseDialogClickListener() {
                            @Override
                            public void onCommitClick() {
                                dismissContinueNaviDialog();
                                uncompletedNavi.isCompleted = true;
                                mHistoryManager.insertOrReplace(uncompletedNavi);
                                mViewModel.startRoute(getPoiInfoEntity(uncompletedNavi));
                            }

                            @Override
                            public void onCancelClick() {
                                uncompletedNavi.isCompleted = true;
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
        poiInfoEntity.setName(history.endPoiName);
        poiInfoEntity.setAddress(history.endPoiName);
        poiInfoEntity.setPoiType(RoutePoiType.ROUTE_POI_TYPE_END);
        poiInfoEntity.setPid(history.poiId);
        GeoPoint historyPoint = mapModelHelp.parseGeoPoint(history.endPoint);
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
    public void notifyAimRoutePushMessage(RouteMsgPushInfo routeMsgPushInfo) {
        ThreadManager.getInstance().postUi(() -> {
            Logger.i(TAG, "notifyAimRoutePushMessage " + routeMsgPushInfo.getName());
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

    public void switchMapMode() {
        mapPackage.switchMapMode(mapModelHelp.getMapTypeId());
    }
}