package com.fy.navi.hmi.map;

import static com.fy.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.annotation.SuppressLint;
import android.app.Application;
import android.os.Bundle;
import android.os.Looper;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.StringRes;
import androidx.databinding.ObservableBoolean;
import androidx.databinding.ObservableField;
import androidx.databinding.ObservableInt;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Lifecycle;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.bean.BuryProperty;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.burypoint.controller.BuryPointController;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.favorite.FavoriteHelper;
import com.fy.navi.hmi.favorite.HomeCompanyFragment;
import com.fy.navi.hmi.favorite.MapPointSearchFragment;
import com.fy.navi.hmi.limit.LimitCitySelectionFragment;
import com.fy.navi.hmi.limit.LimitDriveFragment;
import com.fy.navi.hmi.mapdata.MapDataFragment;
import com.fy.navi.hmi.navi.AuthorizationRequestDialog;
import com.fy.navi.hmi.navi.ForecastAddressDialog;
import com.fy.navi.hmi.navi.NaviGuidanceFragment;
import com.fy.navi.hmi.poi.PoiDetailsFragment;
import com.fy.navi.hmi.route.RouteFragment;
import com.fy.navi.hmi.search.mainsearch.MainSearchFragment;
import com.fy.navi.hmi.search.searchresult.SearchResultFragment;
import com.fy.navi.hmi.setting.SettingFragment;
import com.fy.navi.hmi.traffic.TrafficEventFragment;
import com.fy.navi.scene.impl.imersive.ImersiveStatus;
import com.fy.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.AutoMapConstant.PoiType;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.aos.RestrictedArea;
import com.fy.navi.service.define.aos.RestrictedAreaDetail;
import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.cruise.CruiseInfoEntity;
import com.fy.navi.service.define.map.IBaseScreenMapView;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.map.ThemeType;
import com.fy.navi.service.define.message.MessageCenterInfo;
import com.fy.navi.service.define.message.MessageCenterType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.route.RoutePoiType;
import com.fy.navi.service.define.route.RouteRestrictionParam;
import com.fy.navi.service.define.route.RouteSpeechRequestParam;
import com.fy.navi.service.define.route.RouteTMCParam;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.fy.navi.service.define.utils.NumberUtils;
import com.fy.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.fy.navi.service.logicpaket.search.SearchPackage;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.service.logicpaket.user.forecast.IForecastAddressCallBack;
import com.fy.navi.ui.action.Action;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.vrbridge.IVrBridgeConstant;

import java.util.Date;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseMapViewModel extends BaseViewModel<MapActivity, MapModel> {
    private static final String TAG = "BaseMapViewModel";
    public ObservableBoolean backToCcPVisibility;
    public ObservableBoolean mainBTNVisibility;
    public ObservableBoolean mScaleViewVisibility;
    public ObservableBoolean bottomNaviVisibility;
    public ObservableBoolean backToParkingVisibility;
    public ObservableBoolean messageCenterVisible;
    public ObservableField<MessageCenterInfo> messageCenterEntity;
    public ObservableField<String> messageCenterOperate;
    public ObservableField<String> messageCenterTitle;
    public ObservableField<String> homeTime;
    public ObservableField<String> companyTime;
    public ObservableField<String> messageCenterContent;
    public ObservableBoolean messageCenterContentVisibility;
    public ObservableBoolean messageCenterImgVisibility;
    public ObservableBoolean messageLineVisibility;
    public ObservableBoolean messageLineHeightVisibility;
    public ObservableBoolean messageCloseVisibility;
    public ObservableField<Boolean> naviHomeVisibility;
    public ObservableInt carModeImgId;
    @Nullable
    private RestrictedArea restrictedArea;
    @Nullable
    private RouteRestrictionParam routeRestrictionParam;
    public ObservableField<Boolean> limitDriverVisibility;
    public ObservableField<Boolean> tmcModeVisibility;
    public ObservableField<String> limitDriverTitle;
    public ObservableField<Boolean> cruiseVisibility;
    public ObservableField<Boolean> cruiseLanesVisibility;
    public ObservableField<Boolean> muteVisibility;
    public ObservableField<Boolean> mPopGuideLoginShow;
    public ObservableField<Boolean> mGoHomeVisible;

    private ScheduledFuture mScheduledFuture;
    private ScheduledFuture goHomeTimer;
    private final int mTimer = 300;


    public BaseMapViewModel(@NonNull Application application) {
        super(application);
        backToCcPVisibility = new ObservableBoolean(false);
        mainBTNVisibility = new ObservableBoolean(true);
        mScaleViewVisibility = new ObservableBoolean(true);
        naviHomeVisibility = new ObservableField<>(false);
        limitDriverVisibility = new ObservableField<>(false);
        tmcModeVisibility = new ObservableField<>(false);
        limitDriverTitle = new ObservableField<>("");
        carModeImgId = new ObservableInt(R.drawable.img_car_mode_2d_north);
        cruiseVisibility = new ObservableField<>(false);
        muteVisibility = new ObservableField<>(true);
        bottomNaviVisibility = new ObservableBoolean(true);
        backToParkingVisibility = new ObservableBoolean(false);
        messageCenterVisible = new ObservableBoolean(false);
        messageCenterEntity = new ObservableField<>();
        messageCenterOperate = new ObservableField<>("");
        messageCenterTitle = new ObservableField<>("");
        homeTime = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
        companyTime = new ObservableField<>(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
        messageCenterContent = new ObservableField<>("");
        messageCenterContentVisibility = new ObservableBoolean(false);
        messageCenterImgVisibility = new ObservableBoolean(false);
        messageLineVisibility = new ObservableBoolean(false);
        messageLineHeightVisibility = new ObservableBoolean(false);
        messageCloseVisibility = new ObservableBoolean(false);
        mPopGuideLoginShow = new ObservableField<>(false);
        cruiseLanesVisibility = new ObservableField<>(false);
        mGoHomeVisible = new ObservableField<>(false);
    }

    @Override
    protected MapModel initModel() {
        return new MapModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        mModel.checkContinueNavi(mView);
    }

    @Override
    public void onResume() {
        super.onResume();
        mModel.checkAuthorizationExpired();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    public Action intercept = () -> {
        // 拦截控件点击事件
        mModel.stopCruise();
    };

    //回自车位逻辑
    public Action backToSelfParking = () -> {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        //取消倒计时
        mModel.cancelSelfParkingTimer();
    };

    // 播报和静音切换
    public Action muteOrUnMute = () -> {
        mModel.setCruiseVoice(!muteVisibility.get());
    };

    public Action openSearchFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_SEARCH)
        public void call() {
            addFragment(new MainSearchFragment(), null);
            mModel.stopCruise();
        }
    };

    public Action openHomeFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_HOME)
        public void call() {
            try {
                PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
                if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                    //直接进入导航到家
                    SearchPackage.getInstance().clearLabelMark();
                    startRoute(poiInfoEntity);
                    return;
                }

                mModel.getOnlineForecastArrivedData(AutoMapConstant.HomeCompanyType.HOME);

            } catch (Exception e) {
                Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
            }
        }
    };

    public Action openCompanyFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_WORK)
        public void call() {
            try {
                PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
                if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                    //直接进入导航到公司
                    SearchPackage.getInstance().clearLabelMark();
                    startRoute(poiInfoEntity);
                    return;
                }

                //判断是否存在预测数据
                mModel.getOnlineForecastArrivedData(AutoMapConstant.HomeCompanyType.COMPANY);

            } catch (Exception e) {
                Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
            }
        }
    };

    public Action searchForChargeStation = new Action() {
        @Override
        @HookMethod
        public void call() {
            try {
                final Bundle bundle = new Bundle();
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
                final int powerType = powerType();
                // 油车
                if (powerType == 0) {
                    bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "加油站");
                } else {
                    bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "充电站");
                }
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
                addFragment(new SearchResultFragment(), bundle);

                //For bury point
                BuryPointController.getInstance().setEventName(powerType == 0 ? BuryConstant.EventName.AMAP_MAP_GASSTATION : BuryConstant.EventName.AMAP_MAP_CHARGINGSTATION);

            } catch (Exception e) {
                Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
            }
        }
    };

    public void searchForExtraKeyword(String keyword) {
        try {
            Logger.d(SEARCH_HMI_TAG, "searchExtraKeyword: " + keyword);
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
            bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
            bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
            addFragment(new SearchResultFragment(), bundle);
        } catch (Exception e) {
            Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
        }
    }

    public Action openSettingFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_MAP_SETTING)
        public void call() {
            messageCenterVisible.set(false);
            addFragment(new SettingFragment(), null);
        }
    };

    public Action carHeader = () -> {
        showOrHideSelfParkingView(false);
        boolean result = mModel.switchMapMode();
        String modeText = mModel.getCurrentMapModelText();
        if(!result){
            ToastUtils.Companion.getInstance().showCustomToastView(String.
                    format(ResourceUtils.Companion.getInstance().getString(com.fy.navi.scene.R.string.navi_map_mode_switch_fail), modeText));
            return;
        }
        ToastUtils.Companion.getInstance().showCustomToastView(String.
                format(ResourceUtils.Companion.getInstance().getString(
                        com.fy.navi.scene.R.string.switch_car_angle), modeText));
    };

    public Action messageCenterGone = () -> {
        messageCenterVisible.set(false);
        mModel.deleteMessage();
        MessageCenterInfo messageCenterInfo = messageCenterEntity.get();
        if (messageCenterInfo != null) {
            if (messageCenterInfo.getMsgType() == MessageCenterType.CONTINUE_NAVI) {
                mModel.onCancelContinueNaviClick();
            }
        }
    };

    public Action openLimitDetailFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_TRAFFICRESTRICT_CURRENT)
        public void call() {
            if (needInterceptor()) return;
            Bundle bundle = new Bundle();
            bundle.putSerializable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_DRIVER, routeRestrictionParam);
            addPoiDetailsFragment(new LimitDriveFragment(), bundle);
            closeAllFragmentsUntilTargetFragment(LimitCitySelectionFragment.class.getName());
        }
    };

    public Action messageCenterOperateClick = () -> {
        messageCenterGone.call();
        final MessageCenterInfo messageCenterInfo = messageCenterEntity.get();
        if (messageCenterInfo != null) {
            if (messageCenterInfo.getMsgType() == MessageCenterType.ROAD_LIMIT) {
                openLimitDetailFragment.call();
            } else if (messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_15 ||
                    messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_45) {
                final Bundle bundle = new Bundle();
                bundle.putBoolean("isCheck", true);
                addFragment(new MapDataFragment(), bundle);
            } else if (messageCenterInfo.getMsgType() == MessageCenterType.CONTINUE_NAVI) {
                mModel.onContinueNaviClick();
            }
        }
    };

    public boolean getTopFragment(Class<? extends Fragment> targetClass) {
        Fragment fragment = mView.getSupportFragmentManager().findFragmentById(R.id.layout_fragment);
        if (fragment != null) {
            return targetClass.isInstance(fragment);
        }
        return false;
    }

    public void toHomeFragment() {
        //判断是否存在预测数据
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
        addFragment(new HomeCompanyFragment(), bundle);
        mModel.stopCruise();
    }

    public void toCompanyFragment() {
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
        addFragment(new HomeCompanyFragment(), bundle);
        mModel.stopCruise();

    }

    public void showForecastDialog(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        ThreadManager.getInstance().postUi(() -> {
            new ForecastAddressDialog(mView, type, oftenArrivedItemInfo, new IForecastAddressCallBack() {
                @Override
                public void AddForecastInfo(OftenArrivedItemInfo oftenArrivedItemInfo) {
                    mModel.addHomeOrCompanyInfoToSetting(type, oftenArrivedItemInfo);
                    initTimer();
                }

                @Override
                public void addressClick() {
                    if (AutoMapConstant.HomeCompanyType.HOME == type) {
                        toHomeFragment();
                    } else {
                        toCompanyFragment();
                    }
                }
            }).show();
        });
    }


    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        mModel.loadMapView(mapSurfaceView);
    }

    public void startListenMsg() {
        mModel.startListenMsg();
    }

    public void getCurrentCityLimit() {
        mModel.getCurrentCityLimit();
    }

    public void setMapCenterInScreen(int frameLayoutWidth) {
        Logger.i(TAG, "setMapCenterInScreen");
        mModel.setMapCenterInScreen(frameLayoutWidth);
        final String state = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        // 如果是导航页面的话比例尺继续正常显示，算路界面正常显示比例尺
        mScaleViewVisibility.set(NaviStatus.NaviStatusType.SELECT_ROUTE.equals(state)
                || NaviStatus.NaviStatusType.ROUTING.equals(state) ||
                NaviStatus.NaviStatusType.NAVING.equals(state));
        mainBTNVisibility.set(false);
        bottomNaviVisibility.set(false);
        backToParkingVisibility.set(false);
        mPopGuideLoginShow.set(false);
        mGoHomeVisible.set(false);
    }

    public void setMapCenterInScreen(final int frameLayoutWidth, final Bundle bundle) {
        int type = -1;
        if (bundle != null) {
            type = bundle.getInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, -1);
        }
        Logger.i(TAG, "setMapCenterInScreen type:" + type);
        mModel.setMapCenterInScreen(frameLayoutWidth);
        final String state = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        // 如果是导航页面的话比例尺继续正常显示，算路界面正常显示比例尺
        mScaleViewVisibility.set(type != -1 || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(state)
                || NaviStatus.NaviStatusType.ROUTING.equals(state) ||
                NaviStatus.NaviStatusType.NAVING.equals(state));
        mainBTNVisibility.set(false);
        bottomNaviVisibility.set(false);
        backToParkingVisibility.set(false);
        mPopGuideLoginShow.set(false);
        mGoHomeVisible.set(false);
        cancelTimer();
    }

    public void resetMapCenterInScreen() {
        mView.setMapFocusable(true);
        mModel.resetMapCenterInScreen();
        mScaleViewVisibility.set(true);
        mainBTNVisibility.set(true);
        bottomNaviVisibility.set(true);
        if (mModel.checkPopGuideLogin()) {
            mPopGuideLoginShow.set(true);
        }
        initTimer();
    }

    public void showParkingView() {
        mModel.showParkingView();
    }

    private void checkHomeOfficeShow() {
        tmcModeVisibility.set(mModel.getHomeCompanyDisplay());
        if (!mModel.getHomeCompanyDisplay()) {
            cancelTimer();
            return;
        }

        //Nd车型需要先判断是否是节假日
        if(mModel.showNdGoHomeView()){
            tmcModeVisibility.set(false);
            mModel.sendReqHolidayList();
        }else {
            PoiInfoEntity homePoi = getFavoritePoiInfo(PoiType.POI_HOME);
            PoiInfoEntity companyPoi = getFavoritePoiInfo(PoiType.POI_COMPANY);
            if (ConvertUtils.isEmpty(homePoi) && ConvertUtils.isEmpty(companyPoi)) {
                homeTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
                companyTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
                mView.setTMCView(0, null);
                mView.setTMCView(1, null);
            } else if (!ConvertUtils.isEmpty(homePoi) && !ConvertUtils.isEmpty(companyPoi)) {
                mModel.refreshHomeOfficeTMC(true);
            } else if (!ConvertUtils.isEmpty(homePoi) && ConvertUtils.isEmpty(companyPoi)) {
                mModel.refreshHomeOfficeTMC(true);
                companyTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
                mView.setTMCView(1, null);
            } else {
                mModel.refreshHomeOfficeTMC(false);
                homeTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_go_setting));
                mView.setTMCView(0, null);
            }
        }
    }

    public GeoPoint nearByHome(boolean home){
        PoiInfoEntity poiInfoEntity ;
        if (home) {
            poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
        }else {
            poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
        }
        if(!ConvertUtils.isEmpty(poiInfoEntity)){
           return poiInfoEntity.getMPoint();
        }
        return null;
    }

    public void loadNdOfficeTmc(boolean home){
        mModel.refreshHomeOfficeTMC(home);
    }

    /***
     * 页面倒计时
     */
    public void initTimer() {
        cancelTimer();
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(this::checkHomeOfficeShow, NumberUtils.NUM_0, mTimer);
    }

    /***
     * 取消页面倒计时
     */
    public void cancelTimer() {
        if (!ConvertUtils.isEmpty(mScheduledFuture)) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mScheduledFuture = null;
        }
    }

    private void startGoHomeTimer(){
        cancelTimerGoHomeTimer();
        goHomeTimer = ThreadManager.getInstance().asyncAtFixDelay(new Runnable() {
            @Override
            public void run() {
                cancelTimerGoHomeTimer();
                mGoHomeVisible.set(false);
            }
        }, NumberUtils.NUM_30, NumberUtils.NUM_30);

    }

    private void  cancelTimerGoHomeTimer(){
        if (!ConvertUtils.isEmpty(goHomeTimer)) {
            ThreadManager.getInstance().cancelDelayRun(goHomeTimer);
            goHomeTimer = null;
        }
    }

    public void setBacktoCCP(boolean isTouchStatus) {
        backToCcPVisibility.set(isTouchStatus);
    }

    public void showOrHideSelfParkingView(boolean visible) {
        backToParkingVisibility.set(visible);
    }

    public void addSceneGoHomeCallBackVieModel(int type){
        if (type == AutoMapConstant.HomeCompanyType.HOME) {
            PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
            if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                //直接进入导航到家
                SearchPackage.getInstance().clearLabelMark();
                startRoute(poiInfoEntity);
            }
        }else if(type == AutoMapConstant.HomeCompanyType.COMPANY){
            PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
            if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                //直接进入导航到公司
                SearchPackage.getInstance().clearLabelMark();
                startRoute(poiInfoEntity);
            }
        }
    }

    /**
     * UE单位是公里
     *
     * @param scale 单位是米
     */
    public void updateOnMapScaleChanged(int scale) {
        try {
            String format;
            int kilometer;
            if (scale / 1000 > 0) {
                format = "%d公里";
                kilometer = scale / 1000;
            } else {
                format = "%d米";
                kilometer = scale;
            }
            final String result = String.format(format, kilometer);
            mView.updateOnMapScaleChanged(result);
        } catch (Exception exception) {
            Logger.e(TAG, "updateOnMapScaleChanged, format error:" + exception.getMessage());
        }
    }

    public void toPoiDetailFragment(PoiInfoEntity entity) {
        if (needInterceptor() && FavoriteHelper.getInstance().getHomeCompanyType() == -1) {
            return;
        }
        if (FavoriteHelper.getInstance().getHomeCompanyType() != -1) {
            //如果正在执行地图选点流程，点击item后拉起MapPointSearchFragment进行搜索;
            addPoiDetailsFragment(new MapPointSearchFragment(), MapPointSearchFragment.getBundle(FavoriteHelper.getInstance().getHomeCompanyType(), entity));
            return;
        }
        // 移除交通事件Fragment
        removeTrafficEventFragment();
        openPoiDetailFragment(entity);
    }

    private void removeTrafficEventFragment() {
        Logger.i(TAG, "removeTrafficEventFragment-start!");
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (!ConvertUtils.isNull(baseFragment) && baseFragment instanceof TrafficEventFragment) {
            closeFragment(true);
            Logger.i(TAG, "removeTrafficEventFragment-Success!");
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_MAP_DESTINATION_CHOOSE)
    private void openPoiDetailFragment(PoiInfoEntity entity) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_DETAIL, entity);
        bundle.putInt(AutoMapConstant.PoiBundleKey.BUNDLE_KEY_START_POI_TYPE, PoiType.POI_MAP_CLICK);
        PoiDetailsFragment fragment = new PoiDetailsFragment();
        addPoiDetailsFragment(fragment, bundle);

        //for burying point
        BuryProperty properties = new BuryProperty.Builder()
                .setParams(BuryConstant.ProperType.BURY_KEY_SEARCH_CONTENTS, entity.getName())
                .build();
        BuryPointController.getInstance().setBuryProps(properties);
    }

    public void toSearchResultFragment(final String keyword, final boolean isEnd) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, null);
        args.putBoolean(IVrBridgeConstant.VoiceIntentParams.IS_END, isEnd);
        addFragment(new SearchResultFragment(), args);
    }

    public void toSearchAroundFragment(String keyword, final int radius, PoiInfoEntity endPoint) {
        Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.AROUND_SEARCH);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, endPoint);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_RANGE, radius);
        addFragment(new SearchResultFragment(), args);
    }

    public void toRouteFragment(RouteSpeechRequestParam param) {
        Bundle args = new Bundle();
        args.putSerializable("speech_open_route", param);
        addFragment(new RouteFragment(), args);
    }

    public void openTrafficDetailFragment(PoiInfoEntity entity) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY, entity);
        TrafficEventFragment trafficEventFragment;
        BaseFragment fragment = StackManager.getInstance().getCurrentFragment(mScreenId);
        final Lifecycle.State currentState = mView.getLifecycle().getCurrentState();
        Logger.i(TAG, "openTrafficDetailFragment", "currentState:" + currentState.name());
        if (currentState == Lifecycle.State.RESUMED) {
            if (fragment != null && fragment instanceof TrafficEventFragment) {
                trafficEventFragment = (TrafficEventFragment) fragment;
                trafficEventFragment.setArguments(bundle);
                trafficEventFragment.onInitData();
            } else {
                trafficEventFragment = new TrafficEventFragment();
                addFragment(trafficEventFragment, bundle);
            }
        }
    }

    /**
     * 跳转到设置公司-家界面.
     *
     * @param type    int，0--Common 1-HOME 2-COMPANY.
     * @param keyword 搜索关键字.
     */
    public void toFavoriteFragment(final int type, final String keyword) {
        String sourceFragment = null;
        switch (type) {
            case 0:
                sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMMON;
                break;
            case 1:
                sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_HOME;
                break;
            case 2:
                sourceFragment = AutoMapConstant.SourceFragment.FRAGMENT_COMPANY;
                break;
            default:
                break;
        }
        if (null == sourceFragment) {
            return;
        }

        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, sourceFragment);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE,
                AutoMapConstant.SearchType.SEARCH_KEYWORD);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        addFragment(new SearchResultFragment(), args);
    }

    /**
     * 打开沿途搜界面.
     *
     * @param passBy String，沿途搜关键字.
     */
    public void toAlongWayFragment(final String passBy) {
        final Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT,
                AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE,
                AutoMapConstant.SearchType.ALONG_WAY_SEARCH);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, passBy);
        addFragment(new SearchResultFragment(), args);
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType isNight) {
        mModel.updateUiStyle(mapTypeId, isNight);
    }

    // TODO 到了下班时间收到回家推送消息，显示回家UI---待推送功能实现
    public void showNaviHome() {
        naviHomeVisibility.set(true);
    }

    // 保存最后一次定位的位置信息
    public void saveLastLocationInfo() {
        mModel.saveLastLocationInfo();
    }

    public void setLimitDriverVisibility(boolean visibility) {
        limitDriverVisibility.set(visibility);
    }

    public void updateLimitInfo(@Nullable RouteRestrictionParam param) {
        if (param == null) {
            return;
        }
        this.routeRestrictionParam = param;
        this.restrictedArea = param.getMRestrictedArea();
        // 导航中或者算路中不显示
        boolean statusVis = mModel.getNaviStatus() == NaviStatus.NaviStatusType.NO_STATUS || mModel.getNaviStatus() == NaviStatus.NaviStatusType.CRUISE;
        Logger.d(TAG, "statusVis:" + statusVis, "restrictedArea:" + (restrictedArea != null));

        boolean flag = false;
        for (RestrictedAreaDetail restrictedAreaDetail : this.restrictedArea.getMRestrictedAreaDetails().get(0)) {
            if (restrictedAreaDetail.getMEffect() == 1) {
                flag = true;
                break;
            }
        }
        if (flag) {
            limitDriverTitle.set(getApplication().getString(R.string.limit_today_drive));
        } else {
            limitDriverTitle.set(getApplication().getString(R.string.limit_drive));
        }
        limitDriverVisibility.set(restrictedArea != null && statusVis);

        if (restrictedArea != null && statusVis) {
            //首页消息的显示逻辑  发送package消息
            final boolean showSameDayLimit = mModel.showSameDayLimit();
            if (showSameDayLimit) {
                Logger.i("showSameDayLimit", "showSameDayLimit" + statusVis);
                mModel.managerMessage(new MessageCenterInfo(MessageCenterType.ROAD_LIMIT,
                        ResourceUtils.Companion.getInstance().getString(R.string.message_center_check), 0,
                        ResourceUtils.Companion.getInstance().getString(R.string.message_center_limit),
                        "", new Date(), 0));
            }
        }
    }

    /**
     * 离线地图是否15天未更新
     */
    public void offlineMap15Day() {
        if (mModel.offlineMap15Day()) {
            mModel.managerMessage(new MessageCenterInfo(MessageCenterType.MAP_UPDATE_15,
                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_update), 0,
                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_need_update),
                    "", new Date(), 0));
        }
    }

    /**
     * 离线地图是否45天未更新
     */
    public void offlineMap45Day() {
        if (mModel.offlineMap45Day()) {
            mModel.managerMessage(new MessageCenterInfo(MessageCenterType.MAP_UPDATE_45,
                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_update), 0,
                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_netless),
                    ResourceUtils.Companion.getInstance().getString(R.string.message_center_recommend_update),
                    new Date(), 0));
        }
    }

    /**
     * 继续导航
     */
    public void continueNavi(String content) {
        mModel.managerMessage(new MessageCenterInfo(MessageCenterType.CONTINUE_NAVI,
                ResourceUtils.Companion.getInstance().getString(R.string.message_center_continue_navi), 0,
                content,
                "",
                new Date(), 0));
    }

    /**
     * @param messageCenterInfo 数据
     */
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo) {
        //数据  点击事件需要使用
        messageCenterEntity.set(messageCenterInfo);
        //显示整个view
        messageCenterVisible.set(true);
        //最左边操作标题
        messageCenterOperate.set(messageCenterInfo.getMsgOperate());
        //标题
        messageCenterTitle.set(messageCenterInfo.getMsgTitle());
        //内容
        messageCenterContent.set(messageCenterInfo.getMsgContent());
        //最左边图片
        messageCenterImgVisibility.set(false);
        //关闭显示
        messageCloseVisibility.set(true);
        //2行显示长的竖线
        if (ConvertUtils.isEmpty(messageCenterInfo.getMsgContent())) {
            messageLineVisibility.set(true);
            messageLineHeightVisibility.set(false);
        } else {
            messageLineVisibility.set(false);
            messageLineHeightVisibility.set(true);
        }

        if (messageCenterInfo.getMsgType() == MessageCenterType.ROAD_LIMIT) {
            messageCenterContentVisibility.set(false);
        } else if (messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_15) {
            messageCenterContentVisibility.set(false);
        } else if (messageCenterInfo.getMsgType() == MessageCenterType.MAP_UPDATE_45) {
            messageCenterContentVisibility.set(true);
        } else if (messageCenterInfo.getMsgType() == MessageCenterType.WEATHER) {
            messageCenterOperate.set("");
            messageCenterContentVisibility.set(true);
            messageCenterImgVisibility.set(true);
            mView.setMessageImg(messageCenterInfo.getSrcImg());
        } else if (messageCenterInfo.getMsgType() == MessageCenterType.PHONE_MESSAGE) {
            //线和X图片隐藏
            messageCenterOperate.set("");
            messageCenterContentVisibility.set(true);
            messageCenterImgVisibility.set(true);
            mView.setMessageImg(messageCenterInfo.getSrcImg());
            messageLineVisibility.set(false);
            messageLineHeightVisibility.set(false);
            messageCloseVisibility.set(false);
        }
    }

    /**
     * 移除消息
     */
    public void onMessageInfoRemoveCallback() {
        messageCenterVisible.set(false);
    }

    // 如果处于特定状态不允许POI响应点击事件, 现在有两种“巡航”和“无状态”
    private boolean needInterceptor() {
        String currentStatus = mModel.getNaviStatus();
        return currentStatus == NaviStatus.NaviStatusType.NAVING
                || currentStatus == NaviStatus.NaviStatusType.LIGHT_NAVING
                || currentStatus == NaviStatus.NaviStatusType.SELECT_ROUTE
                || currentStatus == NaviStatus.NaviStatusType.ROUTING;
    }

    public void onNaviStatusChange() {
        updateLimitInfo(routeRestrictionParam);
    }

    //传递搜索参数
    public void setExtraKeyword(String keyword) {
        if (null == keyword || keyword.isEmpty()) {
            return;
        }
        mModel.setSearchKeyword(keyword);
    }

    /**
     * 获取收藏点（家、公司、常用地址、收藏）
     *
     * @param poiType
     * @return
     */
    @SuppressLint("SwitchIntDef")
    private PoiInfoEntity getFavoritePoiInfo(@PoiType int poiType) {
        return switch (poiType) {
            case PoiType.POI_HOME ->
                    BehaviorPackage.getInstance().getHomeFavoriteInfo(); //暂时取的都是本地的
            case PoiType.POI_COMPANY -> BehaviorPackage.getInstance().getCompanyFavoriteInfo();
            default -> null;
        };
    }

    /***触发算路***/
    public void startRoute(PoiInfoEntity poiInfoEntity) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
        addFragment(new RouteFragment(), bundle);
    }

    public IBaseScreenMapView getMapView() {
        return mView.getMapView();
    }

    public void showToast(@StringRes int res) {
        if (Looper.getMainLooper() != Looper.myLooper()) {
            ThreadManager.getInstance().postUi(() -> {
                ToastUtils.Companion.getInstance().showCustomToastView(mView.getString(res));
            });
        } else {
            ToastUtils.Companion.getInstance().showCustomToastView(mView.getString(res));
        }
    }

    // 更新巡航态下的车道信息
    public void updateCruiseLanInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mView.updateCruiseLanInfo(isShowLane, laneInfoEntity);
        cruiseLanesVisibility.set(cruiseVisibility.get() && !ConvertUtils.isNull(laneInfoEntity) && !ConvertUtils.isEmpty(laneInfoEntity.getBackLane()));
    }

    public void showOrHiddenCruise(boolean isShow) {
        cruiseVisibility.set(isShow);
        bottomNaviVisibility.set(!isShow && mainBTNVisibility.get());
    }

    /***
     * 判断巡航UI是否正在显示
     * @return
     */
    public boolean isCruiseUiVisible() {
        return cruiseVisibility.get();
    }

    public void setCruiseMuteOrUnMute(boolean isOpen) {
        muteVisibility.set(isOpen);
        mView.cruiseMuteOrUnMute(isOpen);
    }

    public void stopCruise() {
        mModel.stopCruise();
    }

    public void openRoute(PoiInfoEntity endPoint) {
        if (ConvertUtils.isEmpty(endPoint)) {
            return;
        }
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, endPoint);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
        addFragment(new RouteFragment(), bundle);
    }

    public void startNaviForRouteOver() {
        if (NaviStatus.NaviStatusType.SELECT_ROUTE == NaviStatusPackage.getInstance().getCurrentNaviStatus()) {
            Logger.i(TAG, "startNaviForRouteOver addNaviFragment");
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
            addFragment(new NaviGuidanceFragment(), bundle);
        }
    }

    public void openCollectFragment() {
        final Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB, 3);
        addFragment(new SettingFragment(), bundle);
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
        return mModel.powerType();
    }

    public void setTMCView(RouteTMCParam param) {

        //0代表家  1代表公司
        if (Boolean.TRUE.equals(param.isMIsShort()) && 0 == param.getMKey()) {
            homeTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
        }
        if (Boolean.TRUE.equals(param.isMIsShort()) && 1 == param.getMKey()) {
            companyTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
        }

        if (Boolean.FALSE.equals(param.isMIsShort()) && 0 == param.getMKey()) {
            homeTime.set(param.getMTime());
            if("计算中...".equals(param.getMTime())){
                homeTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
            }
            mView.setTMCView(param.getMKey(), param.getMRouteLightBarItem());
        }
        if (Boolean.FALSE.equals(param.isMIsShort()) && 1 == param.getMKey()) {
            companyTime.set(param.getMTime());
            if("计算中...".equals(param.getMTime())){
                companyTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
            }
            mView.setTMCView(param.getMKey(), param.getMRouteLightBarItem());
        }

        if(mModel.showNdGoHomeView()){
            mGoHomeVisible.set(true);
            mView.setNdGoHomeView(param);
            startGoHomeTimer();
            return;
        }

        if (0 == param.getMKey() && !ConvertUtils.isEmpty(getFavoritePoiInfo(PoiType.POI_COMPANY))) {
            mModel.refreshHomeOfficeTMC(false);
        }
    }

    public void checkPopGuideLogin() {
        mPopGuideLoginShow.set(mModel.checkPopGuideLogin());
    }

    public Action guideLoginClose = () -> {
        mPopGuideLoginShow.set(false);
        mModel.guideLoginClose();
        ToastUtils.Companion.getInstance()
                .showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.guide_login_close_toast));
    };

    public Action guideLoginBind = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_ACCOUNT_POPUP_BINDINGNOW)
        public void call() {
            mPopGuideLoginShow.set(false);
            mModel.guideLoginBind();
        }
    };

    public Action guideLoginCancel = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_ACCOUNT_POPUP_NOREMIND)
        public void call() {
            mPopGuideLoginShow.set(false);
            mModel.guideLoginCancel();
            ToastUtils.Companion.getInstance()
                    .showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.guide_login_close_toast));
        }
    };

    public void updateCruiseRoadName(CruiseInfoEntity cruiseInfoEntity) {
        mView.updateCruiseRoadName(cruiseInfoEntity);
    }

    /**
     * 显示隐私授权弹框
     *
     * @param dialog
     */
    public void showAuthorizationRequestDialog(final AuthorizationRequestDialog dialog) {
        if (dialog != null) {
            dialog.show();
        }
    }

    /**
     * 应用退到后台.
     */
    public void moveToBack() {
        if (null != mView) {
            mView.moveTaskToBack(false);
        }
    }

    void setScreenType(int right){}

    public void chargePreTipDialog(String status){
        if(ConvertUtils.isNull(mModel) || ConvertUtils.isNull(mView)) return;
        String title = "";
        String content = "";
        switch (status){
            case "timeQuick":
                title = ResourceUtils.Companion.getInstance().getString(R.string.quick_charge_pre);
                content = ResourceUtils.Companion.getInstance().getString(R.string.quick_charge_pre_content);
                break;
            case "timeOut":
                title = ResourceUtils.Companion.getInstance().getString(R.string.timeout_charge_pre);
                content = ResourceUtils.Companion.getInstance().getString(R.string.timeout_charge_pre_content);
                mModel.cancelTimeTick();
                break;
            default:
                mModel.cancelTimeTick();
                break;
        }
        String finalTitle = title;
        String finalContent = content;
        ThreadManager.getInstance().postUi(() -> mView.showTripDialog(finalTitle, finalContent));
    }

    public void closePoiFragment(){
        mView.closeAllFragmentsUntilTargetFragment(PoiDetailsFragment.class.getName());
    }
}
