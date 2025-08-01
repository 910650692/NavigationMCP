package com.sgm.navi.hmi.map;

import static com.sgm.navi.service.MapDefaultFinalTag.SEARCH_HMI_TAG;

import android.annotation.SuppressLint;
import android.app.Application;
import android.content.Context;
import android.os.Bundle;
import android.os.Looper;
import android.view.MotionEvent;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;

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
import com.android.utils.process.ProcessManager;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.favorite.FavoriteHelper;
import com.sgm.navi.hmi.favorite.HomeCompanyFragment;
import com.sgm.navi.hmi.favorite.MapPointSearchFragment;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.limit.LimitCitySelectionFragment;
import com.sgm.navi.hmi.limit.LimitDriveFragment;
import com.sgm.navi.hmi.mapdata.MapDataFragment;
import com.sgm.navi.hmi.navi.AuthorizationRequestDialog;
import com.sgm.navi.hmi.navi.NaviGuidanceFragment;
import com.sgm.navi.hmi.permission.ReminderDialog;
import com.sgm.navi.hmi.poi.PoiDetailsFragment;
import com.sgm.navi.hmi.route.RouteFragment;
import com.sgm.navi.hmi.search.mainsearch.MainSearchFragment;
import com.sgm.navi.hmi.search.searchresult.SearchResultFragment;
import com.sgm.navi.hmi.setting.SettingFragment;
import com.sgm.navi.hmi.traffic.TrafficEventFragment;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.AutoMapConstant.PoiType;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.aos.RestrictedArea;
import com.sgm.navi.service.define.aos.RestrictedAreaDetail;
import com.sgm.navi.service.define.aos.TrafficRestrictResponseParam;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.message.MessageCenterInfo;
import com.sgm.navi.service.define.message.MessageCenterType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RouteRequestParam;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.route.RouteTMCParam;
import com.android.utils.ScreenTypeUtils;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.ui.action.Action;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.BaseViewModel;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;
import com.sgm.navi.vrbridge.IVrBridgeConstant;

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
    private boolean mFirstLaunch = false;
    private boolean mInitSdkSuccess = false;

    public ObservableBoolean startIconVisibility;
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
    public ObservableField<Boolean> limitEndNumVisibility;
    public ObservableField<Boolean> tmcModeVisibility;
    public ObservableField<String> limitDriverTitle;
    public ObservableField<String> limitEndNumOne;
    public ObservableField<String> limitEndNumTwo;
    public ObservableField<Boolean> cruiseVisibility;
    public ObservableField<Boolean> cruiseLanesVisibility;
    public ObservableField<Boolean> muteVisibility;
    public ObservableField<Boolean> mPopGuideLoginShow;
    public ObservableField<Boolean> mGoHomeVisible;

    public ObservableField<Boolean> musicTabVisibility;
    public ObservableField<Boolean> sRVisible;
    public ObservableField<Boolean> mIsFullScreen;
    public ObservableField<Boolean> mIsChangingConfigurations;
    public ObservableField<Boolean> mIsContinueNaviNotified;

    private ScheduledFuture mScheduledFuture;
    private ScheduledFuture goHomeTimer;
    private final int mTimer = 300;
    public ReminderDialog reminderDialog = null;
    private boolean mRemindDialogShow = false;
    private int mCurrentProtectState;

    public BaseMapViewModel(@NonNull Application application) {
        super(application);
        startIconVisibility = new ObservableBoolean(true);
        backToCcPVisibility = new ObservableBoolean(false);
        mainBTNVisibility = new ObservableBoolean(true);
        mScaleViewVisibility = new ObservableBoolean(true);
        naviHomeVisibility = new ObservableField<>(false);
        limitDriverVisibility = new ObservableField<>(false);
        limitEndNumVisibility = new ObservableField<>(false);
        tmcModeVisibility = new ObservableField<>(false);
        limitDriverTitle = new ObservableField<>("");
        limitEndNumOne = new ObservableField<>("");
        limitEndNumTwo = new ObservableField<>("");
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
        sRVisible = new ObservableField<>(isSupportSplitScreen());
        mIsFullScreen = new ObservableField<>(true);
        musicTabVisibility = new ObservableField<>(false);
        mIsChangingConfigurations = new ObservableField<>(false);
        mIsContinueNaviNotified = new ObservableField<>(false);
        Logger.d(TAG, "BaseMapViewModel = " + bottomNaviVisibility.get());
    }

    @Override
    protected MapModel initModel() {
        return new MapModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        checkAgreementRights();
        mCurrentProtectState = AutoMapConstant.ProtectState.NONE;
    }

    @Override
    public void onResume() {
        super.onResume();
        if (mModel.isFirstLauncher()) {
            popAgreementDialog();
        } else if (mInitSdkSuccess && mModel.isAllowSGMAgreement() && !mModel.isFirstLauncher()) {
            mModel.checkAuthorizationExpired();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (reminderDialog != null && reminderDialog.isShowing()) {
            reminderDialog.dismiss();
            mRemindDialogShow = false;
        }
    }

    public boolean getSdkInitStatus() {
        return mInitSdkSuccess;
    }

    public void checkAgreementRights() {
        Logger.i(TAG, "checkAgreementRights");
        if (!mModel.isAllowSGMAgreement()) {
            mModel.showSGMAgreement(true);
        } else {
            checkPrivacyRights();
        }
    }

    public void checkPrivacyRights() {
        mFirstLaunch = mModel.isFirstLauncher();
        Logger.i(TAG, "checkPrivacyRights: ", mFirstLaunch);
        if (mFirstLaunch) {
            popAgreementDialog();
        } else {
            mModel.checkPermission();
        }
    }

    public void judgeNetException() {
        if (mModel != null) {
            mModel.judgeNetException();
        }
    }

    public boolean judgeAutoProtocol() {
        return mModel.judgeAutoProtocol();
    }

    public void startTime() {
        mView.startTime();
    }

    public void stopTime() {
        mView.stopTime();
    }

    /**
     * 高德服务权限弹窗
     */
    public void popAgreementDialog() {
        if (reminderDialog != null && reminderDialog.isShowing()) {
            return;
        }
        reminderDialog = new ReminderDialog(mView, new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                FloatViewManager.getInstance().mRemindDialogShow = false;
                setCurrentProtectState(AutoMapConstant.ProtectState.NONE);
                mModel.updateFirstLauncherFlag();
                mModel.checkPermission();
            }

            @Override
            public void onCancelClick() {
                FloatViewManager.getInstance().mRemindDialogShow = false;
                if (FloatViewManager.getInstance().isNaviDeskBg()) {
                    Logger.d(TAG, "桌面地图情况");
                    setCurrentProtectState(AutoMapConstant.ProtectState.CANCEL_AUTO_PROTOCOL);
                } else {
                    moveToBack();
                    ThreadManager.getInstance().asyncDelay(new Runnable() {
                        @Override
                        public void run() {
                            mView.finish();
                            FloatViewManager.getInstance().showAllCardWidgets();
                        }
                    }, 800, TimeUnit.MILLISECONDS);
                }
            }
        });
        reminderDialog.show();
        FloatViewManager.getInstance().mRemindDialogShow = true;
        FloatViewManager.getInstance().hideAllCardWidgets(false);
    }

    public void dismissAuthorizationDialog() {
        mModel.dismissAuthorizationRequestDialog();
    }

    public void dismissReminderDialog() {
        if (reminderDialog != null && reminderDialog.isShowing()) {
            reminderDialog.dismiss();
            reminderDialog = null;
            mRemindDialogShow = false;
        }
    }

    public void reminderDialogReCreate() {
        if (reminderDialog != null && reminderDialog.isShowing()) {
            reminderDialog.dismiss();
            reminderDialog = null;
            popAgreementDialog();
        }
    }

    public boolean isRemindDialogShow() {
        return mRemindDialogShow;
    }

    /**
     * 用于保护桌面地图的定位隐私弹窗检测
     */
    public void checkAuthorizationExpired() {
        mModel.checkAuthorizationExpired();
    }

    /**
     * 用于保护桌面地图的无网络弹窗检测
     */
    public void checkPermission() {
        mModel.checkPermission();
    }

    /**
     * 显示激活加载图片
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        if (mView != null) {
            mView.showActivatingView(show);
        }
    }

    /**
     * 显示激活失败弹窗
     *
     * @param errCode errCode
     * @param msg msg
     */
    public void showActivateFailedDialog(final int errCode, final String msg) {
        if (mView != null) {
            mView.showActivateFailedDialog(errCode, msg);
        }
    }

    public boolean isActivateDialogShowing() {
        if (mView != null) {
            return mView.isActivateDialogShowing();
        }
        return false;
    }

    /**
     * 关闭激活失败弹窗
     */
    public void dismissActivateFailedDialog() {
        if (mView != null) {
            mView.dismissActivateFailedDialog();
        }
    }

    public void hideStartIcon() {
        startIconVisibility.set(false);
    }

    public void showStartIcon() {
        startIconVisibility.set(true);
    }

    @Override
    public void onStop() {
        super.onStop();
        if(null != mModel) StartService.getInstance().unregisterSdkCallback(mModel);
    }

    /**
     * 设置SDK激活状态
     *
     * @param successful true-初始化成功  false-失败
     */
    public void setSdkInitStatus(final boolean successful) {
        if (mInitSdkSuccess == successful) {
            return;
        }
        mInitSdkSuccess = successful;
        if (successful) {
            mView.doAfterInitSdk();
            if (Logger.openLog) {
                Logger.d(TAG, "IsFirstOpenMap: ", AppCache.getInstance().isFirstOpenMap(), " mIsContinueNaviNotified:", mIsContinueNaviNotified.get());
            }
            if (AppCache.getInstance().isFirstOpenMap() && Boolean.FALSE.equals(mIsContinueNaviNotified.get())) {
                mIsContinueNaviNotified.set(true);
                mModel.checkContinueNavi();
            }
            mFirstLaunch = mModel.isFirstLauncher();
            if (mModel.isAllowSGMAgreement() && !mFirstLaunch) {
                mModel.checkAuthorizationExpired();
            }
            bottomNaviVisibility.set(judgedBottomNaviVisibility());
            sRVisible.set(judgedSRVisibility());
            mScaleViewVisibility.set(judgedScaleViewVisibility());
        }
    }


    public Action intercept = () -> {
        // 拦截控件点击事件
        mModel.stopCruise();
    };

    public void getOnlineForecastArrivedData() {
        //猜你想去
        mModel.getOnlineForecastArrivedData(AutoMapConstant.GuessPositionType.OTHER);
    }

    //回自车位逻辑
    public Action backToSelfParking = () -> {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
        //取消倒计时
        if (mModel != null) mModel.cancelSelfParkingTimer();
    };

    // 播报和静音切换
    public Action muteOrUnMute = () -> {
        mModel.setCruiseVoice(Boolean.FALSE.equals(muteVisibility.get()));
    };

    public Action openSearchFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_SEARCH)
        public void call() {
            Logger.i(TAG, "openSearchFragment");
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MAIN_SEARCH_ICON,
                    AutoMapConstant.SearchType.MAIN_SEARCH_ICON);
            addFragment(new MainSearchFragment(), bundle);
            mModel.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
            mModel.stopCruise();
        }
    };

    public Action openHomeFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_HOME_QUICKACCESS)
        public void call() {
            Logger.i(TAG, "openHomeFragment");
            try {
                PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
                if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                    Logger.d(TAG, "hava data");
                    //直接进入导航到家
                    SearchPackage.getInstance().clearLabelMark();
                    startRoute(poiInfoEntity);
                    return;
                }

                mModel.getOnlineForecastArrivedData(AutoMapConstant.GuessPositionType.HOME);

            } catch (Exception e) {
                Logger.e(SEARCH_HMI_TAG, "skipFragment: Exception occurred", e);
            }
        }
    };

    public Action openCompanyFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_WORK_QUICKACCESS)
        public void call() {
            Logger.i(TAG, "openCompanyFragment");
            try {
                PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
                if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                    if (Logger.openLog) Logger.d(TAG, "hava data");
                    //直接进入导航到公司
                    SearchPackage.getInstance().clearLabelMark();
                    startRoute(poiInfoEntity);
                    return;
                }

                //判断是否存在预测数据
                mModel.getOnlineForecastArrivedData(AutoMapConstant.GuessPositionType.COMPANY);

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
                if (powerType == 0) {//油车 混动
                    bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, "加油站");
                } else {// 电车
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
            Logger.d(SEARCH_HMI_TAG, "searchExtraKeyword: ", keyword);
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
        if (!result) {
            ToastUtils.Companion.getInstance().showCustomToastView(String.
                    format(ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_map_mode_switch_fail), modeText));
            return;
        }
        ToastUtils.Companion.getInstance().showCustomToastView(String.
                format(ResourceUtils.Companion.getInstance().getString(
                        com.sgm.navi.scene.R.string.switch_car_angle), modeText));
    };

    public Action messageCenterGone = () -> {
        closeMessageCenter(true);
    };

    public void closeMessageCenter(boolean isCompletedNavi) {
        Logger.d(TAG, "isCompletedNavi: ", isCompletedNavi);
        messageCenterVisible.set(false);
        mModel.deleteMessage();
        if (isCompletedNavi) {
            MessageCenterInfo messageCenterInfo = messageCenterEntity.get();
            if (messageCenterInfo != null) {
                if (messageCenterInfo.getMsgType() == MessageCenterType.CONTINUE_NAVI) {
                    Logger.i(TAG, "onCancelContinueNaviClick");
                    mModel.onCancelContinueNaviClick();
                }
            }
        }
    }

    public MessageCenterType getCurrentMsgType() {
        if (messageCenterEntity == null) {
            return null;
        }
        MessageCenterInfo messageCenterInfo = messageCenterEntity.get();
        if (messageCenterInfo != null) {
            return messageCenterInfo.getMsgType();
        }
        return null;
    }

    public Action openLimitDetailFragment = new Action() {
        @Override
        @HookMethod(eventName = BuryConstant.EventName.AMAP_TRAFFICRESTRICT_CURRENT)
        public void call() {
            if (needInterceptor()) return;
            Bundle bundle = new Bundle();
            bundle.putParcelable(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_LIMIT_DRIVER, routeRestrictionParam);
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
            } else if (messageCenterInfo.getMsgType() == MessageCenterType.GUESS_WANT_GO) {
                OftenArrivedItemInfo oftenArrivedItemInfo = messageCenterInfo.getOftenArrivedItemInfo();
                if (oftenArrivedItemInfo != null) {
                    PoiInfoEntity poiInfoEntity = new PoiInfoEntity()
                            .setAddress(oftenArrivedItemInfo.getWstrAddress())
                            .setName(oftenArrivedItemInfo.getWstrPoiName());
                    if (oftenArrivedItemInfo.getStNaviCoord() != null && oftenArrivedItemInfo.getStNaviCoord().getLat() != NumberUtils.NUM_0 && oftenArrivedItemInfo.getStNaviCoord().getLon() != NumberUtils.NUM_0) {
                        poiInfoEntity.setPoint(oftenArrivedItemInfo.getStNaviCoord());
                        startRoute(poiInfoEntity);
                    } else if (oftenArrivedItemInfo.getStDisplayCoord() != null && oftenArrivedItemInfo.getStDisplayCoord().getLat() != NumberUtils.NUM_0 && oftenArrivedItemInfo.getStDisplayCoord().getLon() != NumberUtils.NUM_0) {
                        poiInfoEntity.setPoint(oftenArrivedItemInfo.getStDisplayCoord());
                        startRoute(poiInfoEntity);
                    } else {
                        if (Logger.openLog) Logger.e(TAG, "address is null");
                    }
                }
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

    public Fragment getTopFragment() {
        if (mView == null) {
            Logger.w(TAG, "mView is null");
            return null;
        }
        return mView.getSupportFragmentManager().findFragmentById(R.id.layout_fragment);
    }

    public boolean isFragmentStackNull() {
        return mView.isFragmentStackNull();
    }

    public void toHomeFragment() {
        //判断是否存在预测数据
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.HOME);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_HOME_COMPANY_TYPE,
                AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY);
        addFragment(new HomeCompanyFragment(), bundle);
        mModel.stopCruise();
    }

    public void toCompanyFragment() {
        Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.SEARCH_KEYWORD);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_HOME_COMPANY, AutoMapConstant.HomeCompanyType.COMPANY);
        bundle.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_HOME_COMPANY_TYPE,
                AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY);
        addFragment(new HomeCompanyFragment(), bundle);
        mModel.stopCruise();

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

    public void setMapCenterInScreen() {
        Logger.i(TAG, "setMapCenterInScreen");
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (baseFragment instanceof MainSearchFragment || baseFragment instanceof SettingFragment || baseFragment instanceof NaviGuidanceFragment) {
            mModel.goToCarPosition();
            mModel.setMapCenterInScreen();
            mModel.refreshMapMode();
        }
        final String state = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        boolean exist = StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), MainSearchFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), LimitDriveFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), LimitCitySelectionFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), SettingFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), PoiDetailsFragment.class.getSimpleName());
        // 如果是导航页面的话比例尺继续正常显示，算路界面正常显示比例尺
        mScaleViewVisibility.set((NaviStatus.NaviStatusType.SELECT_ROUTE.equals(state)
                || NaviStatus.NaviStatusType.ROUTING.equals(state) ||
                NaviStatus.NaviStatusType.NAVING.equals(state) || exist) && !FloatViewManager.getInstance().judgedWidgetIsVisible() && (!ScreenTypeUtils.getInstance().isOneThirdScreen()));
        mainBTNVisibility.set(false);
        bottomNaviVisibility.set(false);
        backToParkingVisibility.set(false);
        mPopGuideLoginShow.set(false);
        mGoHomeVisible.set(false);
        Logger.i(TAG, "setMapCenterInScreen() mainBTNVisibility: ", mainBTNVisibility
                , " bottomNaviVisibility: ", bottomNaviVisibility);
    }

    public void setMapCenterInScreen(final Bundle bundle) {
        int type = -1;
        int searchKey = 0;
        String fragment = "";
        String aroundFragment = "";
        String routeFragment = "";
        String homeCompanyFragment = "";

        if (bundle != null) {
            type = bundle.getInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, -1);
            searchKey = bundle.getInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MAIN_SEARCH_ICON, -1);
            fragment = bundle.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT);
            aroundFragment = bundle.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_AROUND);
            routeFragment = bundle.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_ROUTE_FRAGMENT_TYPE);
            homeCompanyFragment = bundle.getString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_HOME_COMPANY_TYPE);
        }
        Logger.i(TAG, "setMapCenterInScreen type:", type);
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (baseFragment instanceof MainSearchFragment ||
                (baseFragment instanceof SettingFragment &&
                        !NaviPackage.getInstance().getFixedOverViewStatus()) ||
                ((baseFragment instanceof NaviGuidanceFragment) &&
                        !NaviPackage.getInstance().getPreviewStatus())) {
            mModel.goToCarPosition();
            mModel.setMapCenterInScreen();
            mModel.refreshMapMode();
        }
        final String state = NavistatusAdapter.getInstance().getCurrentNaviStatus();
        // 如果是导航页面的话比例尺继续正常显示，算路界面正常显示比例尺 来自主图搜索的地图移动 也显示比例尺
        mScaleViewVisibility.set((type != -1
                || searchKey == AutoMapConstant.SearchType.MAIN_SEARCH_ICON
                || NaviStatus.NaviStatusType.SELECT_ROUTE.equals(state)
                || NaviStatus.NaviStatusType.ROUTING.equals(state)
                || NaviStatus.NaviStatusType.NAVING.equals(state))
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), LimitDriveFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), LimitCitySelectionFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), SettingFragment.class.getSimpleName())
                || StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), PoiDetailsFragment.class.getSimpleName())
                || AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT.equals(fragment)
                || AutoMapConstant.SourceFragment.FRAGMENT_AROUND.equals(fragment)
                || AutoMapConstant.SourceFragment.FRAGMENT_SEARCH_AROUND.equals(aroundFragment)
                || AutoMapConstant.SourceFragment.FRAGMENT_ROUTE.equals(routeFragment)
                || AutoMapConstant.SourceFragment.FRAGMENT_HOME_COMPANY.equals(homeCompanyFragment)
                && (!ScreenTypeUtils.getInstance().isOneThirdScreen()) && !FloatViewManager.getInstance().judgedWidgetIsVisible());
        mainBTNVisibility.set(false);
        bottomNaviVisibility.set(false);
        backToParkingVisibility.set(false);
        mPopGuideLoginShow.set(false);
        mGoHomeVisible.set(false);
        cancelTimer();
        Logger.i(TAG, "setMapCenterInScreen(bundle) mainBTNVisibility: ", mainBTNVisibility
                , " bottomNaviVisibility: ", bottomNaviVisibility);
    }

    public void resetMapCenterInScreen() {
        mView.setMapFocusable(true);
        mModel.refreshMapMode();
        mModel.resetMapCenterInScreen();
        mScaleViewVisibility.set(judgedBottomNaviVisibility());
        mainBTNVisibility.set(true);
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
        sRVisible.set(judgedSRVisibility());
        if (mModel.checkPopGuideLogin()) {
            mPopGuideLoginShow.set(true);
        }
        initTimer();
        Logger.i(TAG, "resetMapCenterInScreen mainBTNVisibility: ", mainBTNVisibility
                , " bottomNaviVisibility: ", bottomNaviVisibility);
    }

    public void resetVisibleInScreen() {
        mScaleViewVisibility.set(judgedBottomNaviVisibility());
        mainBTNVisibility.set(true);
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
        sRVisible.set(judgedSRVisibility());
        if (mModel.checkPopGuideLogin()) {
            mPopGuideLoginShow.set(true);
        }
        mModel.refreshMapMode();
        Logger.i(TAG, "resetMapCenterInScreen mainBTNVisibility: ", mainBTNVisibility
                , " bottomNaviVisibility: ", bottomNaviVisibility);
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
        if (mModel.showNdGoHomeView()) {
            tmcModeVisibility.set(false);
            PoiInfoEntity homePoi = getFavoritePoiInfo(PoiType.POI_HOME);
            PoiInfoEntity companyPoi = getFavoritePoiInfo(PoiType.POI_COMPANY);
            if (!ConvertUtils.isEmpty(homePoi) || !ConvertUtils.isEmpty(companyPoi)) {
                mModel.sendReqHolidayList();
            }
        } else {
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

    public GeoPoint nearByHome(boolean home) {
        PoiInfoEntity poiInfoEntity;
        if (home) {
            poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
        } else {
            poiInfoEntity = getFavoritePoiInfo(PoiType.POI_COMPANY);
        }
        if (!ConvertUtils.isEmpty(poiInfoEntity)) {
            return poiInfoEntity.getMPoint();
        }
        return null;
    }

    public void loadNdOfficeTmc(boolean home) {
        mModel.refreshHomeOfficeTMC(home);
    }

    public void loadNdGoHomeData() {
        mModel.loadNdGoHomeData();
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

    private void startGoHomeTimer() {
        cancelTimerGoHomeTimer();
        goHomeTimer = ThreadManager.getInstance().asyncAtFixDelay(new Runnable() {
            @Override
            public void run() {
                cancelTimerGoHomeTimer();
                mGoHomeVisible.set(false);
            }
        }, NumberUtils.NUM_30, NumberUtils.NUM_30);

    }

    private void cancelTimerGoHomeTimer() {
        if (!ConvertUtils.isEmpty(goHomeTimer)) {
            ThreadManager.getInstance().cancelDelayRun(goHomeTimer);
            goHomeTimer = null;
        }
    }

    public void setBacktoCCP(boolean isTouchStatus) {
        backToCcPVisibility.set(isTouchStatus);
    }

    public void showOrHideSelfParkingView(boolean visible) {
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) return;
        Logger.d(TAG, "showOrHideSelfParkingView visible:" + visible);
        backToParkingVisibility.set(visible);
    }

    public void goHomeOrCompany(int type) {
        Logger.d(TAG, "goHomeOrCompany", type);
        if (type == AutoMapConstant.HomeCompanyType.HOME) {
            PoiInfoEntity poiInfoEntity = getFavoritePoiInfo(PoiType.POI_HOME);
            if (poiInfoEntity != null && poiInfoEntity.getFavoriteInfo() != null) {
                //直接进入导航到家
                SearchPackage.getInstance().clearLabelMark();
                startRoute(poiInfoEntity);
            }
        } else if (type == AutoMapConstant.HomeCompanyType.COMPANY) {
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
            mView.updateOnMapScaleChanged(result, MapPackage.getInstance().getScaleLineLength(MapType.MAIN_SCREEN_MAIN_MAP));
            Logger.d(TAG, "scale: ", scale, ", result: ", result);
        } catch (Exception exception) {
            Logger.e(TAG, "updateOnMapScaleChanged, format error:" + exception.getMessage());
        }
    }

    public void toPoiDetailFragment(PoiInfoEntity entity) {

        if (needInterceptorByVai() && FavoriteHelper.getInstance().getHomeCompanyType() == -1) {
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
        try {
            BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
            if (!ConvertUtils.isNull(baseFragment) && baseFragment instanceof TrafficEventFragment) {
                closeTrafficEventFragment(true);
                Logger.i(TAG, "removeTrafficEventFragment-Success!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "removeTrafficEventFragment failed:", e.getMessage());
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
        addPoiDetailsFragment(new SearchResultFragment(), args);
    }

    public void toSearchAroundFragment(String keyword, final int radius, PoiInfoEntity endPoint, boolean asEnd) {
        Bundle args = new Bundle();
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SOURCE_FRAGMENT, AutoMapConstant.SourceFragment.MAIN_SEARCH_FRAGMENT);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, AutoMapConstant.SearchType.AROUND_SEARCH);
        args.putString(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_KEYWORD, keyword);
        args.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_POI_LIST, endPoint);
        args.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_RANGE, radius);
        args.putBoolean(IVrBridgeConstant.VoiceIntentParams.IS_END, asEnd);
        addPoiDetailsFragment(new SearchResultFragment(), args);
    }

    public void toRouteFragment(RouteSpeechRequestParam param) {
        Bundle args = new Bundle();
        args.putParcelable("speech_open_route", param);
        addFragment(new RouteFragment(), args);
    }

    public void openTrafficDetailFragment(PoiInfoEntity entity, boolean isNeedConvert) {
        Bundle bundle = new Bundle();
        bundle.putParcelable(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_ENTITY, entity);
        bundle.putBoolean(AutoMapConstant.TrafficEventBundleKey.BUNDLE_KEY_IS_NEED_CONVERT,
                isNeedConvert);
        TrafficEventFragment trafficEventFragment;
        BaseFragment fragment = StackManager.getInstance().getCurrentFragment(mScreenId);
        final Lifecycle.State currentState = mView.getLifecycle().getCurrentState();
        Logger.i(TAG, "openTrafficDetailFragment", "currentState:", currentState.name());
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
        addPoiDetailsFragment(new SearchResultFragment(), args);
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

    /**
     * 搜索结果页上一页、下一页、某一页.
     *
     * @param targetPage 直到页面.
     */
    public void turnSearchPage(final int targetPage) {
        if (targetPage < 1) {
            return;
        }

        BaseFragment currentFragment = StackManager.getInstance()
                .getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (currentFragment instanceof SearchResultFragment) {
            SearchResultFragment searchResultFragment = (SearchResultFragment) currentFragment;
            searchResultFragment.turnPage(targetPage);
        }
    }

    public void updateUiStyle(MapType mapTypeId, ThemeType isNight) {
        if (!StartService.getInstance().checkSdkIsNeedInit()) {
            mModel.updateUiStyle(mapTypeId, isNight);
        }
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
        if (!visibility) {
            this.routeRestrictionParam = null;
        }
    }

    public void updateLimitInfo(@Nullable RouteRestrictionParam param) {
        if (param == null) {
            return;
        }
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d(TAG, "One-third screen");
            limitDriverVisibility.set(false);
            return;
        }
        ThreadManager.getInstance().execute(() -> {
            routeRestrictionParam = param;
            restrictedArea = param.getMRestrictedArea();
            // 导航中或者算路中不显示
            boolean statusVis = mModel.getNaviStatus() == NaviStatus.NaviStatusType.NO_STATUS || mModel.getNaviStatus() == NaviStatus.NaviStatusType.CRUISE;
            Logger.d(TAG, "statusVis:", statusVis, "restrictedArea:", (restrictedArea != null));

            boolean flag = false;
            if (restrictedArea == null
                    || restrictedArea.getMRestrictedAreaDetails() == null
                    || restrictedArea.getMRestrictedAreaDetails().isEmpty()
                    || restrictedArea.getMRestrictedAreaDetails().get(0) == null
                    || restrictedArea.getMRestrictedAreaDetails().get(0).isEmpty()) {
                Logger.d(TAG, "limit info is null");
                limitDriverVisibility.set(false);
                return;
            }
            for (RestrictedAreaDetail restrictedAreaDetail : restrictedArea.getMRestrictedAreaDetails().get(0)) {
                if (restrictedAreaDetail.getMEffect() == 1) {
                    flag = true;
                    break;
                }
            }
            if (flag) {
                limitDriverTitle.set(getApplication().getString(R.string.limit_today_drive));
                limitEndNumVisibility.set(false);
                limitEndNumOne.set("");
                limitEndNumTwo.set("");
            } else {
                mModel.getCurrentCityLimitEndNumber();
            }
            limitDriverVisibility.set(restrictedArea != null && statusVis);

            if (restrictedArea != null && statusVis) {
                //首页消息的显示逻辑  发送package消息
                final boolean showSameDayLimit = mModel.showSameDayLimit();
                if (showSameDayLimit) {
                    Logger.i("showSameDayLimit", "showSameDayLimit", statusVis);
                    mModel.managerMessage(new MessageCenterInfo(MessageCenterType.ROAD_LIMIT,
                            ResourceUtils.Companion.getInstance().getString(R.string.message_center_check), 0,
                            ResourceUtils.Companion.getInstance().getString(R.string.message_center_limit),
                            "", new Date(), 0, null));
                }
            }
        });
    }

    /**
     * 更新限行尾号UI
     *
     * @param param 尾号限行内容
     */
    public void updateLimitEndNum(final TrafficRestrictResponseParam param) {
        if (param == null || param.getPlateNo() == null || param.getPlateNo().isEmpty()) {
            Logger.d(TAG, "no restricted end number");
            limitDriverTitle.set(getApplication().getString(R.string.limit_drive));
            limitEndNumVisibility.set(false);
            limitEndNumOne.set("");
            limitEndNumTwo.set("");
            return;
        }
        final String[] nums = param.getPlateNo().split(",");
        if (nums.length >= 2) {
            limitEndNumOne.set(nums[0].trim());
            limitEndNumTwo.set(nums[1].trim());
            limitDriverTitle.set(getApplication().getString(R.string.limit_with_end_number));
            limitEndNumVisibility.set(true);
        } else {
            limitEndNumOne.set(nums[0].trim());
            limitDriverTitle.set(getApplication().getString(R.string.limit_with_end_number));
            limitEndNumVisibility.set(true);
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
                    "", new Date(), 0, null));
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
                    new Date(), 0, null));
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
                new Date(), 0, null));
    }

    /**
     * @param messageCenterInfo 数据
     */
    public void onMessageInfoNotifyCallback(final MessageCenterInfo messageCenterInfo) {
        Logger.d(TAG, "onMessageInfoNotifyCallback", messageCenterInfo.getMsgType());
        //数据  点击事件需要使用
        messageCenterEntity.set(messageCenterInfo);
        //显示整个view, 1/3屏除外
        messageCenterVisible.set(true && !ScreenTypeUtils.getInstance().isOneThirdScreen());
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
        } else if (messageCenterInfo.getMsgType() == MessageCenterType.GUESS_WANT_GO) {
            messageCenterContentVisibility.set(true);
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

    /**
     * 如果处于特定状态不允许POI响应点击事件, 排除算路态
     *
     * @return
     */
    private boolean needInterceptorByVai() {
        String currentStatus = mModel.getNaviStatus();
        return currentStatus == NaviStatus.NaviStatusType.NAVING
                || currentStatus == NaviStatus.NaviStatusType.LIGHT_NAVING;
    }

    public void updateLimitInfo() {
        updateLimitInfo(routeRestrictionParam);
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
        switch (mModel.getNaviStatus()) {
            case NaviStatus.NaviStatusType.SELECT_ROUTE,
                 NaviStatus.NaviStatusType.ROUTING,
                 NaviStatus.NaviStatusType.NAVING:
                final RouteRequestParam routeRequestParam = new RouteRequestParam();
                routeRequestParam.setMPoiInfoEntity(poiInfoEntity);
                routeRequestParam.setMRoutePoiType(RoutePoiType.ROUTE_POI_TYPE_END);
                routeRequestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
                RoutePackage.getInstance().requestRoute(routeRequestParam);
                break;
            case NaviStatus.NaviStatusType.NO_STATUS,
                 NaviStatus.NaviStatusType.CRUISE,
                 NaviStatus.NaviStatusType.LIGHT_NAVING:
                Bundle bundle = new Bundle();
                bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE, poiInfoEntity);
                bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_OPEN_ROUTE_TYPE, RoutePoiType.ROUTE_POI_TYPE_END);
                addFragment(new RouteFragment(), bundle);
                break;
            default:
                break;
        }
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
        Logger.i(TAG, "showOrHiddenCruise: ",
                "bottomNaviVisibility: ", judgedBottomNaviVisibility());
        cruiseVisibility.set(isShow);
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
        if (backToParkingVisibility.get()) {
            backToParkingVisibility.set(false);
        }
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
            SearchPackage.getInstance().clearTypeMark(LayerPointItemType.SEARCH_PARENT_PARK);
            if (Logger.openLog) {
                Logger.i(TAG, "startNaviForRouteOver addNaviFragment");
            }
            Bundle bundle = new Bundle();
            bundle.putInt(AutoMapConstant.RouteBundleKey.BUNDLE_KEY_START_NAVI_SIM, AutoMapConstant.NaviType.NAVI_GPS);
            closeAllFragment();
            addFragment(new NaviGuidanceFragment(), bundle);
        }
    }

    public void openCollectFragment() {
        final Bundle bundle = new Bundle();
        bundle.putInt(AutoMapConstant.CommonBundleKey.BUNDLE_KEY_SETTING_TAB, 2);
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
            if ("计算中...".equals(param.getMTime())) {
                homeTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
            }
            mView.setTMCView(param.getMKey(), param.getMRouteLightBarItem());
        }
        if (Boolean.FALSE.equals(param.isMIsShort()) && 1 == param.getMKey()) {
            companyTime.set(param.getMTime());
            if ("计算中...".equals(param.getMTime())) {
                companyTime.set(ResourceUtils.Companion.getInstance().getString(R.string.map_in_around));
            }
            mView.setTMCView(param.getMKey(), param.getMRouteLightBarItem());
        }

        if (mModel.showNdGoHomeView()) {
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
            if (mModel != null) mModel.guideLoginBind();
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

    public void exitSelf() {
        final String restartFlag = CommonManager.getInstance().getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH);
        final boolean closeDelay = NaviStatusPackage.getInstance().isGuidanceActive();
        final boolean naviDesk = FloatViewManager.getInstance().isNaviDeskBg();
        Logger.d(MapDefaultFinalTag.NAVI_EXIT, "onUpdateSetting: restartFlag = ", restartFlag, " closeDelay = ", closeDelay, "naviDesk", naviDesk);
        if (closeDelay && !ConvertUtils.isEmpty(restartFlag)) {
            return;
        }
        if (ConvertUtils.isEmpty(restartFlag) || naviDesk) {
            ThreadManager.getInstance().asyncDelay(() -> {
                Logger.d(MapDefaultFinalTag.NAVI_EXIT, "地图进程重启 open navi");
                ProcessManager.restartProcess(mApplication);
            }, 800, TimeUnit.MILLISECONDS);
        }
        //closeAllFragment();
        moveToBack();
        ThreadManager.getInstance().asyncDelay(() -> {
            Logger.d(MapDefaultFinalTag.NAVI_EXIT, "地图进程重启 finish mapActivity");
            mView.finish();
        }, 400, TimeUnit.MILLISECONDS);
    }

    public void chargePreTipDialog(String status) {
        if (ConvertUtils.isNull(mModel) || ConvertUtils.isNull(mView)) return;
        String title = "";
        String content = "";
        switch (status) {
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

    /**
     * 处理外部应用打开Map对应页面指令.
     *
     * @param intentPage
     */
    public void processPageIntent(final int intentPage) {
        switch (intentPage) {
            case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                final String keyword = ExportIntentParam.getKeyword();
                if (!ConvertUtils.isEmpty(keyword)) {
                    searchForExtraKeyword(keyword);
                } else {
                    openSearchFragment.call();
                }
                ExportIntentParam.setKeyword("");
                break;
            case INaviConstant.OpenIntentPage.GO_HOME:
                openHomeFragment.call();
                break;
            case INaviConstant.OpenIntentPage.GO_COMPANY:
                openCompanyFragment.call();
                break;
            case INaviConstant.OpenIntentPage.POI_DETAIL_PAGE:
                final PoiInfoEntity poiInfo = ExportIntentParam.getPoiInfo();
                if (null != poiInfo) {
                    toPoiDetailFragment(poiInfo);
                }
                ExportIntentParam.setPoiInfo(null);
                break;
            case INaviConstant.OpenIntentPage.ROUTE_PAGE:
                final PoiInfoEntity endPoint = ExportIntentParam.getPoiInfo();
                if (null != endPoint) {
                    openRoute(endPoint);
                }
                ExportIntentParam.setPoiInfo(null);
                break;
            case INaviConstant.OpenIntentPage.START_NAVIGATION:
                startNaviForRouteOver();
                break;
            case INaviConstant.OpenIntentPage.SEARCH_RESULT_PAGE:
                searchForChargeStation.call();
                break;
            default:
                break;
        }
    }

    /***
     *
     * @return 如果需要分屏重写此方法返回 true 即可
     */
    public boolean isSupportSplitScreen() {
        return false;
    }

    public Action switchSr = () -> {
    };

    // 判断是否点击了非 EditText 区域
    public boolean isShouldHideKeyboard(View view, MotionEvent event) {
        if (view instanceof EditText) {
            int[] location = {0, 0};
            view.getLocationOnScreen(location);
            int left = location[0];
            int top = location[1];
            int right = left + view.getWidth();
            int bottom = top + view.getHeight();
            return !(event.getRawX() >= left && event.getRawX() <= right
                    && event.getRawY() >= top && event.getRawY() <= bottom);
        }
        return false;
    }

    public void hideKeyboard(View view) {
        InputMethodManager imm = (InputMethodManager) mView.getBaseContext().getSystemService(Context.INPUT_METHOD_SERVICE);
        if (imm != null) {
            imm.hideSoftInputFromWindow(view.getWindowToken(), 0);
        }
    }

    public void openGuideFragment() {
        if (mModel == null) {
            Logger.e(TAG, "openGuideFragment mModel is null");
            return;
        }
        mModel.openGuideFragment();
    }

    public void openRouteFragment() {
        if (mModel == null) {
            Logger.e(TAG, "openRouteFragment mModel is null");
            return;
        }
        mModel.openRouteFragment();
    }

    /**
     * 桌面背景模式切换后如果是导航桌面需要隐藏“分屏按钮”
     *
     * @param desktopMode
     */
    public void onDeskBackgroundChange(FloatViewManager.DesktopMode desktopMode) {
        Logger.d(TAG, "onDeskBackgroundChange", desktopMode.getValue(), "sRVisible", sRVisible.get());
        sRVisible.set(judgedSRVisibility());
    }

    /**
     * 桌面小卡片显示和隐藏状态回调
     *
     * @param isVisible true显示
     */
    public void onDeskCardVisibleStateChange(boolean isVisible) {
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
        mScaleViewVisibility.set(judgedScaleViewVisibility());
        sRVisible.set(judgedSRVisibility());
        if (isVisible) {
            backToParkingVisibility.set(false);
        }
    }

    public MapActivity getView() {
        return mView;
    }

    /***
     * 屏幕尺寸发生变化回调通知
     */
    public void notifyScreenSizeChanged() {
        final boolean currentScaleViewVisibility = mScaleViewVisibility.get();
        final boolean currentSRVisible = sRVisible.get();
        Logger.d(
                TAG, "notifyScreenSizeChanged",
                "currentScaleViewVisibility:", currentScaleViewVisibility,
                "currentSRVisible:", currentSRVisible
        );
        mScaleViewVisibility.set(judgedScaleViewVisibility());
        sRVisible.set(judgedSRVisibility());
        mIsFullScreen.set(ScreenTypeUtils.getInstance().isFullScreen());
    }

    public void toSetCarPosition() {
        //如果切1/3屏时，自车位置按钮显示，则隐藏
        if (ScreenTypeUtils.getInstance().isOneThirdScreen() && backToParkingVisibility.get()) {
            backToParkingVisibility.set(false);
        }
        mModel.setMapCenterInScreen();
        mModel.goToCarPosition();
    }

    /**
     * 判断底部导航按钮是否需要显示
     *
     * @return true 显示
     */
    private boolean judgedBottomNaviVisibility() {
        // 1/3屏需要隐藏
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d(TAG, "judgedBottomNaviVisibility", "1/3屏需要隐藏");
            return false;
        }
        // 导航桌面背景模式下，如果底部卡片显示的情况下需要隐藏
        if (FloatViewManager.getInstance().isNaviDeskBg() && FloatViewManager.getInstance().judgedWidgetIsVisible()) {
            Logger.d(TAG, "judgedBottomNaviVisibility", "导航桌面背景模式下，如果底部卡片显示的情况下需要隐藏");
            return false;
        }
        // 巡航态模式下，隐藏底部导航按钮
        if (ConvertUtils.equals(mModel.getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
            Logger.d(TAG, "judgedBottomNaviVisibility", "巡航态模式下，隐藏底部导航按钮");
            return false;
        }
        // 如果容器里面有Fragment,隐藏
        if (StackManager.getInstance().getCurrentFragment(mScreenId) != null) {
            Logger.d(TAG, "judgedBottomNaviVisibility", "如果容器里面有Fragment,隐藏");
            return false;
        }
        Logger.d(TAG, "judgedBottomNaviVisibility", "底部导航按钮当前状态为显示");
        return true;
    }

    /**
     * 判断缩放按钮是否需要显示
     *
     * @return true 显示
     */
    private boolean judgedScaleViewVisibility() {
        // 1/3屏需要隐藏
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d(TAG, "judgedScaleViewVisibility", "1/3屏需要隐藏");
            return false;
        }
        // 导航桌面背景模式下，如果底部卡片显示的情况下需要隐藏
        if (FloatViewManager.getInstance().isNaviDeskBg() && FloatViewManager.getInstance().judgedWidgetIsVisible()) {
            Logger.d(TAG, "judgedScaleViewVisibility", "导航桌面背景模式下，如果底部卡片显示的情况下需要隐藏");
            return false;
        }
        // 如果容器里面有Fragment,隐藏
        if (StackManager.getInstance().getCurrentFragment(mScreenId) != null) {
            Logger.d(TAG, "judgedScaleViewVisibility", "如果容器里面有Fragment,隐藏");
            return true;
        }
        return true;
    }

    /**
     * 判断底部SR分屏按钮是否需要显示
     *
     * @return true 显示
     */
    private boolean judgedSRVisibility() {
        // 1/3屏需要隐藏
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d(TAG, "judgedSRVisibility", "1/3屏需要隐藏");
            return false;
        }
        // 导航桌面背景下，widgets显示，则隐藏
        if (FloatViewManager.getInstance().isNaviDeskBg() && FloatViewManager.getInstance().judgedWidgetIsVisible()) {
            Logger.d(TAG, "judgedSRVisibility", "导航桌面背景下，widgets显示，则隐藏");
            return false;
        }
        return true;
    }

    private void checkViewState() {
        Logger.d(TAG, "checkViewState: ", "bottomNaviVisibility: ", judgedBottomNaviVisibility());
        sRVisible.set(judgedSRVisibility());
        mScaleViewVisibility.set(judgedScaleViewVisibility());
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
    }

    public void notifyStepOneThirdScreen() {
        checkViewState();
    }

    public void closeSplitFragment() {
        mView.closeSplitFragment();
    }

    public void updateUiOnHomeKeyClick() {
        if (backToParkingVisibility.get()) {
            backToSelfParking.call();
            mModel.goToCarPosition();
            backToParkingVisibility.set(false);
        }
        sRVisible.set(judgedSRVisibility());
        bottomNaviVisibility.set(judgedBottomNaviVisibility());
        mScaleViewVisibility.set(judgedScaleViewVisibility());
    }

    public void dismissDialog() {
        if (null != mModel) mModel.dismissDialog();
    }

    public void syncFragment() {
        if (null != mView) {
            mView.syncFragment();
        } else {
            Logger.e(TAG, "syncFragment mView is null");
        }
    }

    public boolean getGoHomeView() {
        return mGoHomeVisible.get();
    }

    public void setGoHomeView(boolean visible) {
        mGoHomeVisible.set(visible);
    }

    public int getCurrentProtectState() {
        return mCurrentProtectState;
    }

    public void setCurrentProtectState(int protectState) {
        this.mCurrentProtectState = protectState;
    }
}
