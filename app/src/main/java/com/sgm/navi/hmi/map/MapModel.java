package com.sgm.navi.hmi.map;

import static com.sgm.navi.hmi.utils.AiWaysGestureManager.AiwaysGestureListener;
import static com.sgm.navi.hmi.utils.AiWaysGestureManager.GestureEvent;
import static com.sgm.navi.service.MapDefaultFinalTag.MAP_TOUCH;
import static com.sgm.navi.service.MapDefaultFinalTag.NAVI_EXIT;

import android.annotation.SuppressLint;
import android.content.ComponentName;
import android.content.Intent;
import android.os.Bundle;
import android.os.Looper;
import android.os.Parcelable;
import android.provider.Settings;
import android.text.TextUtils;
import android.view.MotionEvent;

import androidx.core.app.ActivityCompat;
import androidx.databinding.Observable;
import androidx.databinding.ObservableBoolean;
import androidx.fragment.app.Fragment;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.TimeUtils;
import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.NaviService;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.bean.BuryProperty;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.burypoint.controller.BuryPointController;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.account.AccountQRCodeLoginFragment;
import com.sgm.navi.hmi.activate.ActivateUiStateManager;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.launcher.IDeskBackgroundChangeListener;
import com.sgm.navi.hmi.launcher.OnDeskCardVisibleStateChangeListener;
import com.sgm.navi.hmi.message.MessageCenterHelper;
import com.sgm.navi.hmi.navi.AuthorizationRequestDialog;
import com.sgm.navi.hmi.navi.ContinueNaviDialog;
import com.sgm.navi.hmi.navi.ForecastAddressDialog;
import com.sgm.navi.hmi.navi.NaviGuidanceFragment;
import com.sgm.navi.hmi.navi.PhoneAddressDialog;
import com.sgm.navi.hmi.permission.PermissionUtils;
import com.sgm.navi.hmi.poi.PoiDetailsFragment;
import com.sgm.navi.hmi.route.RouteFragment;
import com.sgm.navi.hmi.search.parking.TerminalParkingFragment;
import com.sgm.navi.hmi.search.searchresult.SearchResultFragment;
import com.sgm.navi.hmi.setting.SettingFragment;
import com.sgm.navi.hmi.splitscreen.SplitFragment;
import com.sgm.navi.hmi.startup.StartupExceptionDialog;
import com.sgm.navi.hmi.traffic.TrafficEventFragment;
import com.sgm.navi.hmi.utils.AiWaysGestureManager;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.RoutePath;
import com.sgm.navi.scene.impl.imersive.ImersiveStatus;
import com.sgm.navi.scene.impl.imersive.ImmersiveStatusScene;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.adapter.navistatus.NavistatusAdapter;
import com.sgm.navi.service.define.aos.RestrictedEndNumberParam;
import com.sgm.navi.service.define.aos.RestrictedParam;
import com.sgm.navi.service.define.aos.TrafficRestrictResponseParam;
import com.sgm.navi.service.define.bean.GeoPoint;
import com.sgm.navi.service.define.bean.MapLabelItemBean;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.layer.refix.DynamicLevelMode;
import com.sgm.navi.service.define.layer.refix.LayerItemRoutePointClickResult;
import com.sgm.navi.service.define.layer.refix.LayerItemUserFavorite;
import com.sgm.navi.service.define.layer.refix.LayerPointItemType;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MapMode;
import com.sgm.navi.service.define.map.MapNotifyType;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.MapVisibleAreaDataManager;
import com.sgm.navi.service.define.map.MapVisibleAreaInfo;
import com.sgm.navi.service.define.map.MapVisibleAreaType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.message.MessageCenterInfo;
import com.sgm.navi.service.define.message.MessageCenterType;
import com.sgm.navi.service.define.mfc.MfcController;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.SoundInfoEntity;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.define.position.LocInfoBean;
import com.sgm.navi.service.define.route.RouteMsgPushInfo;
import com.sgm.navi.service.define.route.RoutePoiType;
import com.sgm.navi.service.define.route.RouteRestrictionParam;
import com.sgm.navi.service.define.route.RouteSpeechRequestParam;
import com.sgm.navi.service.define.route.RouteTMCParam;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.service.define.search.FavoriteInfo;
import com.sgm.navi.service.define.search.PoiInfoEntity;
import com.sgm.navi.service.define.setting.SettingController;
import com.sgm.navi.service.define.user.account.AccountProfileInfo;
import com.sgm.navi.service.define.user.forecast.ForecastArrivedDataInfo;
import com.sgm.navi.service.define.user.forecast.OftenArrivedItemInfo;
import com.sgm.navi.service.define.user.msgpush.MsgPushInfo;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.greendao.history.History;
import com.sgm.navi.service.greendao.history.HistoryManager;
import com.sgm.navi.service.greendao.setting.SettingManager;
import com.sgm.navi.service.logicpaket.agreement.AgreementPackage;
import com.sgm.navi.service.logicpaket.aos.AosRestrictedPackage;
import com.sgm.navi.service.logicpaket.aos.IAosRestrictedObserver;
import com.sgm.navi.service.logicpaket.calibration.CalibrationPackage;
import com.sgm.navi.service.logicpaket.cruise.CruisePackage;
import com.sgm.navi.service.logicpaket.cruise.ICruiseObserver;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.mapdata.MapDataPackage;
import com.sgm.navi.service.logicpaket.message.MessageCenterCallBack;
import com.sgm.navi.service.logicpaket.message.MessageCenterManager;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navi.OpenApiHelper;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.position.IPositionPackageCallback;
import com.sgm.navi.service.logicpaket.position.PositionPackage;
import com.sgm.navi.service.logicpaket.route.IRouteResultObserver;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.service.logicpaket.search.SearchPackage;
import com.sgm.navi.service.logicpaket.search.SearchResultCallback;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.service.logicpaket.setting.SettingUpdateObservable;
import com.sgm.navi.service.logicpaket.signal.SignalCallback;
import com.sgm.navi.service.logicpaket.signal.SignalPackage;
import com.sgm.navi.service.logicpaket.user.account.AccountPackage;
import com.sgm.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.sgm.navi.service.logicpaket.user.forecast.ForecastCallBack;
import com.sgm.navi.service.logicpaket.user.forecast.ForecastPackage;
import com.sgm.navi.service.logicpaket.user.forecast.IForecastAddressCallBack;
import com.sgm.navi.service.logicpaket.user.msgpush.MsgPushCallBack;
import com.sgm.navi.service.logicpaket.user.msgpush.MsgPushPackage;
import com.sgm.navi.service.logicpaket.user.usertrack.UserTrackPackage;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.ui.BuildConfig;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.BaseModel;
import com.sgm.navi.ui.base.FragmentIntent;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;
import com.sgm.navi.utils.ThreeFingerFlyingScreenManager;
import com.sgm.navi.vrbridge.IVrBridgeConstant;

import java.io.File;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/26
 */

public class MapModel extends BaseModel<MapViewModel> implements IMapPackageCallback,
        ImmersiveStatusScene.IImmersiveStatusCallBack, IAosRestrictedObserver, IPositionPackageCallback,
        SignalCallback, SpeedMonitor.CallBack, ICruiseObserver, SettingPackage.SettingChangeCallback,
        MsgPushCallBack, IGuidanceObserver, MessageCenterCallBack, IRouteResultObserver, ILayerPackageCallBack,
        ForecastCallBack, SearchResultCallback, NaviStatusCallback, SettingUpdateObservable.SettingUpdateObserver,
        IDeskBackgroundChangeListener, PermissionUtils.PermissionsObserver, StartService.ISdkInitCallback,
        OnDeskCardVisibleStateChangeListener, IForecastAddressCallBack,
        ScreenTypeUtils.SplitScreenChangeListener, FloatWindowReceiver.FloatWindowCallback  {

    private static final String TAG = "MapModel";
    private CommonManager mCommonManager;
    private StartupExceptionDialog mStartExceptionDialog = null;

    private FloatWindowReceiver floatWindowReceiver;
    private MapPackage mapPackage;
    private LayerPackage layerPackage;
    private PositionPackage positionPackage;
    private MapDataPackage mapDataPackage;
    private HistoryManager mHistoryManager;
    private AosRestrictedPackage restrictedPackage;
    private AiWaysGestureManager aiwaysGestureManager;
    private long limitQueryTaskId;
    private long limitEndNumberTaskId;
    private int mCurrentCityCode;
    private int mCompanyOrHomeType;
    private String mLicense = "";
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
    private CalibrationPackage mCalibrationPackage;
    private SettingPackage mSettingPackage;
    private RoutePackage mRoutePackage;
    private BehaviorPackage behaviorPackage;
    private SearchPackage searchPackage;
    private final String mCallbackId;
    private ForecastPackage mforCastPackage;
    private AccountPackage mAccountPackage;
    private NaviStatusPackage mNaviStatusPackage;
    private StackManager stackManager;
    private boolean mLoadMapSuccess = true;  //只加载一次
    // 24小时对应的毫秒数：24 * 60 * 60 * 1000 = 86,400,000
    private final long MILLIS_IN_24_HOURS = 86400000;
    private History mUncompletedNavi;
    private MapVisibleAreaDataManager mapVisibleAreaDataManager;
    private AuthorizationRequestDialog authorizationRequestDialog = null;

    private ForecastAddressDialog forecastAddressDialog;

    private boolean isReallyMove = false;

    private PhoneAddressDialog phoneAddressDialog;

    private final ActivateUiStateManager.LoadingViewCallBack mActivateViewCallBack =
            isShow -> ThreadManager.getInstance().postUi(() -> mViewModel.showActivatingView(isShow));

    public MapModel() {
        mCallbackId = UUID.randomUUID().toString();
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
    }

    private void setPackageAfterSdkInit() {
        mapPackage = MapPackage.getInstance();
        layerPackage = LayerPackage.getInstance();
        positionPackage = PositionPackage.getInstance();
        mapDataPackage = MapDataPackage.getInstance();
        restrictedPackage = AosRestrictedPackage.getInstance();
        restrictedPackage.addRestrictedObserver(IAosRestrictedObserver.KEY_OBSERVER_LIMIT, this);
        mHistoryManager = HistoryManager.getInstance();
        mHistoryManager.init();
        settingManager = SettingManager.getInstance();
        settingManager.init();
        cruisePackage = CruisePackage.getInstance();
        signalPackage = SignalPackage.getInstance();
        msgPushPackage = MsgPushPackage.getInstance();
        speedMonitor = new SpeedMonitor();
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
        stackManager = StackManager.getInstance();
        mapVisibleAreaDataManager = MapVisibleAreaDataManager.getInstance();
        addGestureListening();//添加收拾监听
        NaviStatusPackage.getInstance().registerObserver(TAG, this);
        SettingUpdateObservable.getInstance().addObserver(TAG, this);
        mapPackage.addTimeHelper(timeHelper);
        speedMonitor.registerSpeedCallBack();
        speedMonitor.registerCallBack(this);
        mViewModel.initVisibleAreaPoint();
        forecastAddressDialog = new ForecastAddressDialog(mViewModel.getView(), this);
    }

    private MapPackage.TimeHelper timeHelper = new MapPackage.TimeHelper() {
        @Override
        public void startTime() {
            mViewModel.startTime();
        }

        @Override
        public void stopTime() {
            mViewModel.stopTime();
        }
    };

    @Override
    public void onCreate() {
        super.onCreate();
        PermissionUtils.getInstance().setPermissionsObserver(this);
        StartService.getInstance().registerSdkCallback(TAG, this);
        AgreementPackage.getInstance().init();
        AgreementPackage.getInstance().setAgreementCallback("StartupModel",
                new AgreementPackage.AgreementCallback() {
                    @Override
                    public void agreementCallback(boolean isSGMAgreed) {
                        if (isSGMAgreed) {
                            mViewModel.checkPrivacyRights();
                        } else {
                            StackManager.getInstance().exitApp();
                        }
                    }
                });
        FloatViewManager.getInstance().addDeskBackgroundChangeListener(this);
        FloatViewManager.getInstance().addDeskCardVisibleChangeListener(this);
        ScreenTypeUtils.getInstance().addSplitScreenChangeListener(TAG, this);
        mViewModel.mainBTNVisibility.addOnPropertyChangedCallback(new Observable.OnPropertyChangedCallback() {
            @Override
            public void onPropertyChanged(Observable sender, int propertyId) {
                boolean value = ((ObservableBoolean) sender).get();
                if (phoneAddressDialog != null && phoneAddressDialog.isShowing()) {
                    phoneAddressDialog.resetDialogParams(
                            value ? ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_417)
                                    : ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_600));
                }
            }
        });
        LayerPackage.getInstance().registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);

        // 注册媒体悬浮窗广播
        FloatWindowReceiver.registerCallback(TAG, this);
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (null == mapPackage) {
            return;
        }
        mapPackage.removeTimeHelper(timeHelper);
        mapPackage.unBindMapView(mViewModel.getMapView());
        speedMonitor.removeCallBack();
        speedMonitor.unInit();
        mapModelHelp.unInit();
        aiwaysGestureManager = null;
        restrictedPackage.removeRestrictedObserver(IAosRestrictedObserver.KEY_OBSERVER_LIMIT);
        msgPushPackage.unregisterCallBack(TAG);
        cruisePackage.unregisterObserver(mViewModel.mScreenId);
        messageCenterManager.unRegisterCallBack(MessageCenterManager.MESSAGECENTERKEY);
        mRoutePackage.unRegisterRouteObserver("Map Activity");
        mforCastPackage.unregisterCallBack(this);
        searchPackage.unRegisterCallBack(mCallbackId);
        mSettingPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP.name());
        signalPackage.unregisterObserver(MapType.MAIN_SCREEN_MAIN_MAP.name());
        naviPackage.unregisterObserver(mViewModel.mScreenId);
        mapPackage.unRegisterCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        layerPackage.unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        NaviStatusPackage.getInstance().unregisterObserver(TAG);
        SettingUpdateObservable.getInstance().removeObserver(TAG, this);
        cancelSelfParkingTimer();
        cancelCloseTmcTimerWithoutNetwork();
        FloatViewManager.getInstance().removeDeskBackgroundChangeListener(this);
        AgreementPackage.getInstance().unRegisterAgreementCallback("StartupModel");
        FloatViewManager.getInstance().removeDeskCardVisibleChangeListener(this);
        ScreenTypeUtils.getInstance().removeSplitScreenChangeListener(TAG);
        clearDialog();
        LayerPackage.getInstance().unRegisterCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        FloatWindowReceiver.unregisterCallback(TAG);
        ActivateUiStateManager.getInstance().removeLoadingViewCallBack();
    }

    public void clearDialog() {
        if (null != forecastAddressDialog && forecastAddressDialog.isShowing()) {
            forecastAddressDialog.dismiss();
        }
        forecastAddressDialog = null;
    }

    @Override
    public void onPermissionsSuccess() {
        Logger.d(TAG, "权限都申请成功 检测网络和数据缓存");
        startInitEngine();
    }

    @Override
    public void onPermissionsFail() {
        if (Logger.openLog) {
            Logger.printStackTrace(NAVI_EXIT,true);
        }
        Logger.i(TAG, "权限申请失败无法进行下一步");
        ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.permission_quest_fail));
        System.exit(0);
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.i(TAG, "onSdkInitSuccess");
        StartService.getInstance().unregisterSdkCallback(this);
        setPackageAfterSdkInit();
        FloatViewManager.getInstance().showAllCardWidgets();
        BroadcastManager.getInstance().init();
        BroadcastManager.getInstance().sendSpiCollectingBroadcast();
        if (isShowStartupException()) {
            popStartupExceptionDialog();
        }else {
            ThreadManager.getInstance().postUi(() -> mViewModel.setSdkInitStatus(true));
        }
    }

    @Override
    public void onSdkBaseLibInitSuccess() {
        ActivateUiStateManager.getInstance().setLoadingViewCallBack(mActivateViewCallBack);
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        // 重试机制统一在NaviService中完成
    }

    /**
     * 设置是否同意了SGM协议.
     *
     * @param state 是否同意协议.
     */
    public void allowSGMAgreement(boolean state) {
        AgreementPackage.getInstance().allowSGMAgreement(state);
    }

    /**
     * 获取是否同意了SGM协议.
     *
     * @return  是否同意协议.
     */
    public boolean isAllowSGMAgreement() {
        return AgreementPackage.getInstance().isAllowSGMAgreement();
    }

    /**
     * 打开SGM协议弹窗.
     *
     * @return 是否同意协议.
     */
    public boolean showSGMAgreement(boolean show) {
        return AgreementPackage.getInstance().showSGMAgreement(show);
    }

    public boolean isFirstLauncher() {
        final boolean isFirstLauncher = TextUtils.isEmpty(
                mCommonManager.getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH)
        );
        Logger.i(TAG, "isFirstLauncher:" + isFirstLauncher);
        return isFirstLauncher;
    }

    /***
     * 同意协议后把标志位置为一个非空的字符
     */
    public void updateFirstLauncherFlag() {
        mCommonManager.insertOrReplace(UserDataCode.SETTING_FIRST_LAUNCH, "1");
    }

    public void checkPermission() {
        Logger.i(TAG, "checkPermission");
        if (PermissionUtils.getInstance().checkoutPermission()) {
            startInitEngine();
        } else {
            PermissionUtils.getInstance().requestPermission();
            FloatViewManager.getInstance().hideAllCardWidgets(false);
        }
    }

    public boolean isShowStartupException() {
        boolean isNetConnect = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        boolean isOfflineData = "1".equals(mCommonManager.getValueByKey(UserDataCode.SETTING_DOWNLOAD_LIST));
        Logger.d(TAG, "mStartExceptionDialog", "is net connect", isNetConnect, "is offline data", isOfflineData);
        boolean isShowException = !isNetConnect && !isOfflineData;
        Logger.d(TAG, "mStartExceptionDialog", "检测网络和离线数据", isShowException);
        return isShowException;
    }

    private boolean isCached() {
        final String[] dirPaths = {GBLCacheFilePath.BLS_LOG};
        final File[] dirs = new File[dirPaths.length];
        for (int i = 0; i < dirPaths.length; i++) {
            dirs[i] = new File(dirPaths[i]);
        }
        return FileUtils.getTotalSizeOfDirectories(dirs) > 0;
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN_FAIL)
    public void popStartupExceptionDialog() {
        if (null == mStartExceptionDialog || !mStartExceptionDialog.isShowing()) {
            mStartExceptionDialog = new StartupExceptionDialog(mViewModel.getView(), new IBaseDialogClickListener() {
                @Override
                public void onNetWorkConnect() {
                    if (null != mStartExceptionDialog && mStartExceptionDialog.isShowing()) {
                        Logger.d(TAG, "mStartExceptionDialog", "close");
                        mStartExceptionDialog.dismiss();
                    }
                    mViewModel.closeProtectView();
                    ThreadManager.getInstance().postUi(() -> mViewModel.setSdkInitStatus(true));
                }

                @Override
                public void onExit() {
                    FloatViewManager.getInstance().showAllCardWidgets();
                    if (FloatViewManager.getInstance().isNaviDeskBg()) {
                        if (null != mStartExceptionDialog && mStartExceptionDialog.isShowing()) {
                            Logger.d(TAG, "桌面地图隐藏弹窗");
                            mStartExceptionDialog.dismiss();
                            mViewModel.showProtectView();
                            mViewModel.protectMap(AutoMapConstant.CANCEL_NET_EXCEPTION_DIALOG);
                        }
                        if (!isShowStartupException()) {
                            Logger.d(TAG, "有网情况下弹窗依然存在");
                            mViewModel.closeProtectView();
                        }
                    } else {
                        if (null != mViewModel) {
                            Logger.d(TAG, "非桌面地图finish应用");
                            mViewModel.moveToBack();
                            ThreadManager.getInstance().asyncDelay(() -> mViewModel.getView().finish(), 400, TimeUnit.MILLISECONDS);
                        }
                    }
                }
            });
        }
        if (!mStartExceptionDialog.isShowing()) {
            Logger.d(TAG, "mStartExceptionDialog", "show");
            mStartExceptionDialog.show();
        }
        FloatViewManager.getInstance().hideAllCardWidgets(false);
    }

    public void startInitEngine() {
        if (StartService.getInstance().checkSdkIsAvailable()) {
            if (null == mapPackage || null == layerPackage) {
                onSdkInitSuccess();
            } else if (isAllowSGMAgreement() && !isFirstLauncher()) {
                checkAuthorizationExpired();
            }
        } else {
            Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "start navi Service");
            Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
        }
    }

    public void loadMapView(IBaseScreenMapView mapSurfaceView) {
        if (null == mapPackage || null == layerPackage) {
            onSdkInitSuccess();
            return;
        }
        boolean mapViewInitResult = MapPackage.getInstance().createMapView(MapType.MAIN_SCREEN_MAIN_MAP);
        if (!mapViewInitResult) return;
        mapPackage.bindMapView(mapSurfaceView);
        layerPackage.initLayer(MapType.MAIN_SCREEN_MAIN_MAP);
        layerPackage.initCarLogoByFlavor(MapType.MAIN_SCREEN_MAIN_MAP, BuildConfig.FLAVOR);
        mapPackage.registerCallback(MapType.MAIN_SCREEN_MAIN_MAP, this);
        layerPackage.registerCallBack(MapType.MAIN_SCREEN_MAIN_MAP, this);
        ImmersiveStatusScene.getInstance().registerCallback("MapModel", this);
        //map devices绑定成功后适配深浅色模式
        mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                ThemeUtils.INSTANCE.isNightModeEnabled(mapSurfaceView.getMapViewContext()) ? ThemeType.NIGHT : ThemeType.DAY);
        if (Logger.openLog) {
            Logger.d(TAG, "isNightModeEnabled ", ThemeUtils.INSTANCE.isNightModeEnabled(mapSurfaceView.getMapViewContext()));
        }
    }

    public void startListenMsg() {
        String uid = "";
        String valueJson = mCommonManager.getValueByKey(UserDataCode.SETTING_GET_USERINFO);
        Logger.i(TAG, "getUserInfo valueJson = " , valueJson);
        if (!TextUtils.isEmpty(valueJson)) {
            Logger.i(TAG, "Login = " , uid);
            msgPushPackage.startListen(uid);
        } else {
            Logger.i(TAG, "Logout");
        }
    }

    public void loadVisibleAreaJson(String jsonPath) {
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
        refreshMapMode();
    }

    public void refreshMapMode() {
        String data = mSettingPackage.getValueFromDB(SettingController.SETTING_GUIDE_MAP_MODE);
        MapMode currentMode = mapPackage.getCurrentMapMode(MapType.MAIN_SCREEN_MAIN_MAP);
        MapMode mapViewMode = MapMode.UP_2D;
        if (!TextUtils.isEmpty(data)) {
            switch (data) {
                case SettingController.VALUE_MAP_MODE_NORTH_2D:
                    mapViewMode = MapMode.NORTH_2D;
                    break;
                case SettingController.VALUE_MAP_MODE_CAR_3D:
                    mapViewMode = MapMode.UP_3D;
                    break;
                default:
                    break;
            }
        }
        if (mapViewMode == MapMode.UP_3D) {
            mapPackage.setPitchAngle(MapType.MAIN_SCREEN_MAIN_MAP,
                    AutoMapConstant.MAP_ZOOM_LEVEL_DEFAULT_3D_PATCHANGLE);
        }

        if(mapViewMode != currentMode) {
            mapPackage.switchMapMode(MapType.MAIN_SCREEN_MAIN_MAP, mapViewMode, true);
            mSettingPackage.setConfigKeyMapviewMode(mapViewMode.ordinal());
        }
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
            //通知launcher隐藏遮罩
            notifyLauncher(true);
            mSettingPackage.setSettingChangeCallback(mapTypeId.name(), this);
            layerPackage.setDefaultCarMode(mapTypeId);
            mapPackage.setMapCenter(mapTypeId, new GeoPoint(positionPackage.getLastCarLocation().getLongitude(),
                    positionPackage.getLastCarLocation().getLatitude()));
            signalPackage.registerObserver(mapTypeId.name(), this);
            setMapCenterInScreen();
            final int exportIntent = ExportIntentParam.getIntentPage();
            if (exportIntent == INaviConstant.OpenIntentPage.ZOOM_LEVEL) {
                mapPackage.goToCarPosition(mapTypeId, false, false);
            } else {
                mapPackage.goToCarPosition(mapTypeId);
            }
            ThreadManager.getInstance().postDelay(() -> mViewModel.hideStartIcon(), 600);
            naviPackage.registerObserver(mViewModel.mScreenId, this);
            // 注册监听
            cruisePackage.registerObserver(mViewModel.mScreenId, this);
            // 恢复偏好设置
            mapModelHelp.restoreSetting();

            // TODO: 2025/7/19 这句话会导致地图白屏暂时注释掉了 RTC-ID：1089066 @陈佳鑫
            //恢复列表搜全览
            ArrayList<PoiInfoEntity> resultPoints = searchPackage.getLastSearchResult().getSearchResultPoints();
            if(!ConvertUtils.isEmpty(resultPoints) && getTopFragment(SearchResultFragment.class)) {
                if (Logger.openLog) {
                    Logger.i(TAG, "恢复列表搜全览");
                }
                searchPackage.showPreview(resultPoints);
            }

            mViewModel.initTimer();
            addFavoriteToMap();
            if (speedMonitor != null) {
                speedMonitor.registerSpeedCallBack();
                speedMonitor.registerCallBack(this);
            } else {
                Logger.e(TAG, "speedMonitor is null");
            }
            processExportCommand();
            //如果处于导航状态，并且是全览或者是固定全览，从后台切到前台，进入全览
            if(mNaviStatusPackage.isGuidanceActive() && (naviPackage.getFixedOverViewStatus() || naviPackage.getPreviewStatus())){
                OpenApiHelper.enterPreview(mapTypeId);
            }
            mViewModel.showOrHideSelfParkingView(false);
            getCurrentCityLimit();
        }
    }

    @Override
    public void onMapTouchEvent(MapType mapTypeId, MotionEvent touchEvent) {
        // 退出巡航
        stopCruise();

        //三指飞屏 并将MapActivity推至后台
        openThreeFingerFlyingScreen(touchEvent);
        FloatViewManager.getInstance().hideWidgetsOnMapTouch(touchEvent);
    }

    @Override
    public void onScaleRotateBegin(MapType mapTypeId) {
        if (getNaviStatus() == NaviStatus.NaviStatusType.NAVING && mSettingPackage.getAutoScale()) {
            layerPackage.setDynamicLevelLock(MapType.MAIN_SCREEN_MAIN_MAP,DynamicLevelMode.DYNAMIC_LEVEL_GUIDE,true);
        }
    }

    @Override
    public void onScaleRotateEnd(MapType mapTypeId) {
        if (getNaviStatus() == NaviStatus.NaviStatusType.NAVING && mSettingPackage.getAutoScale()) {
            layerPackage.setDynamicLevelLock(MapType.MAIN_SCREEN_MAIN_MAP,DynamicLevelMode.DYNAMIC_LEVEL_GUIDE,false);
        }
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
    public void onMapMove(MapType mapTypeId, long px, long py, boolean moveEnd) {
        if(isReallyMove && moveEnd){
            isReallyMove = false;
            ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
            //move事件，并且move结束，开启回车位倒计时
            startSelfParkingTimer();
        }
    }

    @Override
    public void onMove(MapType mapTypeId, long px, long py) {
        isReallyMove = true;
    }

    @Override
    public void onMapClickPoi(MapType mapTypeId, PoiInfoEntity poiInfo) {
        stopCruise();
        mViewModel.toPoiDetailFragment(poiInfo);
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
    public void onNotifyMap(MapType mapTypeId, MapNotifyType eventType) {
        Logger.i(TAG, "onNotifyMap: ", eventType);
        if (Objects.requireNonNull(eventType) == MapNotifyType.REFRESH_SELF_PARKING_TIMER) {
            startSelfParkingTimer();
            if(parkingViewExist()) mViewModel.showOrHideSelfParkingView(true);
        }
    }

    @Override
    public void onImmersiveStatusChange(MapType mapTypeId, ImersiveStatus currentImersiveStatus) {
        if (Logger.openLog) {
            Logger.d(TAG, "onImmersiveStatusChange: ", parkingViewExist(), ", currentImersiveStatus: ", currentImersiveStatus);
        }
        if (ScreenTypeUtils.getInstance().isOneThirdScreen() || parkingViewExist()) {
            if (currentImersiveStatus == ImersiveStatus.TOUCH) {
                mViewModel.showOrHideSelfParkingView(true);
                layerPackage.setFollowMode(MapType.MAIN_SCREEN_MAIN_MAP, false);
                FloatViewManager.getInstance().hideAllCardWidgets(true);
                startSelfParkingTimer();
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
                ThreadManager.getInstance().postDelay(() -> {
                    layerPackage.openDynamicLevel(mapTypeId, DynamicLevelMode.DYNAMIC_LEVEL_GUIDE);
                }, 500);
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
        return getBottomNaviVisibility() || (getTopFragment(PoiDetailsFragment.class)
                && mapPackage.isSearchPoiDetailsFragment()) &&
                NaviStatusPackage.getInstance().getCurrentNaviStatus().equals(NaviStatus.NaviStatusType.NO_STATUS)
                || getTopFragment(TrafficEventFragment.class);
    }

    public boolean isOnlyExistTargetFragment(Class<? extends Fragment> targetClass) {
        return stackManager.getFragmentSize(MapType.MAIN_SCREEN_MAIN_MAP.name()) == 1 && getTopFragment(targetClass);
    }

    private boolean getTopFragment(Class<? extends Fragment> targetClass) {
        return mViewModel.getTopFragment(targetClass);
    }

    public boolean getBottomNaviVisibility() {
        return mViewModel.bottomNaviVisibility.get();
    }

    private void startSelfParkingTimer() {
        if(Logger.openLog) Logger.d(TAG, "startSelfParkingTimer");
        cancelSelfParkingTimer();
        if (ScreenTypeUtils.getInstance().isOneThirdScreen() || parkingViewExist()) {
            mSelfParkingTimer = ThreadManager.getInstance().asyncWithFixDelay(() -> {
                cancelSelfParkingTimer();
                if (ScreenTypeUtils.getInstance().isOneThirdScreen() || parkingViewExist()) {
                    ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.IMERSIVE);
                    if(Logger.openLog) Logger.d("onFinish-startSelfParkingTimer-true");
                    if (getTopFragment(PoiDetailsFragment.class)) {
                        closeFragment(true);
                    }
                }
                if(Logger.openLog) Logger.d("onFinish-startSelfParkingTimer");
            }, 15, 15);
        }
    }

    public void showParkingView() {
        Logger.d(TAG, MAP_TOUCH,"showParkingView");
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
        mViewModel.updateLimitInfo();
        mapModelHelp.onNaviStatusChange(naviStatus);
        Logger.i(TAG, "onNaviStatusChange:" , naviStatus);
        layerPackage.hideOrShowFavoriteMain(MapType.MAIN_SCREEN_MAIN_MAP, NaviStatus.NaviStatusType.NO_STATUS.equals(naviStatus) || NaviStatus.NaviStatusType.CRUISE.equals(naviStatus));
        if (NaviStatus.NaviStatusType.NAVING.equals(naviStatus)) {
            if (mViewModel != null) {
                MessageCenterType messageCenterType = mViewModel.getCurrentMsgType();
                if (MessageCenterType.CONTINUE_NAVI.equals(messageCenterType)) {
                    mViewModel.closeMessageCenter(false);
                }
            }
        }
        if (NaviStatus.NaviStatusType.NO_STATUS.equals(naviStatus) && !mSettingPackage.getPrivacyStatus()) {
            //导航结束，判断当前隐私协议状态，如果为拒绝，退出应用
            if (DeviceUtils.isCar(AppCache.getInstance().getMContext())) {
                if (authorizationRequestDialog != null && !authorizationRequestDialog.isShowing()) {
                    mViewModel.exitSelf();
                }
            }
        }
    }

    @Override
    public void onQueryTrafficEvent(MapType mapTypeId, PoiInfoEntity poiInfo) {
        Logger.i(TAG, "onQueryTrafficEvent:");
        if (TextUtils.equals(mapTypeId.name(), mViewModel.mScreenId)) {
            IMapPackageCallback.super.onQueryTrafficEvent(mapTypeId, poiInfo);
            mViewModel.openTrafficDetailFragment(poiInfo, true);
            ThreadManager.getInstance().postDelay(() -> {
                mViewModel.showOrHideSelfParkingView(true);
            }, 600);
        }
    }

    @Override
    public void onVoiceOpenPage(MapType mapTypeId, Bundle bundle) {
        if (MapType.MAIN_SCREEN_MAIN_MAP == mapTypeId && null != bundle && null != mViewModel) {
            final int voicePage = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE);
            Logger.i(TAG, "voicePage: ", voicePage);
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
                    final Parcelable routingParcelable = bundle.getParcelable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST);
                    if (routingParcelable instanceof RouteSpeechRequestParam param) {
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
                    final String curStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
                    Logger.d(IVrBridgeConstant.TAG, "current status:" + curStatus);
                    ThreadManager.getInstance().execute(() -> {
                        if (NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus) || NaviStatus.NaviStatusType.NAVING.equals(curStatus)) {
                            final int routeIndex = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.ROUTE_INDEX, 0);
                            mRoutePackage.selectRoute(MapType.MAIN_SCREEN_MAIN_MAP, routeIndex);
                            if (NaviStatus.NaviStatusType.SELECT_ROUTE.equals(curStatus)) {
                                mRoutePackage.voiceStartNavi();
                            }
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
                    final BaseFragment currentFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
                    Logger.d(IVrBridgeConstant.TAG, "currentFragment = ", currentFragment.getClass().getName());
                    if (!ConvertUtils.isNull(currentFragment)
                            && ConvertUtils.equals(currentFragment.getClass().getName(), RouteFragment.class.getName())
                            && mRoutePackage != null) {
                        mRoutePackage.clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
                        mRoutePackage.clearRestrictionView(MapType.MAIN_SCREEN_MAIN_MAP);
                    }
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
                case IVrBridgeConstant.VoiceIntentPage.TURN_TARGET:
                    //搜索结果翻页
                    final int page = bundle.getInt(IVrBridgeConstant.VoiceIntentParams.TARGET_PAGE, 0);
                    if (page > 0) {
                        mViewModel.turnSearchPage(page);
                    }
                default:
                    break;
            }
        }
    }

    public String getNaviStatus() {
        return NaviStatusPackage.getInstance().getCurrentNaviStatus();
    }

    private ContinueNaviDialog mContinueNaviDialog;

    /***校验继续导航***/
    public void checkContinueNavi() {
        if (TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            Logger.i(TAG, "NaviStatusType.NAVING");
            return;
        }
        try {
            if (mHistoryManager != null) {
                mUncompletedNavi = mHistoryManager.getUncompletedNavi();
                Logger.i(TAG, "NaviStatus:", getNaviStatus(), ", uncompletedNavi:", mUncompletedNavi);
                if (mUncompletedNavi == null) return;
                Date mUpdateTime = mUncompletedNavi.getMUpdateTime();
                if (mUpdateTime == null) {
                    Logger.i(TAG, "mUpdateTime null");
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
                if (TextUtils.isEmpty(mUncompletedNavi.getMViaPoint())) {
                    Logger.e(TAG, "getMViaPoint null");
                    return;
                }
                mViewModel.continueNavi(mUncompletedNavi.getMEndPoiName());
            }
        } catch (Exception exception) {
            Logger.e(TAG, "showNavTipDialog exception", exception.getMessage());
            onCancelContinueNaviClick();
        }
    }

    public void onContinueNaviClick() {
        if (mUncompletedNavi == null) {
            Logger.i(TAG, "mUncompletedNavi null");
            return;
        }
        onCancelContinueNaviClick();
        String midPoint = mUncompletedNavi.getMViaPoint();
        if (Logger.openLog) {
            Logger.d(TAG, "midPoint:", midPoint);
        }
        if (!ConvertUtils.isEmpty(midPoint)) {
            RouteSpeechRequestParam routeSpeechRequestParam = GsonUtils.fromJson(midPoint, RouteSpeechRequestParam.class);
            routeSpeechRequestParam.setMMapTypeId(MapType.MAIN_SCREEN_MAIN_MAP);
            final Bundle bundle = new Bundle();
            bundle.putInt(IVrBridgeConstant.VoiceIntentParams.INTENT_PAGE, IVrBridgeConstant.VoiceIntentPage.ROUTING);
            bundle.putParcelable(IVrBridgeConstant.VoiceIntentParams.ROUTE_REQUEST, routeSpeechRequestParam);
            MapPackage.getInstance().voiceOpenHmiPage(MapType.MAIN_SCREEN_MAIN_MAP, bundle);
            RoutePackage.getInstance().requestRouteFromSpeech(routeSpeechRequestParam);
        }
    }

    public void onCancelContinueNaviClick() {
        if (mUncompletedNavi == null) {
            return;
        }
        Logger.i(TAG, "onCancelContinueNaviClick setMIsCompleted(true)");
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
        if (!stackManager.isFragmentStackNull(mViewModel.mScreenId)) {
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
        Logger.d(TAG, "准备开启巡航");
        final boolean isSuccess = cruisePackage.startCruise();
        if (isSuccess) {
            Logger.d(TAG, "开启巡航成功");
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
        if (!TextUtils.equals(getNaviStatus(), NaviStatus.NaviStatusType.CRUISE)) {
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
                Logger.i(TAG, "notifyAimPoiPushMessage ", msg);
            }
            if (phoneAddressDialog != null && phoneAddressDialog.isShowing()) {
                phoneAddressDialog.dismiss();
                phoneAddressDialog = null;
            }

            phoneAddressDialog = new PhoneAddressDialog(
                    stackManager.getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
            phoneAddressDialog.setTitle(msg.getName());
            phoneAddressDialog.setDialogClickListener(new IBaseDialogClickListener() {
                @Override
                public void onCommitClick() {
                    mViewModel.startRoute(getPoiInfoEntityFromPushMessage(msg));
                }
            });
            phoneAddressDialog.showDialog(mViewModel.mainBTNVisibility.get());
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
                    bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MSG_PUSH_OPEN_ROUTE_TYPE, routeMsgPushInfo);
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
        if(Logger.openLog) Logger.d(TAG, "onForecastArrivedData: ", data, ", mCompanyOrHomeType: ", mCompanyOrHomeType);
        if (StackManager.getInstance().isExistFragment(MapType.MAIN_SCREEN_MAIN_MAP.name(), SettingFragment.class.getSimpleName())) {
            return;
        }
        //判断是否有家或者公司的数据
        switch (mCompanyOrHomeType){
            case AutoMapConstant.GuessPositionType.OTHER:
                if(data == null) return;
                ArrayList<OftenArrivedItemInfo> mOthers = data.getOthers();
                if (!ConvertUtils.isEmpty(mOthers)) {
                    OftenArrivedItemInfo mOther = mOthers.get(0);
                    if (AppCache.getInstance().isFirstOpenMap()) {
                        managerMessage(new MessageCenterInfo(MessageCenterType.GUESS_WANT_GO, "去这里", 0, mOther.getWstrPoiName(), "为你推荐", new Date(), 0, mOther));
                    }
                }
                break;
            case AutoMapConstant.GuessPositionType.HOME:
                OftenArrivedItemInfo mHomeInfo = (data != null ? data.getHome() : null);
                if (!ConvertUtils.isEmpty(mHomeInfo) && !ConvertUtils.isEmpty(mHomeInfo.getWstrAddress())) {
                    showForecastDialog(mCompanyOrHomeType, mHomeInfo);
                } else {
                    mViewModel.toHomeFragment();
                }
                break;
            case AutoMapConstant.GuessPositionType.COMPANY:
                OftenArrivedItemInfo mCompanyInfo = (data != null ? data.getCompany() : null);
                if (!ConvertUtils.isEmpty(mCompanyInfo) && !ConvertUtils.isEmpty(mCompanyInfo.getWstrAddress())) {
                    showForecastDialog(mCompanyOrHomeType, mCompanyInfo);
                } else {
                    mViewModel.toCompanyFragment();
                }
                break;
            default:
                break;
        }
    }


    public void getOnlineForecastArrivedData(int type) {
        Logger.i(TAG, "getOnlineForecastArrivedData: type " + type);
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
            if (AutoMapConstant.GuessPositionType.HOME == type) {
                mViewModel.toHomeFragment();
            } else if(AutoMapConstant.GuessPositionType.COMPANY == type) {
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
                carModel = ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_north_2d);
                break;
            case UP_2D:
                carModel = ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_up_2d);
                break;
            case UP_3D:
                carModel = ResourceUtils.Companion.getInstance().getString(com.sgm.navi.scene.R.string.navi_up_3d);
                break;
        }
        return carModel;
    }


    /**
     * 地图是否15天未更新
     */
    public boolean offlineMap15Day() {
        final String time = mCommonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_MAP_CHECK);
        if (!TextUtils.isEmpty(time)) {
            //超过15天
            final boolean after15Day = messageCenterHelper.isNotConnectedFor15Days(Long.parseLong(time));
            if (after15Day) {
                mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK, String.valueOf(System.currentTimeMillis()));
                Logger.i("offlineMap15Day", "+++");
                return true;
            }
        } else {
            mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_MAP_CHECK, String.valueOf(System.currentTimeMillis()));
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
        Logger.i("isHoliday", holiday);
        if (!holiday) {
            loadNdGoHomeView();
        }
    }

    private void loadNdGoHomeView() {
        if (mViewModel.showNdGoHomeView()) {
            String key = mCommonManager.getValueByKey(UserDataCode.MAP_ND_GO_HOME_KEY);
            String currentTime = TimeUtils.getInstance().getCurrentTimeToHour();
            Logger.d(TAG, "key:" + key + ",,, currentTime:" + currentTime);
            if (ConvertUtils.isEmpty(key) || !ConvertUtils.equals(key, currentTime)) {
                mCommonManager.insertOrReplace(UserDataCode.MAP_ND_GO_HOME_KEY, currentTime);

                //是否在上班时间段内  在家附近
                LocInfoBean locInfoBean = positionPackage.getLastCarLocation();
                boolean workHours = TimeUtils.isCurrentTimeInSpecialRange(true);
                GeoPoint nearByHome = mViewModel.nearByHome(true);
                GeoPoint nearByCompany = mViewModel.nearByHome(false);
                if (workHours && !ConvertUtils.isEmpty(nearByCompany) && !ConvertUtils.isEmpty(locInfoBean)) {
                    //判断距离是否大于等于1km 小于等于50km 去公司
                    boolean distanceCompany = calcStraightDistance(nearByCompany, locInfoBean);
                    if (distanceCompany) {
                        mViewModel.loadNdOfficeTmc(false);
                    }
                    return;
                }

                //是否在下班时间段内  在公司附近
                boolean endofWorkHours = TimeUtils.isCurrentTimeInSpecialRange(false);
                if (endofWorkHours && !ConvertUtils.isEmpty(nearByHome) && !ConvertUtils.isEmpty(locInfoBean)) {
                    //判断距离是否大于等于1km 小于等于50km 回家
                    boolean distanceHome = calcStraightDistance(nearByHome, locInfoBean);
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
        final String limitTime = mCommonManager.getValueByKey(UserDataCode.SETTING_MESSAGE_LIMIT_TIME);
        if (!TextUtils.isEmpty(limitTime)) {
            //在判断是否是同一天  同一天的也不显示
            final String currentDay = TimeUtils.convertYMD();
            if (!currentDay.equals(limitTime)) {
                mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, currentDay);
                return true;
            }
        } else {
            mCommonManager.insertOrReplace(UserDataCode.SETTING_MESSAGE_LIMIT_TIME, TimeUtils.convertYMD());
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
        if (!ConvertUtils.isEmpty(messageCenterInfo)) {
            mViewModel.onMessageInfoNotifyCallback(messageCenterInfo);
            sendBuryPointForPopup(messageCenterInfo.getMsgTitle());
        } else {
            Logger.e(TAG, "messageCenterInfo is null");
            sendBuryPointForPopup("");
        }
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
        mViewModel.showProtectView();
        authorizationRequestDialog = new AuthorizationRequestDialog(
                stackManager.getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()));
        authorizationRequestDialog.setEndDate(endDate);
        authorizationRequestDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                mSettingPackage.setPrivacyStatus(true);
                mViewModel.closeProtectView();
            }

            @Override
            public void onCancelClick() {
                if (FloatViewManager.getInstance().isNaviDeskBg()) {
                    Logger.d(TAG, "桌面地图情况");
                    mViewModel.showProtectView();
                    mViewModel.protectMap(AutoMapConstant.CANCEL_LOCATION_PROTOCOL);
                } else {
                    mViewModel.moveToBack();
                    ThreadManager.getInstance().asyncDelay(() -> StackManager.getInstance().exitApp(), 800, TimeUnit.MILLISECONDS);
                }
            }
        });
        Logger.d(TAG, "isForeground : ", ProcessManager.isAppInForeground());
        if (ProcessManager.isAppInForeground()) {
            mViewModel.showAuthorizationRequestDialog(authorizationRequestDialog);
        }
    }

    public void dismissAuthorizationRequestDialog() {
        if (authorizationRequestDialog != null && authorizationRequestDialog.isShowing()) {
            Logger.d(TAG, "dismissAuthorizationRequestDialog: ");
            authorizationRequestDialog.dismiss();
        }
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
        ThreadManager.getInstance().postDelay(() -> addPoiDetailsFragment(new PoiDetailsFragment(), bundle), 600);
    }

    public boolean checkPopGuideLogin() {
        BaseFragment baseFragment = StackManager.getInstance().getCurrentFragment(MapType.MAIN_SCREEN_MAIN_MAP.name());
        if (!ConvertUtils.isNull(baseFragment)) {
            Logger.d(MapDefaultFinalTag.SEARCH_HMI_TAG, "checkPopGuideLogin: baseFragment is not null");
            return false;
        }

        if (mAccountPackage.isLogin() || settingManager.getValueByKey(SettingController.GUIDE_LOGIN_IS_CANCEL).equals("1")) {
            return false;
        }

        String lastTimeStr = mCommonManager.getValueByKey(UserDataCode.GUIDE_LOGIN_LAST_TIME);
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
        mCommonManager.insertOrReplace(UserDataCode.GUIDE_LOGIN_LAST_TIME, String.valueOf(System.currentTimeMillis()));
    }

    public void guideLoginCancel() {
        settingManager.insertOrReplace(SettingController.GUIDE_LOGIN_IS_CANCEL, "1");
        mCommonManager.insertOrReplace(UserDataCode.GUIDE_LOGIN_LAST_TIME, "");
    }

    // MFC功能，不准删除
    public void mfcChangeZoom(boolean zoom) {
        if (Boolean.TRUE.equals(zoom)) {
            mapPackage.amplifyLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        } else {
            mapPackage.reduceLevel(MapType.MAIN_SCREEN_MAIN_MAP);
        }
    }
    // MFC功能，不准删除
    public void mfcMoveMap(MfcController mfcController, int moveDis) {
        ImmersiveStatusScene.getInstance().setImmersiveStatus(MapType.MAIN_SCREEN_MAIN_MAP, ImersiveStatus.TOUCH);
        //触控态开始回车位倒计时
        startSelfParkingTimer();
        // 退出巡航
        stopCruise();
        mapPackage.mfcMoveMap(MapType.MAIN_SCREEN_MAIN_MAP, mfcController, moveDis);
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
        if (AppCache.getInstance().getMContext()!=null){
            Logger.d(TAG, " addGestureListening: ");
            aiwaysGestureManager = new AiWaysGestureManager(AppCache.getInstance().getMContext(), new AiwaysGestureListener() {
                @Override
                public boolean mutiFingerSlipAction(GestureEvent gestureEvent, float startX, float startY, float endX, float endY, float velocityX, float velocityY) {
                    if ((gestureEvent == GestureEvent.THREE_GINGER_LEFT_SLIP)) {
                        Logger.d(TAG, "--------三指左滑=====||||");
                        ThreeFingerFlyingScreenManager.getInstance().triggerFlyingScreen(true);
                        if (stackManager.getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()).isTaskRoot() && ProcessManager.isAppInForeground() && !FloatViewManager.getInstance().isNaviDeskBg()){
                            Logger.i(TAG, "--------======桌面地图情况=====||||");
                            stackManager.getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()).moveTaskToBack(true);
                        }
                        return true;
                    } else if ((gestureEvent == GestureEvent.THREE_GINGER_RIGHT_SLIP)) {
                        Logger.d(TAG, "--------======||||三指右滑=====||||");
                        ThreeFingerFlyingScreenManager.getInstance().triggerFlyingScreen(false);
                        return true;
                    }
                    return false;
                }
            });
        }
    }

    /**
     * 主图加载完成后执行外部应用通过对外交互传递过来的指令.
     */
    private void processExportCommand() {
        final int intentPage = ExportIntentParam.getIntentPage();
        Logger.i(TAG, "open map intent page: ", intentPage);
        if(INaviConstant.OpenIntentPage.NONE == intentPage) {
            return;
        }

        ExportIntentParam.setIntentPage(INaviConstant.OpenIntentPage.NONE);
        if (null != mViewModel) {
            mViewModel.processPageIntent(intentPage);
        }
    }

    private void cancelCloseTmcTimerWithoutNetwork(){
        if (!ConvertUtils.isEmpty(mCloseTmcTimer)) {
            ThreadManager.getInstance().cancelDelayRun(mCloseTmcTimer);
            mCloseTmcTimer = null;
        }
    }

    public void openGuideFragment() {
        if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            // Activity被意外destroy需要恢复页面的时候Fragment栈一定是空的
            if (mViewModel == null || mViewModel.isFragmentStackNull()) {
                Logger.i(TAG, " add NaviGuidanceFragment");
                addFragment(new NaviGuidanceFragment(), null);
            } else {
                if (mViewModel.getTopFragment(SplitFragment.class)) {
                    Logger.i(TAG, " Top SplitFragment");
                } else if (mViewModel.getTopFragment(NaviGuidanceFragment.class)) {
                    Logger.i(TAG, " Top NaviGuidanceFragment");
                } else {
                    Logger.i(TAG, " Stack Not Null, Not SplitFragment");
                    addFragment(new NaviGuidanceFragment(), null);
                }
            }
        } else {
            Logger.i(TAG, " NaviStatus is not NAVING");
        }
    }

    public void openRouteFragment() {
        if (Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.SELECT_ROUTE)
                || Objects.equals(NaviStatusPackage.getInstance().getCurrentNaviStatus(), NaviStatus.NaviStatusType.ROUTING)) {
            // Activity被意外destroy需要恢复页面的时候Fragment栈一定是空的
            if (mViewModel == null || mViewModel.isFragmentStackNull()) {
                Logger.i(TAG, " add RouteFragment");
                Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                Bundle bundle = new Bundle();
                bundle.putBoolean(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MAP_OPEN_ROUTE_TYPE, true);
                addFragment((BaseFragment) fragment, bundle);
            } else {
                if (mViewModel.getTopFragment(SplitFragment.class)) {
                    Logger.i(TAG, " Top SplitFragment");
                } else if (mViewModel.getTopFragment(NaviGuidanceFragment.class)) {
                    Logger.i(TAG, " Top NaviGuidanceFragment");
                } else if (mViewModel.getTopFragment(RouteFragment.class)){
                    Logger.i(TAG, " Top RouteFragment");
                } else {
                    Logger.i(TAG, " Stack Not Null, Not SplitFragment");
                    Fragment fragment = (Fragment) ARouter.getInstance().build(RoutePath.Route.ROUTE_FRAGMENT).navigation();
                    Bundle bundle = new Bundle();
                    bundle.putBoolean(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_MAP_OPEN_ROUTE_TYPE, true);
                    addFragment((BaseFragment) fragment, bundle);
                }
            }
        } else {
            Logger.i(TAG, " NaviStatus is not Route");
        }
    }

    @Override
    public void onUpdateSetting(String key, boolean value) {
        Logger.i(NAVI_EXIT,"key", key, "value", value);
        if (SettingController.KEY_SETTING_PRIVACY_STATUS.equals(key)) {
            if (!value) {
                ThreadManager.getInstance().postDelay(() -> mViewModel.exitSelf(), 200);
            }
        }
    }

    @Override
    public void onDeskBackgroundChange(FloatViewManager.DesktopMode desktopMode) {
        mViewModel.onDeskBackgroundChange(desktopMode);
    }

    @Override
    public void onDeskCardVisibleStateChange(boolean isVisible) {
        mViewModel.onDeskCardVisibleStateChange(isVisible);
    }

    public void showForecastDialog(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if(forecastAddressDialog == null){
                forecastAddressDialog = new ForecastAddressDialog(mViewModel.getView(), this);
            }
            forecastAddressDialog.setForecastAddressInfo(type, oftenArrivedItemInfo);
            if(!forecastAddressDialog.isShowing()){
                forecastAddressDialog.show();
            }
        });
    }

    @Override
    public void AddForecastInfo(int type, OftenArrivedItemInfo oftenArrivedItemInfo) {
        addHomeOrCompanyInfoToSetting(type, oftenArrivedItemInfo);
        mViewModel.initTimer();
    }

    @Override
    public void addressClick(int type) {
        if (AutoMapConstant.HomeCompanyType.HOME == type) {
            mViewModel.toHomeFragment();
        } else {
            mViewModel.toCompanyFragment();
        }
    }
    private final ComponentName mLauncherComponentName = new ComponentName(
            "com.patac.launcher",
            "com.patac.launcher.Launcher");
    @Override
    public void onSplitScreenChanged() {
        if (mViewModel == null || mapPackage == null) {
            Logger.e(TAG, "mViewModel/mapPackage is null");
            return;
        }
        if (FloatViewManager.getInstance().isNaviDeskBg() && ScreenTypeUtils.getInstance().isFullScreen()) {
            Intent intent = new Intent();
            intent.setComponent(mLauncherComponentName);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            mViewModel.getView().startActivity(intent);
        }
        mapPackage.changeMapViewParams(mViewModel.getMapView());
        mViewModel.initVisibleAreaPoint();
        mViewModel.notifyScreenSizeChanged();
        mViewModel.updateLimitInfo();
        mViewModel.toSetCarPosition();
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d("screen_change_used", "打开1/3屏幕布局");
            checkStatusCloseAllFragmentAndClearAllLabel();
            mViewModel.syncFragment();
            addFragment(new SplitFragment(), null);
        } else {
            Logger.d("screen_change_used", "关闭1/3屏幕布局");
            mViewModel.closeSplitFragment();
        }
        mViewModel.musicTabVisibility.set(FloatWindowReceiver.isShowMusicTab && ScreenTypeUtils.getInstance().isFullScreen());
    }

    public void checkStatusCloseAllFragmentAndClearAllLabel() {
        String currentNaviStatus = NaviStatusPackage.getInstance().getCurrentNaviStatus();
        if (currentNaviStatus.equals(NaviStatus.NaviStatusType.NAVING) || currentNaviStatus.equals(NaviStatus.NaviStatusType.LIGHT_NAVING)) {
            Logger.d("screen_change_used", "当前导航状态不允许关闭所有Fragment和清除标记");
            return;
        }
        mViewModel.closeAllFragment();
        RoutePackage routePackage = RoutePackage.getInstance();
        if (currentNaviStatus.equals(NaviStatus.NaviStatusType.ROUTING)) {
            routePackage.abortRequest(MapType.MAIN_SCREEN_MAIN_MAP);
        }
        if ((currentNaviStatus.equals(NaviStatus.NaviStatusType.SELECT_ROUTE))) {
            routePackage.clearRestArea(MapType.MAIN_SCREEN_MAIN_MAP);
            routePackage.clearWeatherView(MapType.MAIN_SCREEN_MAIN_MAP);
            routePackage.clearRouteLine(MapType.MAIN_SCREEN_MAIN_MAP);
            return;
        }
        if (searchPackage != null) {
            searchPackage.clearPoiLabelMark();
            searchPackage.clearLabelMark();
        } else {
            Logger.e(TAG, "searchPackage is null");
        }
    }

    @Override
    public void onRouteItemClick(MapType mapTypeId, LayerPointItemType type, LayerItemRoutePointClickResult result) {
        if (Logger.openLog) {
            Logger.i(TAG, "onRouteItemClick: mapTypeId: ", mapTypeId, ", type: ", type,
                    ", result: ", result.toString());
        }
        if (Objects.requireNonNull(type) == LayerPointItemType.ROUTE_POINT_TRAFFIC_EVENT) {
            Logger.i(TAG, "onRouteItemClick: ROUTE_POINT_TRAFFIC_EVENT");
            if (!ConvertUtils.isNull(mViewModel)) {
                PoiInfoEntity poiInfo = new PoiInfoEntity();
                poiInfo.setPid(result.getEventID() + "");
                poiInfo.setMPoint(new GeoPoint(result.getLog(), result.getLat()));
                mViewModel.openTrafficDetailFragment(poiInfo, false);
            }
        }
    }

    @Override
    public void onMarkTerminalParkClickCallBack(final int index) {
        if (mViewModel.getTopFragment(TerminalParkingFragment.class)) {
            return;
        }
        final Bundle bundle = new Bundle();
        final GeoPoint point = new GeoPoint(mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP).getRealPos().getLon()
                , mRoutePackage.getEndPoint(MapType.MAIN_SCREEN_MAIN_MAP).getRealPos().getLat());
        bundle.putParcelable(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SEARCH_TYPE, point);
        bundle.putInt(AutoMapConstant.SearchBundleKey.BUNDLE_KEY_SELECT_INDEX, index);
        addPoiDetailsFragment(new TerminalParkingFragment(), bundle);
        mRoutePackage.clearEndParkPoint(MapType.MAIN_SCREEN_MAIN_MAP);
    }

    @Override
    public void onWindowSideChanged(boolean isOpenFloat) {
        Logger.d(TAG, "悬浮窗开关：" + isOpenFloat);
        if (mViewModel != null) {
            mViewModel.musicTabVisibility.set(isOpenFloat && ScreenTypeUtils.getInstance().isFullScreen());
        }
    }

    private void notifyLauncher(boolean isStart) {
        Logger.d("TagLauncher", "地图加载完毕，发送广播");
        Intent intent = new Intent();
        intent.setAction("com.sgm.navi.hmi.action.NOTIFY_LAUNCHER_SHOW_HIDE_STARTUP");
        intent.putExtra("navi_is_start", isStart); //是否是启动时
        if (mViewModel != null && mViewModel.getView() != null) {
            mViewModel.getView().sendBroadcast(intent);
            Logger.d("TagLauncher", "地图加载完毕，发送完毕");
        } else {
            Logger.e("TagLauncher", "notifyLauncher: mViewModel or getView is null");
        }
    }

    public void dismissDialog() {
        dismissAuthorizationRequestDialog();
        if (null != authorizationRequestDialog && authorizationRequestDialog.isShowing()) {
            authorizationRequestDialog.dismiss();
        }
        authorizationRequestDialog = null;
        if (null != mStartExceptionDialog && mStartExceptionDialog.isShowing()) mStartExceptionDialog.dismiss();
        mStartExceptionDialog = null;
    }
}