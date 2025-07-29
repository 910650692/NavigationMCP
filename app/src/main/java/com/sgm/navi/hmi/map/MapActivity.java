package com.sgm.navi.hmi.map;

import android.Manifest;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings;
import android.view.MotionEvent;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.constraintlayout.widget.ConstraintLayout;
import androidx.core.view.WindowCompat;
import androidx.databinding.Observable;
import androidx.databinding.ObservableBoolean;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.broadcast.HomeActionBroadcastReceiver;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.activate.ActivateFailedDialog;
import com.sgm.navi.hmi.activate.ActivateUiStateManager;
import com.sgm.navi.hmi.databinding.ActivityMapBinding;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.launcher.LauncherWindowService;
import com.sgm.navi.hmi.permission.PermissionUtils;
import com.sgm.navi.hmi.splitscreen.SplitFragment;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.scene.dialog.MsgTopDialog;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MainScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.route.RouteLightBarItem;
import com.sgm.navi.service.define.route.RouteTMCParam;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.base.FragmentIntent;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.define.TripID;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Locale;
import java.util.concurrent.TimeUnit;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/01
 */
public class MapActivity extends BaseActivity<ActivityMapBinding, MapViewModel> {

    private static final String TAG = "MapActivity";
    private static final String KEY_CHANGE_SAVE_INSTANCE = "key_change_save_instance";

    private Animation mRotateAnim;

    private MainScreenMapView mapView;
    private MsgTopDialog mMsgTopDialog;
    private Runnable mOpenGuideRunnable;

    private Runnable timerRunnable = null;
    private int mCurrentUiMode;
    private ActivateFailedDialog mFailedDialog;

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.MAIN_SCREEN_MAIN_MAP.name();
        isMapActivity = true;
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);
        getWindow().setNavigationBarColor(getResources().getColor(R.color.route_charge_param_color));
        mBinding.cruiseLayout.tvCurrentRoadName.setSoundEffectsEnabled(false);
        mRotateAnim = AnimationUtils.loadAnimation(this, R.anim.rotate_animation);
        mRotateAnim.setDuration(2000);
        mRotateAnim.setRepeatCount(Animation.INFINITE);
        mRotateAnim.setInterpolator(new LinearInterpolator());
        mBinding.mainImg.setVisibility(View.VISIBLE);
        mBinding.mainImg.setOnClickListener(v -> FloatViewManager.getInstance().hideAllCardWidgets(false));
        mCurrentUiMode = getResources().getConfiguration().uiMode;
        mViewModel.mainBTNVisibility.addOnPropertyChangedCallback(propertyChangedCallback);
        mOpenGuideRunnable = () -> {
            mViewModel.openGuideFragment();
            mViewModel.openRouteFragment();
        };
        ThreadManager.getInstance().postDelay(mOpenGuideRunnable, NumberUtils.NUM_500);
        mViewModel.musicTabVisibility.set(ScreenTypeUtils.getInstance().isFullScreen() && FloatWindowReceiver.isShowMusicTab);
    }

    /**
     * 桌面地图不同意协议后处理
     *
     * @param situation 1是高德协议， 2是定位协议
     */
    public void protectMap(final int situation) {
        switch (situation) {
            case AutoMapConstant.CANCEL_AUTO_PROTOCOL:
                mBinding.protectView.setOnClickListener(v -> {
                    Logger.d(TAG, "protectMap: 高德协议");
                    mViewModel.checkPrivacyRights();
                    mBinding.protectView.setOnClickListener(null);
                });
                break;
            case AutoMapConstant.CANCEL_LOCATION_PROTOCOL:
                mBinding.protectView.setOnClickListener(v -> {
                    Logger.d(TAG, "protectMap: 定位协议");
                    mViewModel.checkAuthorizationExpired();
                    mBinding.protectView.setOnClickListener(null);
                });
                break;
            case AutoMapConstant.CANCEL_NET_EXCEPTION_DIALOG:
                mBinding.protectView.setOnClickListener(v -> {
                    Logger.d(TAG, "protectMap: 网络异常");
                    closeProtectView();
                    mViewModel.checkPermission();
                    mBinding.protectView.setOnClickListener(null);
                });
                break;
            default:
                Logger.e(TAG, "protectMap: situation is not supported");
                break;
        }
    }

    public void closeProtectView() {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                mBinding.protectView.setVisibility(View.GONE);
            }
        });
    }

    public void showProtectView() {
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                mBinding.protectView.setVisibility(View.VISIBLE);
            }
        });
    }

    private void updateTimeText() {
        // 更新时间格式
        SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss", Locale.getDefault());
        String currentTime = sdf.format(new Date());
        mBinding.screenScaleDate.setText(currentTime);
        // 每秒更新一次
        ThreadManager.getInstance().postDelay(timerRunnable, 1000);
    }

    public void startTime() {
        if (timerRunnable == null) {
            timerRunnable = this::updateTimeText;
            mBinding.screenScaleDate.setVisibility(View.VISIBLE);
            updateTimeText();
        }
    }

    public void stopTime() {
        if (timerRunnable != null) {
            ThreadManager.getInstance().removeHandleTask(timerRunnable);
            mBinding.screenScaleDate.setVisibility(View.GONE);
            timerRunnable = null;
        }
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_map;
    }

    @Override
    public int onFragmentId() {
        return R.id.layout_fragment;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        if (StartService.getInstance().checkSdkIsAvailable()) {
            mViewModel.loadMapView(mBinding.mainMapview);
        }
    }

    @Override
    public void onInitData() {
        Intent intent = getIntent();
        if (intent != null && intent.getIntExtra(BuryConstant.EventName.AMAP_RETURN_DEFAULT, 0)
                == BuryConstant.EventName.AMAP_RETURN_DEFAULT_CODE) {
            sendBuryPointForReset();
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_RETURN_DEFAULT)
    private void sendBuryPointForReset() {
    }

    @Override
    protected void onStart() {
        super.onStart();
        LauncherWindowService.getInstance().showOrHideFloatView(false);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        if (!ConvertUtils.isEmpty(outState)) {
            outState.putBoolean(KEY_CHANGE_SAVE_INSTANCE, true);
        }
    }

    @Override
    protected void onRestoreInstanceState(@NonNull Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
        if (!ConvertUtils.isEmpty(savedInstanceState)) {
            boolean isNeedToUpdateData = savedInstanceState.getBoolean(KEY_CHANGE_SAVE_INSTANCE);
            //todo 页面恢复，请恢复数据
        }
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN)
    protected void onResume() {
        super.onResume();
        syncFragment();
        if (mViewModel.isSupportSplitScreen()) {
            ScreenTypeUtils.getInstance().checkScreenType(getResources().getDisplayMetrics());
        }
        if (mViewModel.getSdkInitStatus()) {
            mViewModel.getCurrentCityLimit();
            //界面可见时重新适配深浅色模式
            mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                    ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
            if (Logger.openLog) {
                Logger.d(TAG, "isNightModeEnabled ", ThemeUtils.INSTANCE.isNightModeEnabled(this));
            }
            setChargeGasImage();
        }
        if (ActivateUiStateManager.getInstance().isActivating()) {
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "展示加载动画");
            showActivatingView(ActivateUiStateManager.getInstance().isActivating());
        } else if (ActivateUiStateManager.getInstance().isActivateFailed()) {
            int errCode = ActivateUiStateManager.getInstance().getErrorCodeSave();
            String msg = ActivateUiStateManager.getInstance().getErrorMsgSave();
            Logger.e(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "展示失败弹窗:", errCode, msg);
            if (!ConvertUtils.equals(errCode, 0) && !ConvertUtils.isEmpty(msg)) {
                showActivateFailedDialog(errCode, msg);
            }
        }

        //check Home键广播
        if (!HomeActionBroadcastReceiver.isRegister) {
            Logger.d(TAG, "registerHomeActionReceiver");
            HomeActionBroadcastReceiver.registerHomeActionReceiver();
        }

        if (mViewModel.getGoHomeView()) {
            mViewModel.setGoHomeView(false);
            mViewModel.loadNdGoHomeData();
        }
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_HIDE)
    protected void onStop() {
        Logger.i(TAG, "onStop");
        mViewModel.dismissAuthorizationDialog();
        super.onStop();
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_CLOSE)
    protected void onDestroy() {
        PermissionUtils.getInstance().remove();
        stopTime();
        // 退出的时候主动保存一下最后的定位信息
        if (mViewModel.getSdkInitStatus()) {
            mViewModel.saveLastLocationInfo();
            ThreadManager.getInstance().removeHandleTask(mOpenGuideRunnable);
        }
        dismissActivateFailedDialog();

        if (mMsgTopDialog != null && mMsgTopDialog.isShowing()) {
            mMsgTopDialog.dismiss();
        }
        mMsgTopDialog = null;
        AppCache.getInstance().setFirstOpenMap(false);
        mViewModel.mainBTNVisibility.removeOnPropertyChangedCallback(propertyChangedCallback);
        if (null != mViewModel) mViewModel.dismissDialog();
        Logger.i(TAG, "onDestroy");
        super.onDestroy();
    }

    public void doAfterInitSdk() {
        //initData
        mViewModel.loadMapView(mBinding.mainMapview);
        // 给限行设置点击事件
        mBinding.includeLimit.setViewModel(mViewModel);
        mBinding.cruiseLayout.setViewModel(mViewModel);
        setChargeGasImage();
        mBinding.includeMessageCenter.setViewModel(mViewModel);


        //initData
        mViewModel.startListenMsg();
        mViewModel.offlineMap15Day();
        mViewModel.offlineMap45Day();
        if (!mStackManager.isExistFragment(mScreenId, "AccountQRCodeLoginFragment")) {
            mViewModel.checkPopGuideLogin();
        }
        if (Boolean.FALSE.equals(mViewModel.mIsChangingConfigurations.get()))
            mViewModel.getOnlineForecastArrivedData();
    }

    public void setChargeGasImage() {
        if (!ConvertUtils.isNull(mViewModel) && !ConvertUtils.isNull(mBinding)) {
            final int powerType = mViewModel.powerType();
            // 油车
            if (powerType == 0) {
                mBinding.skIvBasicRouting.setImageResource(R.drawable.img_home_gas_station);
            } else {
                mBinding.skIvBasicRouting.setImageResource(R.drawable.img_basic_ic_gas_charging);
            }
        } else {
            Logger.d(TAG, "mViewModel or mBinding is null");
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        PermissionUtils.getInstance().onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case PermissionUtils.REQUEST_PERMISSION_EXTERNAL_CODE:
                Logger.i(TAG, "所有文件修改权限申请结果");
                if (Environment.isExternalStorageManager()) {
                    PermissionUtils.getInstance().onRequestPermissionsResult(android.Manifest.permission.MANAGE_EXTERNAL_STORAGE, 0);
                } else {
                    PermissionUtils.getInstance().onRequestPermissionsResult(Manifest.permission.MANAGE_EXTERNAL_STORAGE, -1);
                }
                break;
            case PermissionUtils.REQUEST_PERMISSION_OVERLAY_CODE:
                Logger.i(TAG, "悬浮窗申请结果");
                if (Settings.canDrawOverlays(this)) {
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, 0);
                } else {
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, -1);
                }
                break;
        }
    }

    /**
     * 控制激活动画
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        if (show) {
            mBinding.activateBg.setVisibility(View.VISIBLE);
            mBinding.activatingImg.setVisibility(View.VISIBLE);
            mBinding.activatingTv.setVisibility(View.VISIBLE);
            mBinding.activatingImg.startAnimation(mRotateAnim);
        } else {
            mBinding.activateBg.setVisibility(View.GONE);
            mBinding.activatingImg.setVisibility(View.GONE);
            mBinding.activatingTv.setVisibility(View.GONE);
            if (mRotateAnim != null) {
                mRotateAnim.cancel();
                mRotateAnim.reset();
            }
        }
    }

    /**
     * 显示激活失败弹窗
     *
     * @param msg 错误信息
     */
    public void showActivateFailedDialog(final int errCode, final String msg) {
        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            Logger.e(TAG, "dialog showing");
            return;
        }
        mFailedDialog = new ActivateFailedDialog(this, new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.e(TAG, "重试激活");
                StartService.getInstance().startActivation();
            }

            @Override
            public void onCancelClick() {
                Logger.e(TAG, "激活失败,手动退出应用");
                ThreadManager.getInstance().asyncDelay(new Runnable() {
                    @Override
                    public void run() {
                        StackManager.getInstance().exitApp();
                    }
                }, 1, TimeUnit.MINUTES);
            }
        });
        mFailedDialog.changeDialogContent(errCode, msg);
        mFailedDialog.show();
    }

    /**
     * 关闭弹窗
     */
    public void dismissActivateFailedDialog() {
        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            mFailedDialog.dismiss();
            mFailedDialog.unInitContext();
            mFailedDialog = null;
        }
    }


    @Override
    protected void onMoveMapCenter() {
        Logger.i(TAG, "onMoveMapCenter");
        ThreadManager.getInstance().postUi(() -> {
            mBinding.searchMainTab.setVisibility(View.GONE);
            mViewModel.setMapCenterInScreen();
        });
    }

    @Override
    protected void onMoveMapCenter(final Bundle bundle) {
        Logger.i(TAG, "onMoveMapCenter with bundle");
        ThreadManager.getInstance().postUi(() -> {
            mBinding.searchMainTab.setVisibility(View.GONE);
            mViewModel.setMapCenterInScreen(bundle);
        });
    }

    @Override
    protected void onResetMapCenter() {
        Logger.i(TAG, "onResetMapCenter");
        ThreadManager.getInstance().postUi(() -> {
            mBinding.searchMainTab.setVisibility(View.VISIBLE);
            mViewModel.resetMapCenterInScreen();
        });
    }

    @Override
    protected void onResetMapTabFromDetail() {
        Logger.i(TAG, "onResetMapTabFromDetail");
        ThreadManager.getInstance().postUi(() -> {
            mBinding.searchMainTab.setVisibility(View.VISIBLE);
            mViewModel.resetVisibleInScreen();
        });
    }

    public boolean isFragmentStackNull() {
        return mStackManager.isFragmentStackNull(mScreenId);
    }

    @Override
    public void showParkingView() {
        mViewModel.showParkingView();
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (null == mViewModel || null == mBinding) {
            Logger.e(TAG, "error");
            return;
        }
        if (ConvertUtils.equals(ActivateUiStateManager.getInstance().getActivateState(), AutoMapConstant.ActivateState.ACTIVATING)
                || ConvertUtils.equals(ActivateUiStateManager.getInstance().getActivateState(), AutoMapConstant.ActivateState.ACTIVATE_FAILED)) {
            Logger.e(TAG, "onConfigurationChanged: 激活中package还没初始化");
            dismissActivateFailedDialog();
            int errCode = ActivateUiStateManager.getInstance().getErrorCodeSave();
            String msg = ActivateUiStateManager.getInstance().getErrorMsgSave();
            if (!ConvertUtils.equals(errCode, 0) && !ConvertUtils.isEmpty(msg)) {
                showActivateFailedDialog(errCode, msg);
            }
            mBinding.mainImg.setImageResource(R.drawable.logo_startup);
            return;
        }
        mViewModel.reminderDialogReCreate();
        mBinding.mainImg.setImageResource(R.drawable.logo_startup);
        if (mViewModel.isSupportSplitScreen()) {
            mViewModel.showStartIcon();
            ScreenTypeUtils.getInstance().setScreenType(newConfig);
            ThreadManager.getInstance().postDelay(() -> mViewModel.hideStartIcon(), 200);
        }
        //模式更改不重新触发trips
        mViewModel.mIsChangingConfigurations.set(true);
        int newUiMode = newConfig.uiMode;
        if (mCurrentUiMode != newUiMode) {
            Logger.d(TAG, "onConfigurationChanged", "update theme");
            mCurrentUiMode = newUiMode;
            mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                    ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
            recreate();
        }
    }

    // 更新当前的比例尺数值
    public void updateOnMapScaleChanged(String scale, int scaleLineVaule) {
        mBinding.sceneScaleView.updateOnMapLevelChanged(scale, scaleLineVaule);
    }

    public void setMessageImg(int res) {
        mBinding.includeMessageCenter.baseMapMessageCenterImg.setImageResource(res);
    }

    public IBaseScreenMapView getMapView() {
        return mBinding.mainMapview;
    }

    public MainScreenMapView getMainMapView() {
        return mBinding.mainMapview;
    }

    public void updateCruiseLanInfo(boolean isShowLane, LaneInfoEntity laneInfoEntity) {
        mBinding.cruiseLayout.cruiseLanesView.onLaneInfo(isShowLane, laneInfoEntity);
    }

    public void cruiseMuteOrUnMute(boolean isOpen) {
        mBinding.cruiseLayout.ivVoice.setSelected(isOpen);
        mBinding.cruiseLayout.tvTitle.setText(isOpen ? R.string.cruise_unmute : R.string.cruise_mute);
    }

    public void setTMCView(int key, List<RouteLightBarItem> routeLightBarItems) {
        ThreadManager.getInstance().postUi(() -> {
            if (key == 0) {
                mBinding.skIvBasicHomeProgress.refreshTMC(routeLightBarItems);
            } else {
                mBinding.skIvBasicBusProgress.refreshTMC(routeLightBarItems);
            }
        });
    }

    @Override
    protected void onFragmentSizeChanged() {
        super.onFragmentSizeChanged();
        mViewModel.stopCruise();
        setMapFocusable(false);
        FloatViewManager.getInstance().showAllCardWidgetsAfterFragmentSizeChanged();
    }

    public void setMapFocusable(boolean b) {
        if (ConvertUtils.isEmpty(mapView)) {
            mapView = (MainScreenMapView) getMapView();
        }
        mapView.setFocusable(b);
    }


    public void updateCruiseRoadName(CruiseInfoEntity cruiseInfoEntity) {
        if (ConvertUtils.isNull(cruiseInfoEntity) || ConvertUtils.isEmpty(cruiseInfoEntity.roadName)) {
            mBinding.cruiseLayout.tvCurrentRoadName.setVisibility(View.INVISIBLE);
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
                    mBinding.cruiseLayout.tvCurrentRoadName.setText(cruiseInfoEntity.roadName);
                    mBinding.cruiseLayout.tvCurrentRoadName.setVisibility(View.VISIBLE);
                }
        );
    }

    public void setNdGoHomeView(RouteTMCParam routeTMCParam) {
        mBinding.sceneGoHome.setNdGoHomeView(routeTMCParam);
        mBinding.sceneGoHome.setCallback(new ISceneCallback() {
            @Override
            public void clickGoHomeBtn(int type) {
                mViewModel.goHomeOrCompany(type);
            }
        });
    }

    public void showTripDialog(String title, String content) {
        if (!ConvertUtils.isEmpty(mMsgTopDialog) && mMsgTopDialog.isShowing()) {
            return;
        }
        mMsgTopDialog = new MsgTopDialog(
                StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()), TripID.ROUTE_LOW_BATTER,
                41, 1097);
        mMsgTopDialog.setTitle(title);
        mMsgTopDialog.setContent(content);
        mMsgTopDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick(final TripID tripID) {
                mMsgTopDialog.hide();
            }
        });
        mMsgTopDialog.showDialog();
    }

    /**
     * 监听触摸事件，当焦点不在exitText中时隐藏软键盘
     *
     * @param ev
     * @return
     */
    @Override
    public boolean dispatchTouchEvent(MotionEvent ev) {
        if (ev.getAction() == MotionEvent.ACTION_DOWN) {
            View view = getCurrentFocus();
            if (!ConvertUtils.isNull(view)
                    && !ConvertUtils.isNull(mViewModel)
                    && mViewModel.isShouldHideKeyboard(view, ev)) {
                mViewModel.hideKeyboard(view);
            }
        }
        return super.dispatchTouchEvent(ev);
    }

    public void notifyStepOneThirdScreen() {
        mViewModel.notifyStepOneThirdScreen();
    }

    public void closeSplitFragment() {
        final BaseFragment baseFragment = mStackManager.getCurrentFragment(mScreenId);
        if (baseFragment instanceof SplitFragment) {
            closeFragment(true);
        } else {
            Logger.d("screen_change_used", "不包含1/3屏幕布局");
        }
    }

    public void openGuideFragment() {
        Logger.i(TAG, "closeSplitFragment openGuideFragment");
        mViewModel.openGuideFragment();
    }

    public void updateUiOnHomeKeyClick() {
        mViewModel.updateUiOnHomeKeyClick();
    }

    private final Observable.OnPropertyChangedCallback propertyChangedCallback = new Observable.OnPropertyChangedCallback() {
        @Override
        public void onPropertyChanged(Observable sender, int propertyId) {
            boolean value = ((ObservableBoolean) sender).get();
            ConstraintLayout.LayoutParams layoutParams = (ConstraintLayout.LayoutParams) mBinding.includeMessageCenter.getRoot().getLayoutParams();
            if (!value) {
                layoutParams.startToEnd = mBinding.layoutFragment.getId();
                layoutParams.setMarginStart(ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_m_17));
            } else {
                layoutParams.startToEnd = mBinding.searchMainTab.getId();
                layoutParams.setMarginStart(ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_17));
            }
            mBinding.includeMessageCenter.getRoot().setLayoutParams(layoutParams);
        }
    };

    public void syncFragment() {
        FragmentIntent.syncFragmentList(mScreenId, getSupportFragmentManager());
    }

    public void callPageCode(int pageCode) {
        Logger.d(TAG, pageCode);
        if (mViewModel == null) {
            Logger.e(TAG, "callPageCode mViewModel is null");
            return;
        }
        switch (pageCode) {
            case INaviConstant.OpenIntentPage.SEARCH_PAGE:
                mViewModel.openSearchFragment.call();
                break;
            case INaviConstant.OpenIntentPage.GO_HOME:
                mViewModel.openHomeFragment.call();
                break;
            case INaviConstant.OpenIntentPage.GO_COMPANY:
                mViewModel.openCompanyFragment.call();
                break;
            case INaviConstant.OpenIntentPage.SEARCH_RESULT_PAGE:
                mViewModel.searchForChargeStation.call();
                break;
            default:
                break;
        }
    }

    public void hideOrShowFragmentContainer(boolean isShow) {
        mBinding.layoutFragment.setVisibility(isShow ? View.VISIBLE : View.INVISIBLE);
    }
}