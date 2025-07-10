package com.sgm.navi.hmi.map;

import android.Manifest;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;
import android.os.Environment;
import android.provider.Settings;
import android.util.DisplayMetrics;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.WindowCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.SpUtils;
import com.android.utils.ThemeUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityMapBinding;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.launcher.LauncherWindowService;
import com.sgm.navi.hmi.permission.PermissionUtils;
import com.sgm.navi.hmi.splitscreen.SplitFragment;
import com.sgm.navi.hmi.startup.ActivateFailedDialog;
import com.sgm.navi.service.AutoMapConstant;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.screen.ScreenType;
import com.sgm.navi.service.define.screen.ScreenTypeUtils;
import com.sgm.navi.scene.dialog.MsgTopDialog;
import com.sgm.navi.scene.impl.navi.inter.ISceneCallback;
import com.sgm.navi.service.define.cruise.CruiseInfoEntity;
import com.sgm.navi.service.define.map.IBaseScreenMapView;
import com.sgm.navi.service.define.map.MainScreenMapView;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.map.ThemeType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.route.RouteLightBarItem;
import com.sgm.navi.service.define.route.RouteTMCParam;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.service.logicpaket.map.MapPackage;
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

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/01
 */
public class MapActivity extends BaseActivity<ActivityMapBinding, MapViewModel> {

    private static final String TAG = "MapActivity";
    private static final String KEY_CHANGE_SAVE_INSTANCE = "key_change_save_instance";

    private Animation mRotateAnim;
    private ActivateFailedDialog mFailedDialog;

    private MainScreenMapView mapView;
    private MsgTopDialog mMsgTopDialog;
    private Runnable mOpenGuideRunnable;

    private Runnable timerRunnable = null;

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN)
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
        mBinding.mainImg.post(() -> {
            if (mViewModel.isSupportSplitScreen()) {
                checkConfig();
            }
        });
    }

    /**
     * 桌面地图不同意协议后处理
     *
     * @param situation 1是高德协议， 2是定位协议
     */
    public void protectMap(final int situation) {
        switch (situation) {
            case AutoMapConstant.CANCEL_AUTO_PROTOCOL:
                Logger.d(TAG, "protectMap: 高德协议");
                mBinding.protectView.setOnClickListener(v -> {
                    mViewModel.checkPrivacyRights();
                    mBinding.protectView.setOnClickListener(null);
                });
                break;
            case AutoMapConstant.CANCEL_LOCATION_PROTOCOL:
                Logger.d(TAG, "protectMap: 定位协议");
                mBinding.protectView.setOnClickListener(v -> {
                    mViewModel.checkAuthorizationExpired();
                    mBinding.protectView.setOnClickListener(null);
                });
                break;
            default:
                Logger.e(TAG, "protectMap: situation is not supported");
                break;
        }
    }

    public void closeProtectView() {
        mBinding.protectView.setVisibility(View.GONE);
    }

    public void showProtectView() {
        mBinding.protectView.setVisibility(View.VISIBLE);
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
        if(!StartService.getInstance().checkSdkIsNeedInit()){
            mViewModel.loadMapView(mBinding.mainMapview);
        }
    }

    @Override
    public void onInitObserver() {

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
    protected void onResume() {
        super.onResume();
        checkScreen();
        if (mViewModel.getSdkInitStatus()) {
            mViewModel.getCurrentCityLimit();
            //界面可见时重新适配深浅色模式
            mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                    ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
        }
        FragmentIntent.syncFragmentList(mScreenId, getSupportFragmentManager());
    }

    private void checkScreen() {
        ScreenTypeUtils.getInstance().isSameScreenType(getResources().getDisplayMetrics());
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_HIDE)
    protected void onStop() {
        Logger.i(TAG, "onStop");
        super.onStop();
    }

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_CLOSE)
    protected void onDestroy() {
        stopTime();
        // 退出的时候主动保存一下最后的定位信息
        if (mViewModel.getSdkInitStatus()) {
            mViewModel.saveLastLocationInfo();
            ThreadManager.getInstance().removeHandleTask(mOpenGuideRunnable);
        }

        if(mMsgTopDialog != null && mMsgTopDialog.isShowing()){
            mMsgTopDialog.dismiss();
        }
        mMsgTopDialog = null;

        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            mFailedDialog.dismiss();
        }
        mFailedDialog = null;
        Logger.i(TAG, "onDestroy");
        super.onDestroy();
    }

    public void doAfterInitSdk() {
        //initData
        mViewModel.loadMapView(mBinding.mainMapview);
        // 给限行设置点击事件
        mBinding.includeLimit.setViewModel(mViewModel);
        mBinding.cruiseLayout.setViewModel(mViewModel);
        final int powerType = mViewModel.powerType();
        // 油车
        if (powerType != 1) {
            mBinding.skIvBasicRouting.setImageResource(R.drawable.img_home_gas_station);
        } else {
            mBinding.skIvBasicRouting.setImageResource(R.drawable.img_basic_ic_gas_charging);
        }
        mBinding.includeMessageCenter.setViewModel(mViewModel);

        //initObserver
        addSceneGoHomeCallBack();

        //initData
        mViewModel.startListenMsg();
        mViewModel.offlineMap15Day();
        mViewModel.offlineMap45Day();
        if (!mStackManager.isExistFragment(mScreenId, "AccountQRCodeLoginFragment")) {
            mViewModel.checkPopGuideLogin();
        }
        if(Boolean.FALSE.equals(mViewModel.mIsChangingConfigurations.get())) mViewModel.getOnlineForecastArrivedData();
        mOpenGuideRunnable = () -> mViewModel.openGuideFragment();
        ThreadManager.getInstance().postDelay(mOpenGuideRunnable, NumberUtils.NUM_500);
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
            mBinding.mainImg.setVisibility(View.GONE);
            mBinding.activatingImg.setVisibility(View.VISIBLE);
            mBinding.activatingTv.setVisibility(View.VISIBLE);
            mBinding.activatingImg.startAnimation(mRotateAnim);
        } else {
            mBinding.mainImg.setVisibility(View.VISIBLE);
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
    public void showActivateFailedDialog(final String msg) {
        if (mFailedDialog != null && mFailedDialog.isShowing()) {
            Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "dialog showing");
            return;
        }
        mFailedDialog = new ActivateFailedDialog(this);
        mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "重试激活");
            }

            @Override
            public void onCancelClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "激活失败,手动退出应用");
            }
        });
        mFailedDialog.show();
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

    public boolean isFragmentStackNull(){
        return mStackManager.isFragmentStackNull(mScreenId);
    }

    @Override
    public void showParkingView() {
        mViewModel.showParkingView();
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (null != mViewModel) {
            mViewModel.reminderDialogReCreate();
            mBinding.mainImg.setImageResource(R.drawable.logo_startup);
        }
        if (null == mViewModel || !mViewModel.getSdkInitStatus()) {
            return;
        }
        mViewModel.mIsChangingConfigurations.set(true);
        if (mViewModel.isSupportSplitScreen()) {
            Logger.d("screen_change_used", newConfig.screenWidthDp);
            ScreenTypeUtils.getInstance().setScreenType(newConfig);
            mViewModel.notifyScreenSizeChanged();
            mViewModel.onNaviStatusChange();
            mViewModel.checkStatusCloseAllFragmentAndClearAllLabel();
            setSplitFragment();
            mViewModel.toSetCarPosition();
        }
        mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
        recreate();
    }

    private void setSplitFragment() {
        if (ScreenTypeUtils.getInstance().isOneThirdScreen()) {
            Logger.d("screen_change_used", "打开1/3屏幕布局");
            addFragment(SplitFragment.getInstance(), null);
        } else {
            final BaseFragment baseFragment = mStackManager.getCurrentFragment(mScreenId);
            if (baseFragment instanceof SplitFragment) {
                Logger.d("screen_change_used", "关闭1/3屏幕布局");
                closeFragment(true);
            } else {
                Logger.d("screen_change_used", "不包含1/3屏幕布局");
            }
        }
    }

    // 更新当前的比例尺数值
    public void updateOnMapScaleChanged(String scale) {
        mBinding.sceneScaleView.updateOnMapLevelChanged(scale);
    }

    public void setMessageImg(int res) {
        mBinding.includeMessageCenter.baseMapMessageCenterImg.setImageResource(res);
    }

    public IBaseScreenMapView getMapView() {
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
    }

    private void addSceneGoHomeCallBack() {
        mBinding.sceneGoHome.setCallback(new ISceneCallback() {
            @Override
            public void clickGoHomeBtn(int type) {
                mViewModel.addSceneGoHomeCallBack(type);
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

    private void checkConfig() {
        final ScreenType currentScreenType = ScreenTypeUtils.getInstance().calculateScreenType(getResources().getConfiguration());
        if (currentScreenType != ScreenTypeUtils.getInstance().getScreenType() && mViewModel.isSupportSplitScreen()) {
            ScreenTypeUtils.getInstance().setScreenType(getResources().getConfiguration());
            mViewModel.notifyScreenSizeChanged();
            mViewModel.onNaviStatusChange();
            mViewModel.checkStatusCloseAllFragmentAndClearAllLabel();
            Logger.d(TAG, "checkConfig and need update!");
        }
    }

    public void notifyStepOneThirdScreen() {
        mViewModel.notifyStepOneThirdScreen();
    }
}