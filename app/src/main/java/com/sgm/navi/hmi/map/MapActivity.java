package com.sgm.navi.hmi.map;

import android.content.res.Configuration;
import android.os.Bundle;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.WindowCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
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
import com.sgm.navi.hmi.splitscreen.SplitScreenManager;
import com.sgm.navi.hmi.startup.StartupActivity;
import com.sgm.navi.hmi.utils.ScreenTypeUtils;
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
import com.sgm.navi.service.define.screen.ScreenType;
import com.sgm.navi.service.define.utils.NumberUtils;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.FragmentIntent;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.define.TripID;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;
import com.sgm.navi.ui.view.SkinLinearLayout;

import java.util.List;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/01
 */
public class MapActivity extends BaseActivity<ActivityMapBinding, MapViewModel> {
    private static final String TAG = "MapActivity";
    private static final String KEY_CHANGE_SAVE_INSTANCE = "key_change_save_instance";
    private MainScreenMapView mapView;
    private MsgTopDialog mMsgTopDialog;
    private Runnable mOpenGuideRunnable;

    @Override
    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN)
    public void onCreateBefore() {
        mScreenId = MapType.MAIN_SCREEN_MAIN_MAP.name();
    }

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);
        getWindow().setNavigationBarColor(getResources().getColor(R.color.route_charge_param_color));
        FragmentIntent.syncFragmentList(mScreenId, getSupportFragmentManager());
        mBinding.cruiseLayout.tvCurrentRoadName.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(final View v) {

            }
        });
        mBinding.main.post(() -> {
            SplitScreenManager.getInstance().onConfigurationChanged();
        });
    }

    private void delayRemoveLauncherActivity() {
        final StartupActivity startupActivity = (StartupActivity) StackManager.getInstance().getActivityByClsName(mScreenId, StartupActivity.class);
        if (!ConvertUtils.isNull(startupActivity)) {
            startupActivity.finishNoPop();
            StackManager.getInstance().removeBaseView(mScreenId, startupActivity);
            Logger.i(TAG, "delayRemoveLauncherActivity");
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
    }

    @Override
    public void onInitObserver() {
        addSceneGoHomeCallBack();
    }

    @Override
    public void onInitData() {
        mViewModel.startListenMsg();
        mViewModel.offlineMap15Day();
        mViewModel.offlineMap45Day();
        mViewModel.checkPopGuideLogin();
        mViewModel.getOnlineForecastArrivedData();
        mOpenGuideRunnable = new Runnable() {
            @Override
            public void run() {
                mViewModel.openGuideFragment();
            }
        };
        ThreadManager.getInstance().postDelay(mOpenGuideRunnable, NumberUtils.NUM_500);
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
        mViewModel.getCurrentCityLimit();
        delayRemoveLauncherActivity();
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
        // 退出的时候主动保存一下最后的定位信息
        mViewModel.saveLastLocationInfo();
        Logger.i(TAG, "onDestroy");
        ThreadManager.getInstance().removeHandleTask(mOpenGuideRunnable);
        if(mMsgTopDialog != null && mMsgTopDialog.isShowing()){
            mMsgTopDialog.dismiss();
            mMsgTopDialog = null;
        }
        super.onDestroy();
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
        if (Logger.openLog) {
            Logger.i(TAG, GsonUtils.toJson(newConfig));
        }
        mViewModel.updateUiStyle(MapType.MAIN_SCREEN_MAIN_MAP,
                ThemeUtils.INSTANCE.isNightModeEnabled(this) ? ThemeType.NIGHT : ThemeType.DAY);
        recreate();
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
        FloatViewManager.getInstance().hideAllCardWidgets(true);
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

    public void setSelfParkingViewMarginStart(){
        if(!mViewModel.showNdGoHomeView()) return;
        SkinLinearLayout layout = mBinding.parkingMainTabBottom;
        ViewGroup.MarginLayoutParams params = (ViewGroup.MarginLayoutParams) layout.getLayoutParams();
        params.setMarginStart(ScreenTypeUtils.getScreenType() == ScreenType.SCREEN_FULL ? ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_1143) : ResourceUtils.Companion.getInstance().getDimensionPixelSize(com.sgm.navi.ui.R.dimen.dp_758));
        layout.setLayoutParams(params);
    }
}