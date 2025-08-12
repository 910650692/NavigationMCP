package com.sgm.navi.hmi.launcher;

import static android.content.Context.WINDOW_SERVICE;


import android.content.ComponentCallbacks;
import android.content.ContentResolver;
import android.content.Context;
import android.content.IntentFilter;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;
import android.os.Build;
import android.provider.Settings;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.navi.NaviGuidanceModel;
import com.sgm.navi.service.utils.ExportIntentParam;
import com.sgm.navi.hmi.BuildConfig;
import com.sgm.navi.hmi.databinding.FloatingWindowLayoutBinding;
import com.sgm.navi.hmi.utils.CaptureScreenUtils;
import com.sgm.navi.mapservice.bean.INaviConstant;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.adapter.layer.LayerAdapter;
import com.sgm.navi.service.define.map.MapScreenShotDataInfo;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.define.navi.LaneInfoEntity;
import com.sgm.navi.service.define.navi.NaviEtaInfo;
import com.sgm.navi.service.define.navi.NaviTmcInfo;
import com.sgm.navi.service.define.navistatus.NaviStatus;
import com.sgm.navi.service.logicpaket.layer.ILayerPackageCallBack;
import com.sgm.navi.service.logicpaket.layer.LayerPackage;
import com.sgm.navi.service.logicpaket.map.IEglScreenshotCallBack;
import com.sgm.navi.service.logicpaket.map.IMapPackageCallback;
import com.sgm.navi.service.logicpaket.map.MapPackage;
import com.sgm.navi.service.logicpaket.navi.IGuidanceObserver;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusCallback;
import com.sgm.navi.service.logicpaket.navistatus.NaviStatusPackage;
import com.sgm.navi.service.logicpaket.route.RoutePackage;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.vrbridge.MapStateManager;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/24
 * Description: [在这里描述文件功能]
 */
public class LauncherWindowService implements IGuidanceObserver, IMapPackageCallback, IEglScreenshotCallBack, CaptureScreenUtils.CaptureScreenCallBack, ComponentCallbacks,
        NaviStatusCallback, ILayerPackageCallBack, FloatWindowReceiver.FloatWindowCallback, NetWorkUtils.NetworkObserver {
    private static final String TAG = "LauncherWindowService";
    private WindowManager mWindowManager;
    private View mView;
    private FloatingWindowLayoutBinding mBinding;

    private FloatWindowReceiver floatWindowReceiver;
    private NaviEtaInfo mNaviEtaInfo;
    private NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private NaviStatusPackage mNaviStatusPackage;
    private RoutePackage mRoutePackage;
    private LaneInfoEntity mLastLanInfo;
    private boolean mIsShowLane = false;
    private LayerAdapter mLayerAdapter;
    private final String KEY = "LauncherWindowService";
    private final MapType MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;
    private FloatViewManager mFloatManager;
    private boolean mIsOnShowing = false;
    private int currentUiMode = Configuration.UI_MODE_NIGHT_YES;
    private boolean isInited = false;
    private boolean mCrossImgIsOnShowing = false;
    private CaptureScreenUtils captureScreenUtils;
    private String currentNaviStatus;
    private final int DisplayId = 0;
    private LayerPackage mLayerPackage;
    private boolean isConnected;
    private static boolean isInitialized = false;

    private LauncherWindowService() {

    }

    private void init() {
        Logger.i(TAG, "init:" + isInited);
        if (!isInited) {
            initParameters();
            initCallBacks();
            initView();
            initAndGetMusicTabStatus();
            isInited = true;
        }
    }

    public void unInit() {
        unInitCallBacks();
        unregisterFloatWindowReceiver();
        FloatWindowReceiver.unregisterCallback(TAG);
    }

    private void initParameters() {
        mLayerAdapter = LayerAdapter.getInstance();
        mMapPackage = MapPackage.getInstance();
        mNaviPackage = NaviPackage.getInstance();
        mNaviStatusPackage = NaviStatusPackage.getInstance();
        mWindowManager = (WindowManager) AppCache.getInstance().getMContext().getSystemService(WINDOW_SERVICE);
        mFloatManager = FloatViewManager.getInstance();
        currentUiMode = AppCache.getInstance().getMContext().getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        mNaviEtaInfo = mNaviPackage.getCurrentNaviEtaInfo();
        captureScreenUtils = CaptureScreenUtils.getInstance();
        currentNaviStatus = mNaviStatusPackage.getCurrentNaviStatus();
        mRoutePackage = RoutePackage.getInstance();
        mLayerPackage = LayerPackage.getInstance();
        isConnected = NetWorkUtils.Companion.getInstance().checkNetwork();
    }

    private void initCallBacks() {
        mNaviPackage.registerObserver(KEY, this);
        mMapPackage.registerCallback(MAP_TYPE, this);
//        mMapPackage.registerEGLScreenshotCallBack(KEY, this);
        mFloatManager.bindLauncherService();
        AppCache.getInstance().getMContext().registerComponentCallbacks(this);
        captureScreenUtils.registerListener(this);
        mNaviStatusPackage.registerObserver(KEY, this);
        mLayerPackage.registerCallBack(MAP_TYPE, this);
        //注册网络监听
        NetWorkUtils.Companion.getInstance().registerNetworkObserver(this);
    }

    private void unInitCallBacks() {
        Logger.i(TAG, "unInitCallBacks");
        AppCache.getInstance().getMContext().unregisterComponentCallbacks(this);
        mNaviPackage.unregisterObserver(KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
//        mMapPackage.unregisterEGLScreenshotCallBack(KEY, this);
        mFloatManager.unBindLauncherService();
        captureScreenUtils.unRegisterListener(this);
        mNaviStatusPackage.unregisterObserver(KEY);
        mLayerPackage.unRegisterCallBack(MAP_TYPE, this);
        //注销网络监听
        NetWorkUtils.Companion.getInstance().unRegisterNetworkObserver(this);
    }

    private void unregisterFloatWindowReceiver() {
        if (floatWindowReceiver != null) {
            try {
                AppCache.getInstance().getMContext().unregisterReceiver(floatWindowReceiver);
                Logger.d(TAG, "注销FloatWindowReceiver成功");
            } catch (IllegalArgumentException e) {
                Logger.e(TAG, "注销接收器失败：" + e.getMessage());
            } finally {
                floatWindowReceiver = null;
            }
        }
    }

    private void initClickListener() {
        if (ConvertUtils.isNull(mBinding)) return;
        mBinding.skIvBasicBus.setOnClickListener(v -> {
            openSelf(INaviConstant.OpenIntentPage.GO_COMPANY);
        });
        mBinding.skIvBasicHome.setOnClickListener(v -> {
            openSelf(INaviConstant.OpenIntentPage.GO_HOME);
        });
        mBinding.skIvBasicSearch.setOnClickListener(v -> {
            openSelf(INaviConstant.OpenIntentPage.SEARCH_PAGE);
        });
        mBinding.cardTbtView.setOnClickListener(v -> {
            openSelf(INaviConstant.OpenIntentPage.NONE);
        });
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        IGuidanceObserver.super.onNaviInfo(naviInfoBean);
        updateTbT(naviInfoBean);
    }

    @Override
    public void onEGLScreenshot(MapType mapType, byte[] bytes, MapScreenShotDataInfo info) {
        if (mapType == MAP_TYPE && !ConvertUtils.isNull(mView) && !ConvertUtils.isEmpty(bytes) && mCrossImgIsOnShowing && mNaviPackage.isHasSetCrossRect()) {
            captureScreenUtils.processPicture(bytes, info);
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        updateLanInfo(isShowLane, laneInfo);
    }

    private NaviTmcInfo mLastTmcInfo;
    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        Logger.d(TAG,"onUpdateTMCLightBar");
        mLastTmcInfo = naviTmcInfo;
        if (!ConvertUtils.isNull(mBinding)) {
            mBinding.sceneNaviTmc.setIsShowAutoAdd(false);
            mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
        }
    }

    @SuppressWarnings("FORWARD_NULL")
    private void initView() {
        Logger.i(TAG, "initView");
        if (!checkHasOverLay()) {
            Logger.e(TAG, "悬浮窗权限未开启，请开启悬浮窗权限！");
            return;
        }
        if (mView != null && mWindowManager != null) {
            mWindowManager.removeView(mView);
            mBinding = null;
            mView = null;
            Logger.i(TAG, "remove all view!");
        }
        if (!mIsOnShowing) {
            Logger.i(TAG, "不需要显示，View 暂时不创建！");
            return;
        }
        final Context context = ScreenUtils.Companion.getInstance().getTargetDisplayContext(
                AppCache.getInstance().getMContext(),
                DisplayId
        );
        mBinding = FloatingWindowLayoutBinding.inflate(LayoutInflater.from(context), null);
        mView = mBinding.getRoot();

        if (FloatWindowReceiver.isShowMusicTab){
            mBinding.floatWindow.setVisibility(View.INVISIBLE);
        } else {
            mBinding.floatWindow.setVisibility(View.GONE);
        }

        final WindowManager.LayoutParams layoutParams = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                Build.VERSION.SDK_INT >= Build.VERSION_CODES.O ?
                        WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY :
                        WindowManager.LayoutParams.TYPE_PHONE,
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                PixelFormat.TRANSLUCENT
        );

        layoutParams.gravity = Gravity.LEFT | Gravity.TOP;
        int top = (int) context.getResources().getDimension(com.sgm.navi.ui.R.dimen.launcher_position_top);
        int left = (int) context.getResources().getDimension(com.sgm.navi.ui.R.dimen.launcher_position_left);
        Logger.i(TAG, "LauncherWindowPosition: " + top + "; " + left);
        layoutParams.x = left;
        layoutParams.y = top;
        mWindowManager.addView(mView, layoutParams);
        initClickListener();
        onNetStatusChange(isConnected);
        changeUiTypeOnNaviStatusChanged();
        updateLanInfo(mIsShowLane, mLastLanInfo);
        updateTbT(mNaviEtaInfo);
    }

    private void registerFloatWindowReceiver() {
        if (floatWindowReceiver == null) {
            floatWindowReceiver = new FloatWindowReceiver();
            IntentFilter filter = new IntentFilter();
            filter.addAction("patac.hmi.intent.action.FLOAT_WINDOW_SIDE"); // 匹配广播Action

            String permission = "com.patac.hmi.media.floatwindow.PERMISSION";

            // 根据Android版本选择注册方式，解决SecurityException
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
                // 注意：如果广播来自其他应用，需用RECEIVER_EXPORTED
                AppCache.getInstance().getMContext().registerReceiver(
                        floatWindowReceiver,
                        filter,
                        permission,
                        null,
                        Context.RECEIVER_EXPORTED // 允许接收外部应用广播
                );
            } else {
                // 低版本直接注册
                AppCache.getInstance().getMContext().registerReceiver(
                        floatWindowReceiver,
                        filter,
                        permission,
                        null
                );
            }
            Logger.d(TAG, "动态注册FloatWindowReceiver成功");
        }
    }

    private void updateTbT(NaviEtaInfo etaInfo) {
        Logger.i(TAG, "updateTbT");
        ThreadManager.getInstance().postUi(() -> {
            this.mNaviEtaInfo = etaInfo;
            if (!ConvertUtils.isNull(etaInfo) && !ConvertUtils.isNull(mBinding)) {
                mBinding.sceneNaviTbt.onNaviInfo(mNaviEtaInfo);
                mBinding.sceneNaviEta.onNaviInfo(mNaviEtaInfo);
                mBinding.sceneNaviTmc.onNaviInfo(mNaviEtaInfo);
            }
        });
    }

    /***
     * 启动Navi_App
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_ENTERAPP)
    public void openSelf(final int pageCode) {
        Logger.i(TAG, "openSelf:" + pageCode);
        // 非导航状态打开对应窗口时候先关闭所有Fragment
        if (!isOnNavigating()) {
            BaseActivity baseActivity = StackManager.getInstance().getCurrentActivity(MAP_TYPE.name());
            if (mRoutePackage.isRouteState()) {
                mRoutePackage.clearRouteLine(MAP_TYPE);
            }
            if (!ConvertUtils.isNull(baseActivity)) {
                baseActivity.closeAllFragment();
            }
        }
        ExportIntentParam.setIntentPage(pageCode);
        ProcessManager.restartProcess(AppCache.getInstance().getMContext(), mFloatManager.isNaviDeskBg());
    }

    public static void startService() {
        if (TextUtils.equals("buick", BuildConfig.FLAVOR)) return;
        Logger.i(TAG, "start service success!");
        if (!isInitialized) {
            isInitialized = true;
            InstanceHolder.instance.init();
        }
    }

    public static boolean isServiceInitialized() {
        return isInitialized;
    }

    private void initAndGetMusicTabStatus() {
        try {
            ContentResolver contentResolver = AppCache.getInstance().getMContext().getContentResolver();
            int storedValue = Settings.System.getInt(
                    contentResolver,
                    "com.patac.hmi.media.float.position"
            );
            Logger.d(TAG, "读取到的悬浮窗位置值: " + storedValue);
            FloatWindowReceiver.isShowMusicTab = storedValue == 2;
        } catch (Settings.SettingNotFoundException e) {
            Logger.e(TAG, "未找到设置项: com.patac.hmi.media.float.position");
        }

        // 注册媒体悬浮窗广播
        registerFloatWindowReceiver();
        FloatWindowReceiver.registerCallback(TAG, this);
    }

    public static void stopService() {
        Logger.i(TAG, "stop service success！");
        InstanceHolder.instance.unInit();
    }

    public void showOrHideFloatView(boolean isShow) {
        Logger.i(TAG, "showOrHideFloatView:" + isShow);
        ThreadManager.getInstance().postUi(() -> {
            mIsOnShowing = isShow;
            if (!isInited) {
                startService();
            } else if (!ConvertUtils.isNull(mView)) {
                mView.setVisibility(isShow ? View.VISIBLE : View.INVISIBLE);
                mView.setFocusable(isShow);
                if (isShow) {
                    changeUiTypeOnNaviStatusChanged();
                } else {
                    MapStateManager.getInstance().vrSendLauncherShow(false);
                }
            } else {
                initView();
            }
        });
    }

    /***
     * 检查悬浮窗权限释放开启
     * @return
     */
    private boolean checkHasOverLay() {
        return Settings.canDrawOverlays(AppCache.getInstance().getMContext());
    }

    public void changeCrossVisible(boolean isVisible) {
        Logger.i(TAG, "changeCrossVisible:" + isVisible);
        mCrossImgIsOnShowing = isVisible;
        if (!ConvertUtils.isNull(mBinding)) {
            ThreadManager.getInstance().postUi(() -> {
                if (!isVisible) {
                    mBinding.ivCross.setVisibility(View.GONE);
                }

                // 大图显示后隐藏车道线和光柱图
                if (isVisible && mBinding.sceneNaviLanes != null && mBinding.sceneNaviTmc != null) {
                    mBinding.sceneNaviLanes.setVisibility(isVisible ? View.GONE : View.VISIBLE);
                    mBinding.sceneNaviTmc.setVisibility(isVisible ? View.GONE : View.VISIBLE);
                }
            });
        }
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        Logger.i(TAG, "onConfigurationChanged");
        // 检查深色模式变更
        int uiMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
        if (uiMode != currentUiMode && !ConvertUtils.isNull(mBinding)) {
            Logger.i(TAG, "刷新UI");
            initView();
        }
        currentUiMode = uiMode;
    }

    @Override
    public void onLowMemory() {
        Logger.i(TAG, "onLowMemory");
    }

    @Override
    public void onImageProcessCompleted(@Nullable Bitmap bitmap) {
        if (ConvertUtils.isNull(mBinding)) return;
        if (!ConvertUtils.isNull(bitmap)) {
            mBinding.ivCross.setImageBitmap(bitmap);
            mBinding.ivCross.setVisibility(mCrossImgIsOnShowing ? View.VISIBLE : View.GONE);
        } else {
            mBinding.ivCross.setVisibility(View.GONE);
        }
    }

    @Override
    public void onNaviStatusChange(String naviStatus) {
        Logger.i(TAG, "onNaviStatusChange", naviStatus);
        this.currentNaviStatus = naviStatus;
        changeUiTypeOnNaviStatusChanged();
    }

    @Override
    public void onCrossImageVisibleChanged(MapType mapTypeId, boolean visible) {
        ILayerPackageCallBack.super.onCrossImageVisibleChanged(mapTypeId, visible);
        this.mCrossImgIsOnShowing = visible;
        Logger.d(TAG, "onCrossImageVisibleChanged", visible);
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding) && !visible) {
                mBinding.ivCross.setVisibility(View.GONE);
            }
        });
    }

    private void changeUiTypeOnNaviStatusChanged() {
        if (ConvertUtils.isNull(mBinding) || ConvertUtils.isNull(mView)) {
            Logger.i(TAG, "changeUiTypeOnNaviStatusChanged mBinding || mView  is null");
            return;
        }
        final boolean isNavigating = isOnNavigating();
        MapStateManager.getInstance().vrSendLauncherShow(isNavigating);
        ThreadManager.getInstance().postUi(() -> {
            mBinding.cardTbtView.setVisibility(isNavigating ? View.VISIBLE : View.GONE);
            mBinding.cardNaviView.setVisibility((isNavigating || FloatViewManager.getInstance().isBiZhiDeskBg()) ? View.GONE : View.VISIBLE);
            if (isNavigating && mLastTmcInfo != null) {
                mBinding.sceneNaviTmc.setIsShowAutoAdd(false);
                mBinding.sceneNaviTmc.onUpdateTMCLightBar(mLastTmcInfo);
                Logger.d(TAG,"重新绘制光柱图最新数据");
            }
            // 3. 新增逻辑：如果路口大图正在显示，强制隐藏车道线（首次进入时也生效）
            mBinding.sceneNaviLanes.setVisibility(mCrossImgIsOnShowing ? View.GONE : View.VISIBLE);
            mBinding.sceneNaviTmc.setVisibility(mCrossImgIsOnShowing ? View.GONE : View.VISIBLE);
        });
    }

    private void updateLanInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        this.mLastLanInfo = laneInfo;
        this.mIsShowLane = isShowLane;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding) && !ConvertUtils.isNull(mView)) {
                if (mCrossImgIsOnShowing) {
                    mBinding.sceneNaviLanes.setVisibility(View.GONE);
                }else {
                    mBinding.sceneNaviLanes.setVisibility(isShowLane ? View.VISIBLE : View.GONE);
                }
                mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
            }
        });
    }

    private boolean isOnNavigating() {
        final boolean isOnNavigating = ConvertUtils.equals(currentNaviStatus, NaviStatus.NaviStatusType.NAVING);
        Logger.i(TAG, "isOnNavigating", isOnNavigating);
        return isOnNavigating;
    }

    public static LauncherWindowService getInstance() {
        return InstanceHolder.instance;
    }

    @Override
    public void onWindowSideChanged(boolean isOpenFloat) {
        if (mBinding == null){
            return;
        }
        if (isOpenFloat){
            mBinding.floatWindow.setVisibility(View.INVISIBLE);
        } else {
            mBinding.floatWindow.setVisibility(View.GONE);
        }
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        if (mFloatManager != null) {
            mFloatManager.onNaviStop();
        }
    }

    public void onNetStatusChange(boolean isConnect) {
        // 离线隐藏Tbt面板
        Logger.d(TAG, "离线隐藏tmc光柱图，在线显示 isConnected:", isConnect);
        isConnected = isConnect;
        if (mBinding == null) {
            Logger.w(TAG, "onNetStatusChange: mBinding is null, ignoring update");
            return;
        }
        ThreadManager.getInstance().postUi(() -> {
            mBinding.sceneNaviTmc.setVisibility(isConnected ? View.VISIBLE : View.GONE);
        });
    }

    @Override
    public void onNetConnectSuccess() {
        isConnected = true;
        Logger.d(TAG, "onNetConnectSuccess: isConnected=" + isConnected);
        onNetStatusChange(isConnected);
    }

    @Override
    public void onNetDisConnect() {
        isConnected = false;
        Logger.d(TAG, "onNetDisConnect: isConnected=" + isConnected);
        onNetStatusChange(isConnected);
    }

    @Override
    public void onNetUnavailable() {
    }

    @Override
    public void onNetBlockedStatusChanged() {
    }

    @Override
    public void onNetLosing() {
    }

    @Override
    public void onNetLinkPropertiesChanged() {
    }

    @Override
    public void onNetValidated() {

    }

    private static final class InstanceHolder {
        private static final LauncherWindowService instance = new LauncherWindowService();
    }
}
