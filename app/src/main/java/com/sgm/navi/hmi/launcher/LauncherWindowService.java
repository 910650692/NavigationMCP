package com.sgm.navi.hmi.launcher;

import static android.content.Context.WINDOW_SERVICE;


import android.content.ComponentCallbacks;
import android.content.Context;
import android.content.res.Configuration;
import android.graphics.Bitmap;
import android.graphics.PixelFormat;
import android.os.Build;
import android.provider.Settings;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.WindowManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.screen.ScreenUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.broadcast.FloatWindowReceiver;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.exportservice.ExportIntentParam;
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
        NaviStatusCallback, ILayerPackageCallBack, FloatWindowReceiver.FloatWindowCallback {
    private static final String TAG = "LauncherWindowService";
    private WindowManager mWindowManager;
    private View mView;
    private FloatingWindowLayoutBinding mBinding;
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

    private LauncherWindowService() {

    }

    private void init() {
        Logger.i(TAG, "init:" + isInited);
        if (!isInited) {
            initParameters();
            initCallBacks();
            initView();
            isInited = true;
        }
    }

    public void unInit() {
        unInitCallBacks();
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
        FloatWindowReceiver.unregisterCallback(TAG);
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
        if (mapType == MAP_TYPE && !ConvertUtils.isNull(mView) && !ConvertUtils.isEmpty(bytes) && mCrossImgIsOnShowing) {
            captureScreenUtils.processPicture(bytes, info);
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        updateLanInfo(isShowLane, laneInfo);
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding)) {
                mBinding.sceneNaviTmc.setIsShowAutoAdd(false);
                mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
            }
        });
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
        if (FloatWindowReceiver.isLauncherStatus){
            ViewGroup.LayoutParams floatParams = mBinding.floatWindow.getLayoutParams();
            floatParams.width = FloatWindowReceiver.musicWindowWidth + 36;
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
        changeUiTypeOnNaviStatusChanged();
        updateLanInfo(mIsShowLane, mLastLanInfo);
        updateTbT(mNaviEtaInfo);
        FloatWindowReceiver.registerCallback(TAG, this);
    }

    private void updateTbT(NaviEtaInfo etaInfo) {
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
        AppCache.getInstance().openMap(mFloatManager.isNaviDeskBg());
    }

    public static void startService() {
        if (TextUtils.equals("buick", BuildConfig.FLAVOR)) return;
        Logger.i(TAG, "start service success!");
        InstanceHolder.instance.init();
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
        if (!isVisible && !ConvertUtils.isNull(mBinding)) {
            ThreadManager.getInstance().postUi(() -> {
                mBinding.ivCross.setVisibility(View.GONE);
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
        });
    }

    private void updateLanInfo(boolean isShowLane, LaneInfoEntity laneInfo) {
        this.mLastLanInfo = laneInfo;
        this.mIsShowLane = isShowLane;
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding) && !ConvertUtils.isNull(mView)) {
                mBinding.sceneNaviLanes.setVisibility(isShowLane ? View.VISIBLE : View.GONE);
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
    public void onWindowSideChanged(boolean isOpenFloat, int windowWidth) {
        Logger.d(TAG, "悬浮窗开关：" + isOpenFloat + "  悬浮窗宽度：" + windowWidth);
        if (mBinding == null){
            return;
        }

        if (isOpenFloat){
            ViewGroup.LayoutParams floatParams = mBinding.floatWindow.getLayoutParams();
            floatParams.width = windowWidth + 36;
            mBinding.floatWindow.setVisibility(View.INVISIBLE);
        } else {
            mBinding.floatWindow.setVisibility(View.GONE);
        }
    }

    private static final class InstanceHolder {
        private static final LauncherWindowService instance = new LauncherWindowService();
    }
}
