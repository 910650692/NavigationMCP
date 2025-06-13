package com.fy.navi.hmi.launcher;

import static android.content.Context.WINDOW_SERVICE;

import android.app.ActivityOptions;
import android.content.ComponentCallbacks;
import android.content.Intent;
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
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.exportservice.ExportIntentParam;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.databinding.FloatingWindowLayoutBinding;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.hmi.startup.StartupActivity;
import com.fy.navi.hmi.utils.CaptureScreenUtils;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppCache;
import com.fy.navi.service.adapter.layer.LayerAdapter;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.map.MapType;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.logicpaket.map.IEglScreenshotCallBack;
import com.fy.navi.service.logicpaket.map.IMapPackageCallback;
import com.fy.navi.service.logicpaket.map.MapPackage;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.base.StackManager;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/24
 * Description: [在这里描述文件功能]
 */
public class LauncherWindowService implements IGuidanceObserver, IMapPackageCallback, IEglScreenshotCallBack, CaptureScreenUtils.CaptureScreenCallBack, ComponentCallbacks {
    private static final String TAG = "LauncherWindowService";
    private WindowManager mWindowManager;
    private View mView;
    private FloatingWindowLayoutBinding mBinding;
    private NaviEtaInfo mNaviEtaInfo;
    private NaviPackage mNaviPackage;
    private MapPackage mMapPackage;
    private NavistatusAdapter mNaviStatusAdapter;
    private LaneInfoEntity mLastLanInfo;
    private LayerAdapter mLayerAdapter;
    private final String KEY = "LauncherWindowService";
    private final MapType MAP_TYPE = MapType.MAIN_SCREEN_MAIN_MAP;
    private FloatViewManager mFloatManager;
    private boolean mIsOnShowing = false;
    private int currentUiMode = Configuration.UI_MODE_NIGHT_YES;
    private boolean isInited = false;
    private boolean mCrossImgIsOnShowing = false;
    private CaptureScreenUtils captureScreenUtils;
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
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mWindowManager = (WindowManager) AppCache.getInstance().getMContext().getSystemService(WINDOW_SERVICE);
        mFloatManager = FloatViewManager.getInstance();
        currentUiMode = AppCache.getInstance().getMContext().getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        if (ConvertUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            mNaviEtaInfo = mNaviPackage.getCurrentNaviEtaInfo();
        }
        captureScreenUtils = CaptureScreenUtils.getInstance();
    }

    private void initCallBacks() {
        mNaviPackage.registerObserver(KEY, this);
        mMapPackage.registerCallback(MAP_TYPE, this);
//        mMapPackage.registerEGLScreenshotCallBack(KEY, this);
        mFloatManager.bindLauncherService();
        AppCache.getInstance().getMContext().registerComponentCallbacks(this);
        captureScreenUtils.registerListener(this);
    }

    private void unInitCallBacks() {
        Logger.i(TAG, "unInitCallBacks");
        AppCache.getInstance().getMContext().unregisterComponentCallbacks(this);
        mNaviPackage.unregisterObserver(KEY);
        mMapPackage.unRegisterCallback(MAP_TYPE, this);
//        mMapPackage.unregisterEGLScreenshotCallBack(KEY, this);
        mFloatManager.unBindLauncherService();
        captureScreenUtils.unRegisterListener(this);
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
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding)) {
                mBinding.cardNaviView.setVisibility(View.GONE);
                mBinding.cardTbtView.setVisibility(View.VISIBLE);
                if (ConvertUtils.isEmpty(naviInfoBean)) return;
                mNaviEtaInfo = naviInfoBean;
                updateTbT();
            }
        });
    }

    @Override
    public void onEGLScreenshot(MapType mapType, byte[] bytes) {
        IEglScreenshotCallBack.super.onEGLScreenshot(mapType, bytes);
        if (mapType == MAP_TYPE && !ConvertUtils.isNull(mView) && !ConvertUtils.isEmpty(bytes) && mCrossImgIsOnShowing) {
            captureScreenUtils.processPicture(bytes, mLayerAdapter.getRoadCrossRect(MAP_TYPE));
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding)) {
                mLastLanInfo = laneInfo;
                mBinding.sceneNaviLanes.setVisibility(isShowLane ? View.VISIBLE : View.GONE);
                mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
            }
        });
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        ThreadManager.getInstance().postUi(() -> {
            if (!ConvertUtils.isNull(mBinding)) {
                mBinding.cardNaviView.setVisibility(View.VISIBLE);
                mBinding.cardTbtView.setVisibility(View.GONE);
            }
        });
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

    private void initView() {
        Logger.i(TAG, "initView");
        if (!checkHasOverLay()) {
            Logger.e(TAG, "悬浮窗权限未开启，请开启悬浮窗权限！");
            return;
        }
        if (mView != null && mWindowManager != null) {
            mWindowManager.removeView(mView);
        }
        mBinding = FloatingWindowLayoutBinding.inflate(LayoutInflater.from(AppCache.getInstance().getMContext()), null);
        mView = mBinding.getRoot();

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
        layoutParams.x = 300;
        layoutParams.y = 0;
        mWindowManager.addView(mView, layoutParams);
        initClickListener();
        mBinding.cardTbtView.setVisibility(
                TextUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING) ? View.VISIBLE : View.GONE
        );
        mBinding.cardNaviView.setVisibility(
                TextUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING) ? View.GONE : View.VISIBLE
        );
        if (!ConvertUtils.isNull(mLastLanInfo) && TextUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            mBinding.sceneNaviLanes.onLaneInfo(true, mLastLanInfo);
            updateTbT();
        }
        if (!ConvertUtils.isNull(mNaviEtaInfo) && TextUtils.equals(mNaviStatusAdapter.getCurrentNaviStatus(), NaviStatus.NaviStatusType.NAVING)) {
            updateTbT();
        }
        mBinding.ivCross.setVisibility(mCrossImgIsOnShowing ? View.VISIBLE : View.GONE);
        showOrHideFloatView(mIsOnShowing);
    }

    private void updateTbT() {
        if (ConvertUtils.isNull(mNaviEtaInfo)) return;
        mBinding.sceneNaviTbt.onNaviInfo(mNaviEtaInfo);
        mBinding.sceneNaviEta.onNaviInfo(mNaviEtaInfo);
        mBinding.sceneNaviTmc.onNaviInfo(mNaviEtaInfo);
    }

    /***
     * 启动Navi_App
     */
    @HookMethod(eventName = BuryConstant.EventName.AMAP_WIDGET_ENTERAPP)
    public void openSelf(final int pageCode) {
        Logger.i(TAG, "openSelf:" + pageCode);
        Class startCls = StartupActivity.class;
        boolean isActivityExist = StackManager.getInstance().isActivityExist(MAP_TYPE.name(), MapActivity.class);
        if (isActivityExist) {
            startCls = MapActivity.class;
        }
        Logger.i(TAG, "isActivityExist:" + isActivityExist);
        ExportIntentParam.setIntentPage(pageCode);
        Intent intent = new Intent(AppCache.getInstance().getMContext(), startCls);
        final ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }

    public static void startService() {
        if (TextUtils.equals("buick", BuildConfig.FLAVOR)) return;
        Logger.i(TAG, "凯迪车型显示悬浮窗口！");
        ThreadManager.getInstance().postUi(() -> {
            InstanceHolder.instance.init();
        });
    }

    public static void stopService() {
        Logger.i(TAG, "凯迪车型关闭悬浮窗口！");
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
            mBinding.ivCross.setVisibility(isVisible ? View.VISIBLE : View.GONE);
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
        if (!ConvertUtils.isNull(bitmap) && !ConvertUtils.isNull(mBinding)) {
            mBinding.ivCross.setImageBitmap(bitmap);
        }
    }

    public static LauncherWindowService getInstance() {
        return InstanceHolder.instance;
    }

    private static final class InstanceHolder {
        private static final LauncherWindowService instance = new LauncherWindowService();
    }
}
