package com.fy.navi.hmi.launcher;

import android.annotation.SuppressLint;
import android.app.Service;
import android.content.Intent;
import android.content.res.Configuration;
import android.graphics.PixelFormat;
import android.os.Build;
import android.os.IBinder;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.WindowManager;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.databinding.FloatingWindowLayoutBinding;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;
import com.fy.navi.service.define.navi.LaneInfoEntity;
import com.fy.navi.service.define.navi.NaviEtaInfo;
import com.fy.navi.service.define.navi.NaviTmcInfo;
import com.fy.navi.service.define.navistatus.NaviStatus;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.navi.IGuidanceObserver;
import com.fy.navi.service.logicpaket.navi.NaviPackage;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/4/24
 * Description: [在这里描述文件功能]
 */
public class LauncherWindowService extends Service implements IGuidanceObserver {
    private static final String TAG = "LauncherWindowService";
    private WindowManager mWindowManager;
    private View mView;
    private WindowManager.LayoutParams mLayoutParams;
    private FloatingWindowLayoutBinding mBinding;
    private NaviEtaInfo mNaviEtaInfo;
    private NaviPackage mNaviPackage;
    private NavistatusAdapter mNaviStatusAdapter;
    private LaneInfoEntity mLastLanInfo;

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @SuppressLint("NewApi")
    @Override
    public void onCreate() {
        super.onCreate();
        mNaviPackage = NaviPackage.getInstance();
        mNaviPackage.registerObserver(TAG, this);
        mNaviStatusAdapter = NavistatusAdapter.getInstance();
        mWindowManager = (WindowManager) getSystemService(WINDOW_SERVICE);
        initView();
        FloatViewManager.getInstance().bindWindowService(this);
        showOrHideFloatView(false);
    }

    private void initClickListener() {
        if (ConvertUtils.isNull(mBinding)) return;
        mBinding.skIvBasicBus.setOnClickListener(v -> {
            startMapActivity(INaviConstant.OpenIntentPage.GO_COMPANY);
        });
        mBinding.skIvBasicHome.setOnClickListener(v -> {
            startMapActivity(INaviConstant.OpenIntentPage.GO_HOME);
        });
        mBinding.skIvBasicSearch.setOnClickListener(v -> {
            startMapActivity(INaviConstant.OpenIntentPage.SEARCH_PAGE);
        });
        mBinding.cardTbtView.setOnClickListener(v -> {
            startMapActivity(INaviConstant.OpenIntentPage.NONE);
        });
    }

    @Override
    public void onNaviInfo(NaviEtaInfo naviInfoBean) {
        IGuidanceObserver.super.onNaviInfo(naviInfoBean);
        if (!ConvertUtils.isNull(mBinding)) {
            mBinding.cardNaviView.setVisibility(View.GONE);
            mBinding.cardTbtView.setVisibility(View.VISIBLE);
            if (ConvertUtils.isEmpty(naviInfoBean)) return;
            mNaviEtaInfo = naviInfoBean;
            updateTbT();
        }
    }

    @Override
    public void onLaneInfo(final boolean isShowLane, final LaneInfoEntity laneInfo) {
        if (!ConvertUtils.isNull(mBinding)) {
            mLastLanInfo = laneInfo;
            mBinding.sceneNaviLanes.onLaneInfo(isShowLane, laneInfo);
        }
    }

    @Override
    public void onNaviStop() {
        IGuidanceObserver.super.onNaviStop();
        if (!ConvertUtils.isNull(mBinding)) {
            mBinding.cardNaviView.setVisibility(View.VISIBLE);
            mBinding.cardTbtView.setVisibility(View.GONE);
        }
    }

    @Override
    public void onUpdateTMCLightBar(final NaviTmcInfo naviTmcInfo) {
        if (!ConvertUtils.isNull(mBinding)) {
            mBinding.sceneNaviTmc.setIsShowAutoAdd(false);
            mBinding.sceneNaviTmc.onUpdateTMCLightBar(naviTmcInfo);
        }
    }

    private void initView() {
        if (mView != null && mWindowManager != null) {
            mWindowManager.removeView(mView);
        }
        mBinding = FloatingWindowLayoutBinding.inflate(LayoutInflater.from(this), null);
        mView = mBinding.getRoot();

        mLayoutParams = new WindowManager.LayoutParams(
                WindowManager.LayoutParams.WRAP_CONTENT,
                WindowManager.LayoutParams.WRAP_CONTENT,
                Build.VERSION.SDK_INT >= Build.VERSION_CODES.O ?
                        WindowManager.LayoutParams.TYPE_APPLICATION_OVERLAY :
                        WindowManager.LayoutParams.TYPE_PHONE,
                WindowManager.LayoutParams.FLAG_NOT_FOCUSABLE,
                PixelFormat.TRANSLUCENT
        );

        mLayoutParams.gravity = Gravity.START | Gravity.TOP;
        mLayoutParams.x = 300;
        mLayoutParams.y = 0;
        mWindowManager.addView(mView, mLayoutParams);
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
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        Logger.i(TAG, "onConfigurationChanged");
        initView();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        FloatViewManager.getInstance().unBindWindowService();
        if (!ConvertUtils.isNull(mNaviPackage)) {
            mNaviPackage.unregisterObserver(TAG);
        }
        if (mWindowManager != null && mView != null) {
            mWindowManager.removeView(mView);
        }
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
    public void startMapActivity(int pageCode) {
        Logger.i(TAG, "startMapActivity:" + pageCode);
        Intent intent = new Intent(AppContext.getInstance().getMContext(), MapActivity.class);
        intent.putExtra(INaviConstant.PAGE_EXTRA, pageCode);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        AppContext.getInstance().getMContext().startActivity(intent);
    }

    public static void startService() {
        if (!TextUtils.equals("cadi", BuildConfig.FLAVOR)) return;
        Logger.i(TAG, "凯迪车型显示悬浮窗口！");
        AppContext.getInstance().getMContext().startService(
                new Intent(AppContext.getInstance().getMContext(), LauncherWindowService.class)
        );
    }

    public static void stopService() {
        Logger.i(TAG, "凯迪车型关闭悬浮窗口！");
        AppContext.getInstance().getMContext().stopService(
                new Intent(AppContext.getInstance().getMContext(), LauncherWindowService.class)
        );
    }

    public void showOrHideFloatView(boolean isShow) {
        Logger.i(TAG, "showOrHideFloatView:" + isShow);
        ThreadManager.getInstance().postUi(new Runnable() {
            @Override
            public void run() {
                if (!ConvertUtils.isNull(mView)) {
                    mView.setVisibility(isShow ? View.VISIBLE : View.GONE);
                }
            }
        });
    }
}
