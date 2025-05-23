package com.fy.navi;

import android.app.Activity;
import android.app.Application;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatDelegate;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.DeviceUtils;
import com.android.utils.crash.AppCrashRecord;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.launcher.LauncherWindowService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.patacnetlib.PatacNetClient;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.ui.BaseApplication;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication implements Application.ActivityLifecycleCallbacks {
    private static final String TAG = "NaviApplication";

    @Override
    public void onCreate() {
        super.onCreate();
        AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
        AppContext.getInstance().setMApplication(this);
        AppContext.getInstance().setMContext(getApplicationContext());
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        initARouter();
        initDataTrack(); // 初始化SDK
        BaseTestCarType testCarType = new TestCarType();
        PatacNetClient.getInstance().init(this); // 初始化网络适配器

        if (!DeviceUtils.isCar(this)) {
            Thread.setDefaultUncaughtExceptionHandler(new AppCrashRecord(this));
        }
        LauncherWindowService.startService();
        registerActivityLifecycleCallbacks(this);
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        StartService.getInstance().unSdkInit();
        LauncherWindowService.stopService();
    }

    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(this);
    }

    private void initDataTrack() {
        BuryManager.getInstance().initPatacDataTrackManager(getApplicationContext(), DeviceUtils.isCar(getApplicationContext()));
    }

    @Override
    public void onActivityCreated(@NonNull Activity activity, @Nullable Bundle bundle) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.CREATED);
        }
    }

    @Override
    public void onActivityStarted(@NonNull Activity activity) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.STARTED);
        }
    }

    @Override
    public void onActivityResumed(@NonNull Activity activity) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.RESUMED);
        }
    }

    @Override
    public void onActivityPaused(@NonNull Activity activity) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.PAUSED);
        }
    }

    @Override
    public void onActivityStopped(@NonNull Activity activity) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.STOPPED);
        }
    }

    @Override
    public void onActivitySaveInstanceState(@NonNull Activity activity, @NonNull Bundle bundle) {

    }

    @Override
    public void onActivityDestroyed(@NonNull Activity activity) {
        if (activity instanceof MapActivity) {
            updateIsAppInForeground(AutoMapConstant.AppRunStatus.DESTROYED);
        }
    }

}
