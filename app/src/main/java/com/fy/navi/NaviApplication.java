package com.fy.navi;

import android.app.Activity;
import android.app.Application;

import androidx.annotation.NonNull;
import androidx.annotation.WorkerThread;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.DeviceUtils;
import com.android.utils.crash.AppCrashRecord;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.process.ProcessStatus;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.launcher.LauncherWindowService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.hmi.splitscreen.SplitScreenManager;
import com.fy.navi.patacnetlib.PatacNetClient;
import com.fy.navi.service.AppCache;
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
        initComponent();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        StartService.getInstance().unSdkInit();
    }

    @Override
    protected void updateProgressStatus(@NonNull Activity activity, int processStatus) {
        if (activity instanceof MapActivity) {
            ProcessManager.updateProcessStatus(processStatus);
            switch (processStatus) {
                case ProcessStatus.AppRunStatus.RESUMED -> ProcessManager.updateIsAppInForeground(true);
                case ProcessStatus.AppRunStatus.PAUSED -> ProcessManager.updateIsAppInForeground(false);
            }
        }
    }

    private void initComponent() {
        ThreadManager.getInstance().execute(() -> {
            AppCache.getInstance().setMApplication(this);
            AppCache.getInstance().setMContext(getApplicationContext());
            Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
            registerActivityLifecycleCallbacks(NaviApplication.this);
            BaseTestCarType testCarType = new TestCarType();
            initARouter();
            initDataTrack();
            try {
            	PatacNetClient.getInstance().init(this); // 初始化网络适配器
            } catch (Exception e) {
                //临时Catch方案，ND8775台架 install的包会crash
                Logger.e(TAG, "PatacNetClient init failed: " + e.getMessage());
            }
            if (!DeviceUtils.isCar(NaviApplication.this)) {
                Thread.setDefaultUncaughtExceptionHandler(new AppCrashRecord(NaviApplication.this));
            }
            initOtherService();
        });
    }

    /**
     * 初始化路由
     */
    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(this);
    }

    /**
     * 初始化埋点SDK
     */
    private void initDataTrack() {
        BuryManager.getInstance().initPatacDataTrackManager(getApplicationContext(), DeviceUtils.isCar(getApplicationContext()));
    }

    /***
     * 初始化其它服务,运行在子线程
     */
    @WorkerThread
    private void initOtherService() {
        SplitScreenManager.getInstance().init();
        LauncherWindowService.startService();
    }
}
