package com.fy.navi;

import androidx.appcompat.app.AppCompatDelegate;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.DeviceUtils;
import com.android.utils.crash.AppCrashRecord;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.StartService;
import com.fy.navi.service.adapter.search.cloudByPatac.PatacNetClient;
import com.fy.navi.ui.BaseApplication;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication {
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
        PatacNetClient.getInstance().init(); // 初始化网络适配器

        if (!DeviceUtils.isCar(this)) {
            Thread.setDefaultUncaughtExceptionHandler(new AppCrashRecord(this));
        }
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        StartService.getInstance().unSdkInit();
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
}
