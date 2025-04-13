package com.fy.navi;

import android.annotation.SuppressLint;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import androidx.appcompat.app.AppCompatDelegate;
import androidx.core.content.ContextCompat;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.fy.navi.burypoint.BuryManager;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.scene.ui.navi.manager.INaviSceneEvent;
import com.fy.navi.scene.ui.navi.manager.NaviSceneId;
import com.fy.navi.scene.ui.navi.manager.NaviSceneManager;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
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
        initSensorsDataAPI(); // 初始化SDK
        BaseTestCarType testCarType = new TestCarType();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        NaviService.exitProcess();
    }

    private void initARouter() {
        if (BuildConfig.DEBUG) {
            ARouter.openLog();
            ARouter.openDebug();
        }
        ARouter.init(this);
    }

    private void initSensorsDataAPI() {
        BuryManager.getInstance().initSensorsDataAPI(getApplicationContext());
    }
}
