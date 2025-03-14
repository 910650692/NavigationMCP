package com.fy.navi;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;

import androidx.core.content.ContextCompat;

import com.alibaba.android.arouter.launcher.ARouter;
import com.android.utils.log.Logger;
import com.fy.navi.flavor.BaseTestCarType;
import com.fy.navi.flavor.TestCarType;
import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.MapDefaultFinalTag;
import com.fy.navi.service.logicpaket.navi.NaviPackage;
import com.fy.navi.ui.BaseApplication;
import com.fy.navi.ui.IsAppInForegroundCallback;

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
        BaseTestCarType testCarType = new TestCarType();
        AppContext.mApplication = this;
        AppContext.mContext = getApplicationContext();
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        initARouter();
//        test();
    }

    private void test() {
        Logger.i("shisong", "test");
        ContextCompat.registerReceiver(getApplicationContext(), new BroadcastReceiver() {
            @Override
            public void onReceive(Context context, Intent intent) {
                Logger.i("shisong", "onReceive");


                Logger.i("shisong", "get foreground = " + NaviPackage.getInstance().getIsAppInForeground());

            }
        }, new IntentFilter("shi.song"), ContextCompat.RECEIVER_EXPORTED);
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
}
