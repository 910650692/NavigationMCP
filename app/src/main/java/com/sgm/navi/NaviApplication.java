package com.sgm.navi;

import static com.sgm.navi.service.MapDefaultFinalTag.NAVI_EXIT;

import android.app.Activity;
import android.app.Application;

import androidx.annotation.NonNull;

import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.process.ProcessStatus;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.flavor.CarModelsFeature;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.patacnetlib.PatacNetClient;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.ui.BaseApplication;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class NaviApplication extends BaseApplication implements Application.ActivityLifecycleCallbacks {
    private static final String TAG = MapDefaultFinalTag.INIT_HMI_TAG;

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.setDefaultTag(MapDefaultFinalTag.DEFAULT_TAG);
        Logger.e("NaviApp_Start", "isExternalStorageAvailable start");
        if (!FileUtils.getInstance().checkExternalStorageAvailable()) {
            Logger.e("NaviApp_Start", "isExternalStorageAvailable is false = killSelf");
            killSelf();
        }
        initComponent();
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "onTerminate");
        CarModelsFeature.getInstance().unRegisterBroadcast();
        StartService.getInstance().unSdkInit();
    }

    @Override
    protected void updateProgressStatus(@NonNull Activity activity, int processStatus) {
        if (activity instanceof MapActivity) {
            ProcessManager.updateProcessStatus(processStatus);
            switch (processStatus) {
                case ProcessStatus.AppRunStatus.RESUMED ->
                        ProcessManager.updateIsAppInForeground(true);
                case ProcessStatus.AppRunStatus.PAUSED ->
                        ProcessManager.updateIsAppInForeground(false);
            }
        }
    }


    private void killSelf() {
        ThreadManager.getInstance().execute(() -> {
            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                Logger.w(TAG, "killSelf exception", e.getMessage());
            }
            Logger.e(NAVI_EXIT, "isExternalStorageAvailable is false");
            System.exit(1);
        });
    }

    private void initComponent() {
        AppCache.getInstance().setMApplication(this);
        AppCache.getInstance().setMContext(getApplicationContext());
        registerActivityLifecycleCallbacks(NaviApplication.this);
        ThreadManager.getInstance().execute(() -> {
            try {
                PatacNetClient.getInstance().init(this); // 初始化网络适配器
            } catch (Exception e) {
                //临时Catch方案，ND8775台架 install的包会crash
                Logger.e(TAG, "PatacNetClient init failed: " + e.getMessage());
            }
        });
    }
}
