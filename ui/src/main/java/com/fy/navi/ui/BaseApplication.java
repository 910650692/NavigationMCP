package com.fy.navi.ui;

import android.app.Activity;
import android.app.Application;
import android.os.Bundle;
import android.os.UserManager;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.android.utils.DeviceUtils;
import com.android.utils.process.ProcessManager;
import com.android.utils.UtilsManager;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessStatus;

public class BaseApplication extends Application implements Application.ActivityLifecycleCallbacks {
    public static final String TAG = "BaseApplication";

    @Override
    public void onCreate() {
        super.onCreate();
        if (DeviceUtils.isCar(this) && getSystemService(UserManager.class).isSystemUser()) {
            Logger.d(TAG, "CurrentisSystemuser,killprocess");
            System.exit(0);
        }
        UtilsManager.init(this);
    }

    @Override
    public void onLowMemory() {
        super.onLowMemory();
    }

    @Override
    public void onTrimMemory(final int level) {
        super.onTrimMemory(level);
        switch (level) {
            case TRIM_MEMORY_COMPLETE:
                // 尽可能释放所有不必要的资源
                Logger.i(getClass().getSimpleName(), "统处于低内存状态，正在运行的进程将被清理。应该尽可能释放所有不必要的资源");
                break;
            case TRIM_MEMORY_MODERATE:
                // 释放中等优先级的资源
                Logger.i(getClass().getSimpleName(), "系统内存不足，后台进程可能会被杀死。释放中等优先级的资源");
                break;
            case TRIM_MEMORY_BACKGROUND:
                // 释放容易恢复的资源
                Logger.i(getClass().getSimpleName(), "应用不在前台运行，应该释放容易恢复的资源");
                break;
            case TRIM_MEMORY_UI_HIDDEN:
                // 进行资源释放操作
                Logger.i(getClass().getSimpleName(), "应用的所有UI都不可见。释放与UI相关的资源");
                break;
            case TRIM_MEMORY_RUNNING_CRITICAL:
                Logger.i(getClass().getSimpleName(), "系统内存极低，正在运行的进程面临被杀死的风险。释放所有不必要的资源");
                break;
            case TRIM_MEMORY_RUNNING_LOW:
                // 释放一些非必要资源
                Logger.i(getClass().getSimpleName(), "系统内存较低，释放一些非必要资源");
                break;
            case TRIM_MEMORY_RUNNING_MODERATE:
                Logger.i(getClass().getSimpleName(), "系统内存充足但不多。可以考虑释放一些资源");
                break;
            default:
                Logger.i(getClass().getSimpleName(), "未知的level = " + level);
                break;
        }
    }

    @Override
    public void onTerminate() {
        super.onTerminate();
        Logger.i(TAG, "应用进程退出");
        UtilsManager.clearCache();
        ProcessManager.removeAllCallback();
    }

    @Override
    public void onActivityCreated(@NonNull Activity activity, @Nullable Bundle bundle) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.CREATED);
    }

    @Override
    public void onActivityStarted(@NonNull Activity activity) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.STARTED);
    }

    @Override
    public void onActivityResumed(@NonNull Activity activity) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.RESUMED);
    }

    @Override
    public void onActivityPaused(@NonNull Activity activity) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.PAUSED);
    }

    @Override
    public void onActivityStopped(@NonNull Activity activity) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.STOPPED);
    }

    @Override
    public void onActivitySaveInstanceState(@NonNull Activity activity, @NonNull Bundle bundle) {

    }

    @Override
    public void onActivityDestroyed(@NonNull Activity activity) {
        updateProgressStatus(activity, ProcessStatus.AppRunStatus.DESTROYED);
    }

    protected void updateProgressStatus(@NonNull Activity activity, int processStatus) {

    }
}
