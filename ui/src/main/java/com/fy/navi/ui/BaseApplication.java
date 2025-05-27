package com.fy.navi.ui;

import android.app.Application;
import android.os.UserManager;

import androidx.annotation.NonNull;
import androidx.lifecycle.DefaultLifecycleObserver;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.ProcessLifecycleOwner;

import com.android.utils.ConvertUtils;
import com.android.utils.DeviceUtils;
import com.android.utils.UtilsManager;
import com.android.utils.log.Logger;

import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArrayList;

public class BaseApplication extends Application {

    public static final String TAG = "BaseApplication";

    private static List<IsAppInForegroundCallback> FOREGROUND_CALLBACK_LIST =
            new CopyOnWriteArrayList<>();

    private static int mIsAppInForeground = 0;

    // bugId:1046245 做测试用，验证完毕删除
    private static int MY_PID = 0;

    @Override
    public void onCreate() {
        super.onCreate();
        if (DeviceUtils.isCar(this) && getSystemService(UserManager.class).isSystemUser()) {
            Logger.d(TAG, "CurrentisSystemuser,killprocess");
            System.exit(0);
        }
        UtilsManager.init(this);
        Random random = new Random();
        MY_PID = random.nextInt(1000);
        Logger.i(TAG, "onCreate, MY_PID = " + MY_PID);
        addLifeCycleObserver();
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
        Logger.i(getClass().getSimpleName(), "应用进程退出");
        UtilsManager.clearCache();
    }

    /**
     * 监听应用前后台状态
     */
    private void addLifeCycleObserver() {
        ProcessLifecycleOwner.get().getLifecycle().addObserver(new DefaultLifecycleObserver() {
            @Override
            public void onStop(final @NonNull LifecycleOwner owner) {
                Logger.i(TAG, "onStop");
                DefaultLifecycleObserver.super.onStop(owner);
            }

            @Override
            public void onResume(final @NonNull LifecycleOwner owner) {
                Logger.i(TAG, "onResume");
                DefaultLifecycleObserver.super.onResume(owner);
            }
        });
    }

    /**
     * 添加应用前后台状态监听
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void addIsAppInForegroundCallback(final IsAppInForegroundCallback callback) {
        Logger.i(TAG, "addIsAppInForegroundCallback callback = " + callback);
        if (!FOREGROUND_CALLBACK_LIST.contains(callback)) {
            FOREGROUND_CALLBACK_LIST.add(callback);
        }
    }

    /**
     * 移除应用前后台状态监听
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void removeIsAppInForegroundCallback(final IsAppInForegroundCallback callback) {
        Logger.i(TAG, "removeIsAppInForegroundCallback callback = " + callback);
        FOREGROUND_CALLBACK_LIST.remove(callback);
    }

    /**
     * 更新应用前后台状态
     *
     * @param isAppInForeground 见AutoMapConstant.AppRunStatus
     */
    public static void updateIsAppInForeground(final int isAppInForeground) {
        Logger.i(TAG, "updateIsAppInForeground isAppInForeground = " + isAppInForeground);
        mIsAppInForeground = isAppInForeground;
        if (ConvertUtils.isEmpty(FOREGROUND_CALLBACK_LIST)) {
            return;
        }
        for (IsAppInForegroundCallback callback : FOREGROUND_CALLBACK_LIST) {
            if (callback != null) {
                callback.isAppInForeground(isAppInForeground);
            }
        }
    }

    /**
     * 判断应用运行状态.
     * 用户操作预期结果
     * 打开应用isForeground = true
     * 从最近任务切换回应用isForeground = true
     * 弹出 Dialog 或悬浮窗isForeground = true
     * 按 Home 键返回桌面isForeground = false
     * 启动另一个应用isForeground = false
     * 锁屏isForeground = false
     *
     * @return 见AutoMapConstant.AppRunStatus.
     */
    public static int isAppInForeground() {
        return mIsAppInForeground;
    }

    public static int getMyPid() {
        return MY_PID;
    }
}
