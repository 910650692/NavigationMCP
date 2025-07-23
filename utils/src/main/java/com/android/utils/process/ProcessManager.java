package com.android.utils.process;

import android.app.ActivityOptions;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/5/30
 */
public class ProcessManager {
    private static final String TAG = ProcessManager.class.getSimpleName();
    private static List<ProcessForegroundStatus> processStatusList = new CopyOnWriteArrayList<>();
    private static boolean mIsAppInForeground = false;
    private static int mAppRunStatus = 0;

    /**
     * 添加应用前后台状态监听.
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void addIsAppInForegroundCallback(final ProcessForegroundStatus callback) {
        processStatusList = ConvertUtils.push(processStatusList, callback);
    }

    /**
     * 移除应用前后台状态监听
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void removeIsAppInForegroundCallback(final ProcessForegroundStatus callback) {
        ConvertUtils.remove(processStatusList, callback);
    }

    /**
     * 清空回调接口.
     */
    public static void removeAllCallback() {
        ConvertUtils.clear(processStatusList);
        processStatusList = null;
    }

    /**
     * 当前应用是否处于前台.
     *
     * @return 前后台状态，true-前台  false-后台.
     */
    public static boolean isAppInForeground() {
        return mIsAppInForeground;
    }

    /**
     * 更新应用前后台状态
     *
     * @param appInForeground 见AutoMapConstant.AppRunStatus
     */
    public static void updateIsAppInForeground(final boolean appInForeground) {
        mIsAppInForeground = appInForeground;
        if (ConvertUtils.isEmpty(processStatusList)) {
            return;
        }
        for (ProcessForegroundStatus callback : processStatusList) {
            if (callback != null) {
                callback.isAppInForeground(mIsAppInForeground);
            }
        }
    }

    /**
     * 获取当前进程运行状态值.
     *
     * @return int, 具体值见ProcessStatus.AppRunStatus.
     */
    public static int getAppRunStatus() {
        return mAppRunStatus;
    }

    /**
     * 更新当前进程状态
     *
     * @param appRunStatus 见AutoMapConstant.AppRunStatus
     */
    public static void updateProcessStatus(@ProcessStatus.AppRunStatus int appRunStatus) {
        mAppRunStatus = appRunStatus;
        if (ConvertUtils.isEmpty(processStatusList)) {
            return;
        }
        for (ProcessForegroundStatus callback : processStatusList) {
            if (callback != null) {
                callback.isAppInForeground(appRunStatus);
            }
        }
    }

    /**
     * 重启进程.
     */
    public static void restartProcess(Context context) {
        if (null == context) return;
        Logger.i(TAG, "current process name", context.getPackageName());
        try {
            final String appPkgName = context.getPackageName();
            final PackageManager packageManager = context.getPackageManager();
            final Intent launcherIntent = packageManager.getLaunchIntentForPackage(appPkgName);
            if (null != launcherIntent) {
                launcherIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                final ActivityOptions options = ActivityOptions.makeBasic();
                options.setLaunchDisplayId(0);
                context.startActivity(launcherIntent, options.toBundle());
            } else {
                Logger.e(TAG, "can't find map hmi");
            }

        } catch (ActivityNotFoundException exception) {
            Logger.e(TAG, "open map error: " + exception.getMessage());
        }
    }

    /**
     * 重启进程.
     *
     * @param context current context
     * @param flag    特殊标志位
     */
    public static void restartProcess(Context context, final boolean flag) {
        if (null == context) return;
        final boolean appForeground = ProcessManager.isAppInForeground();
        Logger.i(TAG, "current process name", context.getPackageName(), "flag", flag, "appForeground", appForeground);
        if (appForeground) return;
        try {
            if (!flag) restartProcess(context);
            else {
                Intent intent = new Intent(Intent.ACTION_MAIN);
                intent.addCategory(Intent.CATEGORY_HOME);
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                context.startActivity(intent);
            }
        } catch (ActivityNotFoundException exception) {
            Logger.e(TAG, "open map error: " + exception.getMessage());
        }
    }

    public interface ProcessForegroundStatus {
        default void isAppInForeground(boolean appInForegroundStatus) {

        }

        default void isAppInForeground(int appRunStatus) {

        }
    }
}
