package com.android.utils.process;

import com.android.utils.ConvertUtils;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/5/30
 */
public class ProcessManager {
    private static List<ProcessForegroundStatus> processStatusList = new CopyOnWriteArrayList<>();
    private static boolean mIsAppInForeground;

    /**
     * 添加应用前后台状态监听
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void addIsAppInForegroundCallback(ProcessForegroundStatus callback) {
        processStatusList = ConvertUtils.push(processStatusList, callback);
    }

    /**
     * 移除应用前后台状态监听
     *
     * @param callback IsAppInForegroundCallback
     */
    public static void removeIsAppInForegroundCallback(ProcessForegroundStatus callback) {
        ConvertUtils.remove(processStatusList, callback);
    }

    public static void removeAllCallback() {
        ConvertUtils.clear(processStatusList);
        processStatusList = null;
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
    public static boolean isAppInForeground() {
        return mIsAppInForeground;
    }

    /**
     * 更新应用前后台状态
     *
     * @param appInForegroundStatus 见AutoMapConstant.AppRunStatus
     */
    public static void updateIsAppInForeground(boolean appInForegroundStatus) {
        mIsAppInForeground = appInForegroundStatus;
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
     * 更新当前进程状态
     *
     * @param isAppInForeground 见AutoMapConstant.AppRunStatus
     */
    public static void updateProcessStatus(@ProcessStatus.AppRunStatus int isAppInForeground) {
        if (ConvertUtils.isEmpty(processStatusList)) return;
        for (ProcessForegroundStatus callback : processStatusList) {
            if (callback != null) {
                callback.isAppInForeground(isAppInForeground);
            }
        }
    }

    public interface ProcessForegroundStatus {
        default void isAppInForeground(boolean appInForegroundStatus) {

        }

        default void isAppInForeground(int isInForeground) {

        }
    }
}
