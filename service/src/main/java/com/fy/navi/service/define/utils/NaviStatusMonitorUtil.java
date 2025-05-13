package com.fy.navi.service.define.utils;

import android.util.Log;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

import lombok.Getter;

public class NaviStatusMonitorUtil {

    private static final String TAG = "NaviStatusMonitorUtil";

    // 单例实例
    @Getter
    private static final NaviStatusMonitorUtil instance = new NaviStatusMonitorUtil();

    // 监听器列表
    private final List<OnNavigationStatusListener> listeners = new CopyOnWriteArrayList<>();

    private NaviStatusMonitorUtil() {}

    /**
     * 导航开始时调用此方法
     */
    public void notifyNavigationStarted() {
        Log.d(TAG, "notifyNavigationStarted");
        for (OnNavigationStatusListener listener : listeners) {
            listener.onNavigationStarted();
        }
    }

    /**
     * 导航结束时调用此方法
     */
    public void notifyNavigationStopped() {
        Log.d(TAG, "notifyNavigationStopped");
        for (OnNavigationStatusListener listener : listeners) {
            listener.onNavigationStopped();
        }
    }

    /**
     * 添加监听器
     */
    public void addListener(OnNavigationStatusListener listener) {
        if (listener != null && !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    /**
     * 移除监听器
     */
    public void removeListener(OnNavigationStatusListener listener) {
        listeners.remove(listener);
    }

    /**
     * 监听器接口
     */
    public interface OnNavigationStatusListener {
        void onNavigationStarted();
        void onNavigationStopped();
    }
}
