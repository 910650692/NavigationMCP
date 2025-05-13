package com.fy.navi.hmi.cluster.utils;

import android.util.Log;

import androidx.annotation.Nullable;

import com.android.utils.NetWorkUtils;
import com.android.utils.thread.ThreadManager;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * 网络状态监控工具类，用于全局监听和获取网络状态
 */
public class NetworkMonitorUtil {

    private static final String TAG = "NetworkMonitor";
    private static final int ONE_SECOND = 1000;

    // 单例实例
    private static final NetworkMonitorUtil instance = new NetworkMonitorUtil();

    // 当前网络状态
    private Boolean currentNetStatus;

    // 监听器列表
    private final List<OnNetStatusChangeListener> listeners = new CopyOnWriteArrayList<>();

    // 网络工具类
    private final NetWorkUtils netWorkUtils;

    private NetworkMonitorUtil() {
        netWorkUtils = NetWorkUtils.Companion.getInstance();
        currentNetStatus = netWorkUtils.checkNetwork();
        // 注册系统网络变化回调
        netWorkUtils.registerNetworkObserver(new NetWorkUtils.NetworkObserver() {
            @Override
            public void onNetConnectSuccess() {
                onNetStatusChange();
            }

            @Override
            public void onNetUnavailable() {
                onNetStatusChange();
            }

            @Override
            public void onNetBlockedStatusChanged() {
                onNetStatusChange();
            }

            @Override
            public void onNetLosing() {
                onNetStatusChange();
            }

            @Override
            public void onNetLinkPropertiesChanged() {
                onNetStatusChange();
            }

            @Override
            public void onNetDisConnect() {
                onNetStatusChange();
            }
        });
    }

    public static NetworkMonitorUtil getInstance() {
        return instance;
    }

    /**
     * 添加网络状态监听器
     */
    public void addListener(@Nullable OnNetStatusChangeListener listener) {
        if (listener != null && !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    /**
     * 移除网络状态监听器
     */
    public void removeListener(@Nullable OnNetStatusChangeListener listener) {
        if (listener != null) {
            listeners.remove(listener);
        }
    }

    /**
     * 获取当前网络状态（true=连接中，false=断开）
     */
    public boolean isNetConnected() {
        return Boolean.TRUE.equals(currentNetStatus);
    }

    /**
     * 网络状态变化通知
     */
    private void onNetStatusChange() {
        ThreadManager.getInstance().postDelay(() -> {
            Boolean isConnected = netWorkUtils.checkNetwork();
            Log.d(TAG, "onNetStatusChange: " + isConnected);

            if (isConnected != currentNetStatus) {
                currentNetStatus = isConnected;
                for (OnNetStatusChangeListener listener : listeners) {
                    listener.onNetStatusChange(Boolean.TRUE.equals(isConnected));
                }
            }
        }, ONE_SECOND);
    }

    /**
     * 网络状态监听接口
     */
    public interface OnNetStatusChangeListener {
        void onNetStatusChange(boolean isConnected);
    }
}
