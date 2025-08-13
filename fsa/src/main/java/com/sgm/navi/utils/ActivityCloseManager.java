package com.sgm.navi.utils;

import com.android.utils.log.Logger;

import java.util.ArrayList;
import java.util.List;

public class ActivityCloseManager {//自己的activity关闭接口方法

    private static final String TAG = "ActivityCloseManager";
    private static ActivityCloseManager instance;

    private final List<OnCloseActivityListener> listeners = new ArrayList<>();

    private final List<OnOpenOrCloseActivityListener> openOrCloseListeners = new ArrayList<>();

    private ActivityCloseManager() {}

    public static synchronized ActivityCloseManager getInstance() {
        if (instance == null) {
            instance = new ActivityCloseManager();
        }
        return instance;
    }

    public void addOnCloseListener(OnCloseActivityListener listener) {
        if (listener != null && !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    public void removeOnCloseListener(OnCloseActivityListener listener) {
        listeners.remove(listener);
    }

    public void addOnOpenOrCloseListener(OnOpenOrCloseActivityListener listener) {
        if (listener != null && !openOrCloseListeners.contains(listener)) {
            openOrCloseListeners.add(listener);
        }
    }

    public void removeOnOpenOrCloseListener(OnOpenOrCloseActivityListener listener) {
        openOrCloseListeners.remove(listener);
    }

    /**
     * 触发关闭事件
     */
    public void triggerClose(boolean isCluster, boolean isOpen) {
        if (!listeners.isEmpty()) {
            Logger.d(TAG, "triggerClose ActivityCloseManager: 正在触发关闭");
            for (OnCloseActivityListener listener : listeners) {
                listener.onOpenOrClose(isCluster, isOpen);
            }
        } else {
            Logger.w(TAG, "triggerClose ActivityCloseManager: 未设置监听器");
        }
    }

    /**
     * 触发RearScreen开启与关闭事件
     */
    public void triggerRearScreenOpenOrClose(int mapType, boolean isOpen) {
        if (!openOrCloseListeners.isEmpty()) {
            Logger.d(TAG, "triggerRearScreenOpenOrClose");
            for (OnOpenOrCloseActivityListener listener : openOrCloseListeners) {
                listener.onOpenOrClose(mapType, isOpen);
            }
        } else {
            Logger.w(TAG, "triggerRearScreenOpenOrClose: 未设置监听器");
        }
    }
}
