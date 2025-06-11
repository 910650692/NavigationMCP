package com.fy.navi.utils;

import com.android.utils.log.Logger;

import java.util.ArrayList;
import java.util.List;

public class ActivityCloseManager {//自己的activity关闭接口方法

    private static final String TAG = "ActivityCloseManager";
    private static ActivityCloseManager instance;

    private List<OnCloseActivityListener> listeners = new ArrayList<>();

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

    /**
     * 触发关闭事件
     */
    public void triggerClose(boolean isCluster) {
        if (!listeners.isEmpty()) {
            Logger.d(TAG, "triggerClose: 正在触发关闭");
            for (OnCloseActivityListener listener : listeners) {
                listener.onClose(isCluster);
            }
        } else {
            Logger.w(TAG, "triggerClose: 未设置监听器");
        }
    }
}
