package com.fy.navi.utils;

import com.android.utils.log.Logger;

public class ActivityCloseManager {

    private static final String TAG = "ActivityCloseManager";
    private static ActivityCloseManager instance;

    private OnCloseActivityListener listener;

    private ActivityCloseManager() {}

    public static synchronized ActivityCloseManager getInstance() {
        if (instance == null) {
            instance = new ActivityCloseManager();
        }
        return instance;
    }

    /**
     * 设置关闭监听器
     */
    public void setOnCloseListener(OnCloseActivityListener listener) {
        this.listener = listener;
    }

    /**
     * 移除监听器
     */
    public void removeListener() {
        this.listener = null;
    }

    /**
     * 触发关闭事件
     */
    public void triggerClose() {
        if (listener != null) {
            Logger.d(TAG, "triggerClose: 正在触发关闭");
            listener.onClose();
        } else {
            Logger.w(TAG, "triggerClose: 未设置监听器");
        }
    }
}
