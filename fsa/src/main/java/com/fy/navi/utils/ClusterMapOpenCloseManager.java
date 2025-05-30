package com.fy.navi.utils;

import com.android.utils.log.Logger;

public class ClusterMapOpenCloseManager {

    private static final String TAG = "ClusterMapOpenCloseManager";
    private static ClusterMapOpenCloseManager instance;

    private ClusterMapOpenCloseListener listener;

    private ClusterMapOpenCloseManager() {}

    public static synchronized ClusterMapOpenCloseManager getInstance() {
        if (instance == null) {
            instance = new ClusterMapOpenCloseManager();
        }
        return instance;
    }

    /**
     * 设置关闭监听器
     */
    public void setClusterMapOpenCloseListener(ClusterMapOpenCloseListener listener) {
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
    public void triggerOpenOrClose(boolean isOpen) {//true 打开 false 关闭
        if (listener != null) {
            listener.onClusterMapOpenOrClose(isOpen);
        } else {
            Logger.w(TAG, "triggerClose: 未设置监听器");
        }
    }

}
