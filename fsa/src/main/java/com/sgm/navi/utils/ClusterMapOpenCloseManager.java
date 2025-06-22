package com.sgm.navi.utils;

import com.android.utils.log.Logger;
import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

public class ClusterMapOpenCloseManager {//暴露外层的仪表地图开启关闭管理类

    private static final String TAG = "ClusterMapOpenCloseManager";
    private static ClusterMapOpenCloseManager instance;

    private List<ClusterMapOpenCloseListener> mClusterMapOpenCloseListener = new CopyOnWriteArrayList<>();
    // 新增字段：true 表示开启，false 表示关闭
    private boolean open;
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
    public void addClusterMapOpenCloseListener(ClusterMapOpenCloseListener listener) {
        if (listener != null && !mClusterMapOpenCloseListener.contains(listener)) {
            mClusterMapOpenCloseListener.add(listener);
        }
    }

    /**
     * 移除监听器
     */
    public void removeListener(ClusterMapOpenCloseListener listener) {
        mClusterMapOpenCloseListener.remove(listener);
    }

    public boolean isClusterOpen() {
        return open;
    }

    public void setClusterOpen(boolean open) {
        this.open = open;
    }

    /**
     * 触发关闭事件
     */
    public void triggerOpenOrClose(boolean isOpen) {//true 打开 false 关闭
        setClusterOpen(isOpen);
        if (!mClusterMapOpenCloseListener.isEmpty()) {
            Logger.d(TAG, "triggerClose: 关闭");
            for (ClusterMapOpenCloseListener listener : mClusterMapOpenCloseListener) {
                listener.onClusterMapOpenOrClose(isOpen);
            }
        } else {
            Logger.w(TAG, "triggerClose: 未设置监听器");
        }
    }

}
