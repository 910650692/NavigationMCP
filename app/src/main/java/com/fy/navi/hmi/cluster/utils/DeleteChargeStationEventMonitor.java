package com.fy.navi.hmi.cluster.utils;

import java.util.ArrayList;
import java.util.List;

/**
 * 删除自动添加充电站事件监听器
 */
public class DeleteChargeStationEventMonitor {

    // 单例实例
    private static final DeleteChargeStationEventMonitor instance = new DeleteChargeStationEventMonitor();

    // 监听器列表
    private final List<OnDeleteChargeStationListener> listeners = new ArrayList<>();

    private DeleteChargeStationEventMonitor() {}

    public static DeleteChargeStationEventMonitor getInstance() {
        return instance;
    }

    /**
     * 添加监听器
     */
    public void addListener(OnDeleteChargeStationListener listener) {
        if (listener != null && !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    /**
     * 移除监听器
     */
    public void removeListener(OnDeleteChargeStationListener listener) {
        listeners.remove(listener);
    }

    /**
     * 通知所有监听器，用户点击了“确认删除”
     */
    public void notifyDeleteConfirmed() {
        for (OnDeleteChargeStationListener listener : listeners) {
            listener.onDeleteChargeStationConfirmed();
        }
    }

    /**
     * 删除充电站监听接口
     */
    public interface OnDeleteChargeStationListener {
        void onDeleteChargeStationConfirmed();
    }
}
