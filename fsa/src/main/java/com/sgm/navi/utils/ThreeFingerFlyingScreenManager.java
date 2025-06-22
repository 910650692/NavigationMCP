package com.sgm.navi.utils;

import com.android.utils.log.Logger;

import java.util.ArrayList;
import java.util.List;

public class ThreeFingerFlyingScreenManager {//飞屏

    private static final String TAG = "ActivityCloseManager";
    private static ThreeFingerFlyingScreenManager instance;

    private List<ThreeFingerFlyingScreenListener> listeners = new ArrayList<>();

    private ThreeFingerFlyingScreenManager() {}

    public static synchronized ThreeFingerFlyingScreenManager getInstance() {
        if (instance == null) {
            instance = new ThreeFingerFlyingScreenManager();
        }
        return instance;
    }

    public void addOnCloseListener(ThreeFingerFlyingScreenListener listener) {
        if (listener != null && !listeners.contains(listener)) {
            listeners.add(listener);
        }
    }

    public void removeOnCloseListener(ThreeFingerFlyingScreenListener listener) {
        listeners.remove(listener);
    }

    /**
     * 触发关闭事件
     */
    public void triggerFlyingScreen(boolean isLeft) {
        if (!listeners.isEmpty()) {
            Logger.d(TAG, "triggerClose: 飞屏");
            for (ThreeFingerFlyingScreenListener listener : listeners) {
                listener.onThreeFingerFlyingScreenCall(isLeft);
            }
        } else {
            Logger.w(TAG, "triggerClose: 未设置监听器");
        }
    }
}
