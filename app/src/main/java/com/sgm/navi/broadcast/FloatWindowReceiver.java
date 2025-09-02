package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.android.utils.log.Logger;
import com.sgm.navi.service.logicpaket.navi.NaviPackage;

import java.util.concurrent.ConcurrentHashMap;

public class FloatWindowReceiver extends BroadcastReceiver {
    private static final String TAG = "FloatWindowReceiver";
    public static volatile boolean isShowMusicTab = false;

    private static final ConcurrentHashMap<String, FloatWindowCallback> callbacks = new ConcurrentHashMap<>();
    private int lastWindowSide = -1;
    private int pendingWindowSide = -1;
    private boolean isCurrentlyOnRightSide = false;

    @Override
    public void onReceive(Context context, Intent intent) {
        if ("patac.hmi.intent.action.FLOAT_WINDOW_SIDE".equals(intent.getAction())) {
            int windowSide = intent.getIntExtra("windowSide", 0);

            // 避免连续相同的状态
            if (windowSide == lastWindowSide) {
                Logger.d(TAG, "忽略相同的windowSide: " + windowSide);
                return;
            }

            // 右移广播，会返回两次false，5和3都返回false，只需要处理一次
            if (lastWindowSide == 5 && windowSide == 3) {
                Logger.d(TAG, "过滤5，处理3: " + windowSide);
            } else if (windowSide == 5) {
                Logger.d(TAG, "过滤5: " + windowSide);
                return; // 直接过滤掉5
            }

            // 左移广播，处理4和2的情况：4先来，暂存；2后来，处理2并忽略4
            if (windowSide == 4) {
                Logger.d(TAG, "暂存4，等待2: " + windowSide);
                pendingWindowSide = 4;
                return;
            }
            if (windowSide == 2 && pendingWindowSide == 4) {
                Logger.d(TAG, "处理2，忽略暂存的4: " + windowSide);
                pendingWindowSide = -1;
            }

            // 处理退出悬浮框：1.左侧退出，2右侧退出
            if (windowSide == 6) {
                if (!isShowMusicTab) {
                    Logger.d(TAG, "在右侧收到6，忽略: " + windowSide);
                    return;
                }
            }

            // 更新状态并处理
            lastWindowSide = windowSide;
            isShowMusicTab = windowSide == 1 || windowSide == 2;
            Logger.d(TAG, "收到悬浮窗广播: windowSide=" + windowSide);
            NaviPackage.getInstance().setMIsFloatWindowShow(isShowMusicTab);
            // 遍历所有回调并通知
            for (FloatWindowCallback callback : callbacks.values()) {
                if (callback != null) {
                    callback.onWindowSideChanged(isShowMusicTab);
                }
            }
        }
    }

    // 注册回调
    public static void registerCallback(String key, FloatWindowCallback callback) {
        if (!callbacks.containsKey(key)) {
            callbacks.put(key, callback);
        }
    }

    // 移除回调
    public static void unregisterCallback(String key) {
        callbacks.remove(key);
    }

    public interface FloatWindowCallback {
        /**
         * 当悬浮窗位置发生变化时调用
         * @param isOpenFloat 悬浮窗位置（1=显示 6=隐藏）
         */
        void onWindowSideChanged(boolean isOpenFloat);
    }

}
