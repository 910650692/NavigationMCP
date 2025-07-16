package com.sgm.navi.broadcast;

import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;

import com.android.utils.log.Logger;

import java.util.concurrent.ConcurrentHashMap;

public class FloatWindowReceiver extends BroadcastReceiver {
    private static final String TAG = "FloatWindowReceiver";
    public static volatile boolean isShowMusicTab = false;

    private static final ConcurrentHashMap<String, FloatWindowCallback> callbacks = new ConcurrentHashMap<>();

    @Override
    public void onReceive(Context context, Intent intent) {
        if ("patac.hmi.intent.action.FLOAT_WINDOW_SIDE".equals(intent.getAction())) {
            int windowSide = intent.getIntExtra("windowSide", 0);
            isShowMusicTab = windowSide == 1 || windowSide == 2;
            Logger.d(TAG, "收到悬浮窗广播: windowSide=" + windowSide);

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
