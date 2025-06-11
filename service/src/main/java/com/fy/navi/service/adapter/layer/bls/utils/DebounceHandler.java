package com.fy.navi.service.adapter.layer.bls.utils;

import android.os.Handler;
import android.os.Looper;

import com.android.utils.thread.LooperType;
import com.android.utils.thread.ThreadManager;

/**
 * 地图缩放时的回调数据更新防抖
 */
public class DebounceHandler {
    private final Handler mHandler;
    private final long mDelayMillis;
    private Runnable mPendingRunnable;

    public DebounceHandler(long delayMillis) {
        this(delayMillis, ThreadManager.getInstance().getLooper(LooperType.CommonBackGroundLooper));
    }

    public DebounceHandler(long delayMillis, Looper looper) {
        mDelayMillis = delayMillis;
        mHandler = new Handler(looper);
    }

    public void handle(Runnable runnable) {
        if (mPendingRunnable != null) {
            mHandler.removeCallbacks(mPendingRunnable);
        }

        mPendingRunnable = () -> {
            try {
                runnable.run();
            } finally {
                mPendingRunnable = null;
            }
        };

        mHandler.postDelayed(mPendingRunnable, mDelayMillis);
    }

    public void cancel() {
        if (mPendingRunnable != null) {
            mHandler.removeCallbacks(mPendingRunnable);
            mPendingRunnable = null;
        }
    }
}
