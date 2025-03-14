package com.android.utils.thread;

import android.os.Handler;
import android.os.Looper;

/**
 * @Introduce: Handler线程池,可便面内存泄漏.
 * @Author: lvww
 * @Date: 2023/10/11
 * @Description:
 */
public class ThreadHandlerPool extends Pool {
    private static Handler mHandler;

    protected ThreadHandlerPool() {
        if (null == mHandler) mHandler = new Handler(Looper.getMainLooper());
    }

    protected void postUi(Runnable runnable) {
        isShutdown();
        mHandler.post(runnable);
    }

    protected void postDelay(Runnable runnable, long time) {
        isShutdown();
        mHandler.postDelayed(runnable, time);
    }

    protected void removeTask(Runnable runnable) {
        isShutdown();
        mHandler.removeCallbacks(runnable);
    }

    @Override
    protected boolean isShutdown() {
        if (null == mHandler) throw new RuntimeException("mHandler is shutdown");
        return false;
    }

    @Override
    protected void closePool() {
        if (null != mHandler) mHandler = null;
    }
}
