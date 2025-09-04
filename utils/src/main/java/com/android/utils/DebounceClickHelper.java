package com.android.utils;

import android.os.SystemClock;

/**
 * 防抖点击工具类（单例模式）
 * 用于防止按钮在短时间内被重复点击
 */
public class DebounceClickHelper {
    private static final long DEFAULT_DEBOUNCE_TIME = 1000; // 默认防抖时间1秒
    private long mLastClickTime = 0;
    // 私有构造函数，防止外部实例化
    private DebounceClickHelper() {
    }

    /**
     * 静态内部类实现单例模式
     */
    private static class SingletonHolder {
        private static final DebounceClickHelper INSTANCE = new DebounceClickHelper();
    }

    /**
     * 获取单例实例
     * @return DebounceClickHelper实例
     */
    public static DebounceClickHelper getInstance() {
        return SingletonHolder.INSTANCE;
    }

    /**
     * 检查是否可以执行点击事件（使用默认防抖时间）
     * @return true-可以执行, false-需要防抖拦截
     */
    public boolean canClick() {
        return canClick(DEFAULT_DEBOUNCE_TIME);
    }

    /**
     * 检查是否可以执行点击事件（使用指定防抖时间）
     * @param debounceTime 防抖时间(毫秒)
     * @return true-可以执行, false-需要防抖拦截
     */
    public boolean canClick(long debounceTime) {
        long currentTime = SystemClock.elapsedRealtime();
        if (currentTime - mLastClickTime > debounceTime) {
            mLastClickTime = currentTime;
            return true;
        }
        return false;
    }

    /**
     * 重置点击时间
     */
    public void reset() {
        mLastClickTime = 0;
    }
}
