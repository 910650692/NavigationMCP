package com.android.utils.thread;

/**
 * @Introduce: .
 * @Author: lvww
 * @Date: 2023/11/13
 * @Description:
 */
public abstract class Pool {
   protected abstract boolean isShutdown();

    protected abstract void closePool();
}
