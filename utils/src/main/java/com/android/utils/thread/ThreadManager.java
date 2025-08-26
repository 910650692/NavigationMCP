package com.android.utils.thread;

import android.content.Context;
import android.os.HandlerThread;
import android.os.Looper;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

/**
 * @Introduce: 线程池管理API.
 * @Author: lvww
 * @Date: 2023/10/11
 * @Description:
 */
public class ThreadManager {
    private ThreadPool mThreadPool;

    private ThreadHandlerPool mThreadHandler;

    private ThreadScheduledPool mThreadScheduled;
    private final Map<LooperType, HandlerThread> mHandlerThreadMap = new HashMap<>();

    private ThreadManager() {
/*        throw new RuntimeException("Can't create ThreadUtils " + getCurrentThreadName()
                + " Private constructors cannot be created");*/
    }

    public void initThreadPool(Context context) {
        mThreadPool = new ThreadPool(context).initPool();
        mThreadHandler = new ThreadHandlerPool();
        mThreadScheduled = new ThreadScheduledPool(context);
    }

    public Thread getCurrentThread() {
        return Thread.currentThread();
    }

    public long getCurrentThreadId() {
        return Thread.currentThread().getId();
    }

    public String getCurrentThreadName() {
        return Thread.currentThread().getName();
    }

    public boolean isMainThread() {
        return ConvertUtils.equals(Looper.getMainLooper().getThread(), getCurrentThread());
    }

    public Looper getLooper(LooperType type) {
        HandlerThread thread = mHandlerThreadMap.get(type);
        if (thread != null) {
            return thread.getLooper();
        }
        thread = new HandlerThread(type.getThreadName());
        thread.start();
        mHandlerThreadMap.put(type, thread);
        return thread.getLooper();
    }

    /**
     * Get current thread info.
     *
     * @return thread info
     */
    public String getThreadInfo() {
        Thread thread = Thread.currentThread();
        return "current thread info -> group："
                + thread.getThreadGroup()
                + ", name："
                + thread.getName()
                + ", id："
                + thread.getId()
                + ", isMain："
                + isMainThread();
    }

    public void removeHandleTask(Runnable runnable) {
        mThreadHandler.removeTask(runnable);
    }

    public void runOnUiThread(Runnable runnable) {
        if (isMainThread()) runnable.run();
        else postUi(runnable);
    }

    public void postUi(Runnable runnable) {
        mThreadHandler.postUi(runnable);
    }

    public void postDelay(Runnable runnable, long time) {
        mThreadHandler.postDelay(runnable, time);
    }

    public void execute(Runnable runnable) {
        mThreadPool.execute(runnable);
    }

    public <T> T execute(RunTask<T> runnable) {
        return mThreadPool.execute(runnable);
    }

    public <T> T executeNewRun(RunTask<T> runnable) {
        return mThreadPool.executeNewRun(runnable);
    }

    public void runAsync(Runnable runnable) {
        mThreadPool.runAsync(runnable);
    }

    public <T> T supplyAsync(RunTask<T> runnable) {
        return mThreadPool.supplyAsync(runnable);
    }

    public <T> T supplyAsync(RunTask<T> runnable, long timeout) {
        return mThreadPool.supplyAsync(runnable, timeout);
    }

    public <T> CompletableFuture<T> supplyAsyncCall(RunTask<T> runnable) {
        return mThreadPool.supplyAsyncCall(runnable);
    }

    public <T> CompletableFuture<T> supplyAsyncCall(RunTask<T> runnable, long timeout) {
        return mThreadPool.supplyAsyncCall(runnable, timeout);
    }

    public <T, U> U thenApply(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return mThreadPool.thenApply(future, uRunTask);
    }

    public <T, U> U thenApply(CompletableFuture<T> future, RunFunction<T, U> uRunTask, int timeout) {
        return mThreadPool.thenApply(future, uRunTask, timeout);
    }

    public <T, U> CompletableFuture<U> thenApplyCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return mThreadPool.thenApplyCall(future, uRunTask);
    }

    public <T, U> CompletableFuture<U> thenApplyCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask, int timeout) {
        return mThreadPool.thenApplyCall(future, uRunTask, timeout);
    }

    public <T, U> U thenApplyAsync(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return mThreadPool.thenApplyAsync(future, uRunTask);
    }

    public <T, U> U thenApplyAsync(CompletableFuture<T> future, RunFunction<T, U> uRunTask, int timeout) {
        return mThreadPool.thenApplyAsync(future, uRunTask, timeout);
    }

    public <T, U> CompletableFuture<U> thenApplyAsyncCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return mThreadPool.thenApplyAsyncCall(future, uRunTask);
    }

    public <T, U> CompletableFuture<U> thenApplyAsyncCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask, int timeout) {
        return mThreadPool.thenApplyAsyncCall(future, uRunTask, timeout);
    }

    public ScheduledFuture asyncDelayWithResult(Runnable run, long start) {
        return mThreadScheduled.schedule(run, start);
    }

    public ScheduledFuture asyncDelayWithResult(Runnable run, long start, TimeUnit unit) {
        return mThreadScheduled.schedule(run, start, unit);
    }

    public void asyncDelay(Runnable run, long start) {
        mThreadScheduled.schedule(run, start);
    }

    public void asyncDelay(Runnable run, long start, TimeUnit unit) {
        mThreadScheduled.schedule(run, start, unit);
    }

    public ScheduledFuture asyncWithFixDelay(Runnable run, long start, long interval) {
        return mThreadScheduled.scheduleWithFixed(run, start, interval);
    }

    public ScheduledFuture asyncWithFixDelay(Runnable run, long start, long interval, TimeUnit unit) {
        return mThreadScheduled.scheduleWithFixed(run, start, interval, unit);
    }

    public ScheduledFuture asyncAtFixDelay(Runnable run, long start, long interval) {
        return mThreadScheduled.scheduleAtFixedRate(run, start, interval);
    }

    public ScheduledFuture asyncAtFixDelay(Runnable run, long start, long interval, TimeUnit unit) {
        return mThreadScheduled.scheduleAtFixedRate(run, start, interval, unit);
    }

    public void cancelDelayRun(ScheduledFuture scheduledFuture) {
        mThreadScheduled.cancelSchedule(scheduledFuture);
    }

    public void sleep(int time, TimeUnit unit){
        try {
            unit.sleep(time);
        } catch (InterruptedException e) {
            Logger.e(e.toString());
        }
    }

    public void closePool() {
        mThreadPool.closePool();
        mThreadHandler.closePool();
        mThreadScheduled.closePool();
        mThreadPool = null;
        mThreadHandler = null;
        mThreadScheduled = null;
        for (HandlerThread handlerThread : mHandlerThreadMap.values()) {
            handlerThread.quitSafely();
        }
        mHandlerThreadMap.clear();
    }

    public static ThreadManager getInstance() {
        return Helper.tu;
    }

    private static class Helper {
        private static final ThreadManager tu = new ThreadManager();
    }
}