package com.android.utils.thread;

import android.content.Context;

import com.android.utils.log.Logger;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * @Introduce: 异步线程池.
 * @Author: lvww
 * @Date: 2023/10/11
 * @Description: 根据CPU核数来自动设置最大核心线程以及最大线程数量及线程任务战
 */
public class ThreadPool extends Pool {
    /*** CPU 核数 **/
    private static final int CPU_NAM = Runtime.getRuntime().availableProcessors();

    /*** 核心线程 **/
    private static int THREAD_POOL_CENTER_SIZE = 8;

    /*** 队列最大值 **/
    private static final int QUEUE_NUM = 20;

    /*** 线程保活时间 **/
    private static final long KEEP_ALIVE_TIME = 20L;

    private ThreadPoolExecutor mThreadPool;

    private Context mContext;

    protected ThreadPool(Context context) {
        if (context == null) {
            throw new IllegalArgumentException("Context cannot be null");
        }
        this.mContext = context;
        //取最优核心线程数
        if (CPU_NAM > THREAD_POOL_CENTER_SIZE) THREAD_POOL_CENTER_SIZE = CPU_NAM;
    }

    protected ThreadPool initPool() {
        if (null == mThreadPool) {
            mThreadPool = new ThreadPoolExecutor(THREAD_POOL_CENTER_SIZE,
                    THREAD_POOL_CENTER_SIZE * 10,
                    KEEP_ALIVE_TIME, TimeUnit.SECONDS,
                    new LinkedBlockingQueue<>(QUEUE_NUM),
                    new ThreadFactoryPxy(THREAD_POOL_CENTER_SIZE * 10, Logger.getDefaultTag(), mContext.getPackageName()));
        }
        return this;
    }

    protected boolean isShutdown() {
        if (null == mThreadPool || mThreadPool.isShutdown())
            throw new RuntimeException("ThreadPoolExecutor is shutdown");
        else return false;
    }

    protected void closePool() {
        if (null == mThreadPool) return;
        mThreadPool.shutdown();
        mThreadPool = null;
        mContext = null;
    }

    /**
     * 开启异步线程.{经大量测试, execute没有 @link runAsync执行的快}
     *
     * @param runnable task run
     */
    protected void execute(Runnable runnable) {
        isShutdown();
        mThreadPool.execute(runnable);
    }

    /**
     * 开启异步线程 可携带返回值.
     *
     * @param runTask 任务线程
     */
    protected <T> T execute(RunTask<T> runTask) {
        try {
            isShutdown();
            FutureTask<T> futureTask = new FutureTask<T>(runTask);
            mThreadPool.submit(futureTask);
            return futureTask.get();
        } catch (ExecutionException | InterruptedException e) {
            Logger.e(e.toString());
            return null;
        }
    }

    /**
     * 开启异步线程 可携带返回值.
     *
     * @param runTask 任务线程
     */
    protected <T> T executeNewRun(RunTask<T> runTask) {
        try {
            isShutdown();
            return mThreadPool.submit((Callable<T>) runTask).get();
        } catch (ExecutionException | InterruptedException e) {
            Logger.e(e.toString());
            return null;
        }
    }

    /**
     * 开启异步线程.
     *
     * @param runnable 任务线程
     */
    protected void runAsync(Runnable runnable) {
        if (isShutdown()) return;
        CompletableFuture.runAsync(runnable, mThreadPool);
    }

    /**
     * 开启异步线程 可携带返回值
     *
     * @param runnable 任务线程
     * @param <T>      返回类型参数
     * @return T
     */
    protected <T> T supplyAsync(RunTask<T> runnable) {
        CompletableFuture<T> future = supplyAsyncCall(runnable);
        try {
            return future.get();
        } catch (ExecutionException | InterruptedException e) {
            future.cancel(true);
            return null;
        }
    }

    protected <T> T supplyAsync(RunTask<T> runnable, long timeout) {
        CompletableFuture<T> future = supplyAsyncCall(runnable, timeout);
        try {
            if (0 == timeout) return future.get();
            return future.get(timeout, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            future.cancel(true);
            return null;
        }
    }

    protected <T> CompletableFuture<T> supplyAsyncCall(RunTask<T> runnable) {
        isShutdown();
        return CompletableFuture.supplyAsync(runnable, mThreadPool);
    }

    protected <T> CompletableFuture<T> supplyAsyncCall(RunTask<T> runnable, long timeout) {
        isShutdown();
        return CompletableFuture.supplyAsync(runnable, mThreadPool).completeOnTimeout(null, timeout, TimeUnit.MILLISECONDS);
    }

    /**
     * 开启异步线程 可携带返回值, 本次调用会和参数线程在同一个线程.
     *
     * @param uRunTask 任务线程
     * @param <T>      入参类型,
     * @param <U>      返回类型参数
     * @return U
     */
    protected <T, U> U thenApply(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        CompletableFuture<U> completableFuture = thenApplyCall(future, uRunTask);
        try {
            return completableFuture.get();
        } catch (ExecutionException | InterruptedException e) {
            completableFuture.cancel(true);
            return null;
        }
    }

    protected <T, U> U thenApply(CompletableFuture<T> future, RunFunction<T, U> uRunTask, long timeout) {
        CompletableFuture<U> completableFuture =  thenApplyCall(future, uRunTask).completeOnTimeout(null, timeout, TimeUnit.MILLISECONDS);
        try {
            return completableFuture.get();
        } catch (ExecutionException | InterruptedException e) {
            completableFuture.cancel(true);
            return null;
        }
    }

    protected <T, U> CompletableFuture<U> thenApplyCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return future.thenApply(uRunTask);
    }

    protected <T, U> CompletableFuture<U> thenApplyCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask, long timeout) {
        return future.thenApply(uRunTask).completeOnTimeout(null, timeout, TimeUnit.MILLISECONDS);
    }

    /**
     * 本次调用会和参数线程不在同一个线程.
     *
     * @param uRunTask 任务线程
     * @param <T>      入参类型,
     * @param <U>      返回类型参数
     * @return U
     */
    protected <T, U> U thenApplyAsync(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        CompletableFuture<U> completableFuture = future.thenApplyAsync(uRunTask);
        try {
            return completableFuture.get();
        } catch (ExecutionException | InterruptedException e) {
            completableFuture.cancel(true);
            return null;
        }
    }

    protected <T, U> U thenApplyAsync(CompletableFuture<T> future, RunFunction<T, U> uRunTask, long timeout) {
        CompletableFuture<U> completableFuture = thenApplyAsyncCall(future, uRunTask).completeOnTimeout(null, timeout, TimeUnit.MILLISECONDS);
        try {
            return completableFuture.get(timeout, TimeUnit.MILLISECONDS);
        } catch (ExecutionException | InterruptedException | TimeoutException e) {
            completableFuture.cancel(true);
            return null;
        }
    }

    protected <T, U> CompletableFuture<U> thenApplyAsyncCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask) {
        return future.thenApplyAsync(uRunTask);
    }

    protected <T, U> CompletableFuture<U> thenApplyAsyncCall(CompletableFuture<T> future, RunFunction<T, U> uRunTask, long timeout) {
        return future.thenApplyAsync(uRunTask).completeOnTimeout(null, timeout, TimeUnit.MILLISECONDS);
    }
}