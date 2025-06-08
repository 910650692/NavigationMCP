package com.fy.navi.service.adapter.position.bls.comm;


import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * GPS定位计时器，默认2s未收到定位信息，则判断为未定位
 */
public class GpsStatusChecker {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private static final int TIME_OUT = 2;
    private volatile boolean mRun = true;
    private OnTimeOutCallback callback;
    private ScheduledFuture mScheduledFuture;
    private Runnable mCustomTimer;
    private final AtomicInteger mTickCount = new AtomicInteger(0);

    public GpsStatusChecker() {
    }

    public void setTimeOutListener(OnTimeOutCallback c) {
        callback = c;
    }

    public synchronized void startGpsStatusChecker() {
        Logger.i(TAG, "startGpsStatusChecker");
        mTickCount.set(0);
        mCustomTimer = new Runnable() {
            @Override
            public void run() {
                try {
                    Logger.i(TAG, "mRun " + mRun + ",mTickCount:" + mTickCount.get());
                    if (mRun) {
                        boolean isTimeout = false;
                        if (mTickCount.get() < TIME_OUT) {
                            mTickCount.incrementAndGet();
                        } else {
                            mTickCount.set(0);
                            isTimeout = true;
                        }
                        if (callback != null && isTimeout) {
                            Logger.i(TAG, "GpsStatusChecker onTimeOut not receive gps more than " + TIME_OUT + " s");
                            callback.onGpsCheckTimeOut();
                        }
                    }
                } catch (Exception e) {
                    Logger.i(TAG, "GpsStatusChecker interrupt e：" + e.toString());
                }
            }
        };
        mScheduledFuture = ThreadManager.getInstance().asyncAtFixDelay(mCustomTimer, 0, 1000, TimeUnit.MILLISECONDS);
    }

    public synchronized void stopGpsStatusChecker() {
        Logger.i(TAG, "stopGpsStatusChecker");
        mRun = false;
        mTickCount.set(0);
        if (mScheduledFuture != null) {
            ThreadManager.getInstance().cancelDelayRun(mScheduledFuture);
            mCustomTimer = null;
        }
    }

    public synchronized void clearCount() {
        Logger.i(TAG, "clearCount");
        mTickCount.set(0);
    }

    public interface OnTimeOutCallback {
        void onGpsCheckTimeOut();
    }
}
