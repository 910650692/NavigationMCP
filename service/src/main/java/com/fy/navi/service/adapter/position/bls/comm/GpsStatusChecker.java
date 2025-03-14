package com.fy.navi.service.adapter.position.bls.comm;


import com.android.utils.log.Logger;
import com.fy.navi.service.MapDefaultFinalTag;

import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * GPS定位计时器，默认2s未收到定位信息，则判断为未定位
 */
public class GpsStatusChecker extends Thread {
    private static final String TAG = MapDefaultFinalTag.POSITION_SERVICE_TAG;
    private static final int TIME_OUT = 10;
    private volatile int mTickCount = 0;

    private volatile boolean mRun = false;
    private AtomicInteger mState = new AtomicInteger(0);
    private OnTimeOutCallback callback;

    public GpsStatusChecker() {
    }

    public void setTimeOutListener(OnTimeOutCallback c) {
        callback = c;
    }

    @Override
    public void run() {
        super.run();
        mRun = true;
        while (mRun) {
            switch (mState.get()) {
                case 0:
                    synchronized (this) {
                        try {
                            wait();
                        } catch (InterruptedException e) {
                            Thread.currentThread().interrupt();
                        }
                    }
                    break;
                case 1:
                    try {
                        boolean isTimeout = false;
                        synchronized (this) {
                            mTickCount++;
                            if (mTickCount > TIME_OUT) {
                                mTickCount = 0;
                                isTimeout = true;
                            }
                        }
                        if (callback != null && isTimeout) {
                            Logger.i(TAG, "GpsStatusChecker onTimeOut not receive gps more than " + TIME_OUT + " s");
                            callback.onTimeOut();
                        }
                        synchronized (this) {
                            wait(1000);
                        }
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                        Thread.currentThread().interrupt();
                    }
                    break;
                default:
                    break;
            }
        }
    }

    public synchronized void clearCount() {
        mTickCount = 0;
    }

    public synchronized void cancel() {
        mRun = false;
        notifyAll();
    }

    public synchronized void doWait() {
        if (mState.get() == 0) {
            return;
        }
        mState.set(0);
        mTickCount = 0;
        notifyAll();
    }

    public synchronized void doCount() {
        if (mState.get() == 1) {
            return;
        }
        mState.set(1);
        mTickCount = 0;
        notifyAll();
    }

    public interface OnTimeOutCallback {
        void onTimeOut();
    }
}
