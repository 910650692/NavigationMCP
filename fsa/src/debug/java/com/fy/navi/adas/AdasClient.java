package com.fy.navi.adas;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;


public final class AdasClient {
    private static final String TAG = AdasClient.class.getSimpleName();

    private boolean mServiceReady;

    public static AdasClient getInstance() {
        return SingleHolder.INSTANCE;
    }

    private final static class SingleHolder {
        private static final AdasClient INSTANCE = new AdasClient();
    }

    private AdasClient() {
    }

    /**
     * 启动Adas客户端，尝试连接.
     *
     * @param context Context.
     */
    public void start(@NonNull final Context context) {
        Logger.e(TAG,"AdasClient start error");
    }

    /**
     * 销毁Adas客户端，不再接收连接状态，同时不再发送数据.
     */
    public void destroy() {
        mServiceReady = false;
    }

}
