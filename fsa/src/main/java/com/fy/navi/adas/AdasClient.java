package com.fy.navi.adas;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.AdasServiceConnectListener;

public final class AdasClient {
    private static final String TAG = AdasClient.class.getSimpleName();

    private AdasManager mAdasManager;
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
        Logger.d(TAG,"AdasClient start ");
        mAdasManager = AdasManager.getInstance(context);
        mAdasManager.addAdasServiceConnectListener(mServiceConnectListener);
    }

    /**
     * 销毁Adas客户端，不再接收连接状态，同时不再发送数据.
     */
    public void destroy() {
        if (null != mAdasManager) {
            mAdasManager.unregisterADUPropertyCallback();
            mAdasManager.removeAdasServiceConnectListener(mServiceConnectListener);
        }
        mServiceReady = false;
    }

    //客户端连接状态监听
    private final AdasServiceConnectListener mServiceConnectListener = new AdasServiceConnectListener() {
        @Override
        public void onServiceReady(final boolean serviceReady) {
            Log.d(TAG, "onServiceReady: " + serviceReady);
            mServiceReady = serviceReady;
            if (mServiceReady) {
                SuperCruiseManager.getInstance().init(mAdasManager);
                L2PPManager.getInstance().init(mAdasManager);
            }
        }
    };

}
