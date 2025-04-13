package com.fy.navi.adas;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.AdasServiceConnectListener;

public final class AdasClient {
    private static final String TAG = AdasClient.class.getSimpleName();

    private AdasManager mAdasManager;

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
        Log.d(TAG, "start");
        mAdasManager = AdasManager.getInstance(context);
        mAdasManager.addAdasServiceConnectListener(mServiceConnectListener);
    }

    //客户端连接状态监听
    private final AdasServiceConnectListener mServiceConnectListener = new AdasServiceConnectListener() {
        @Override
        public void onServiceReady(final boolean serviceReady) {
            Log.d(TAG, "connection status: " + serviceReady);
            if (serviceReady) {
                SuperCruiseManager.getInstance().init(mAdasManager);
                L2PPManager.getInstance().init(mAdasManager);
            } else {
                SuperCruiseManager.getInstance().uninit();
                L2PPManager.getInstance().uninit();
            }
        }
    };

}
