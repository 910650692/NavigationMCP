package com.fy.navi.adas;

import android.content.Context;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.activatehq.ActivateHQ;
import com.fy.navi.l2pp.GmcL2ppManager;
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

    public void init(@NonNull final Context context) {
        Logger.d(TAG, "init start");
        mAdasManager = AdasManager.getInstance(context);
        mAdasManager.addAdasServiceConnectListener(mServiceConnectListener);
        Logger.d(TAG, "init end");
    }

    private final AdasServiceConnectListener mServiceConnectListener = new AdasServiceConnectListener() {
        @Override
        public void onServiceReady(final boolean serviceReady) {
            Logger.d(TAG, "connection status: " + serviceReady);
            if (serviceReady) {
                GmcL2ppManager.getInstance().init(mAdasManager);
                SuperCruiseManager.getInstance().init(mAdasManager);
                ActivateHQ.getInstance().init(mAdasManager);
            } else {
                GmcL2ppManager.getInstance().uninit();
                SuperCruiseManager.getInstance().uninit();
            }
        }
    };
}
