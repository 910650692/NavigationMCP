package com.fy.navi.adas;

import android.content.Context;

import androidx.annotation.NonNull;

import com.gm.cn.adassdk.AdasDataStateListener;
import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.AdasServiceConnectListener;
import com.gm.cn.adassdk.CruiseState;
import com.gm.cn.adassdk.CruiseStateCallback;
import com.gm.cn.adassdk.automateddrivingux.PropertyCallback;

public class AdasClient {

    private AdasManager mAdasManager;
    private boolean mServiceReady;

    private long mAdasTransferRate = 5;
    private long mMinRouteLength;

    public static AdasClient getInstance() {
        return SingleHolder.mInstance;
    }

    private static class SingleHolder {
        private static final AdasClient mInstance = new AdasClient();
    }

    private AdasClient() {}

    /**
     * 启动Adas客户端，尝试连接.
     *
     * @param context Context.
     */
    public void start(@NonNull Context context) {
        mAdasManager = AdasManager.getInstance(context);
        mAdasManager.addAdasServiceConnectListener(mServiceConnectListener);
    }

    private void registerListener() {
        mAdasManager.addAdasDataListener(mAdasDataStateListener);
        mAdasManager.registerCruiseState(mCruiseStateCallback);
        mAdasManager.registerADUPropertyCallback(mPropertyCallback);
    }

    /**
     * 销毁Adas客户端，不再接收连接状态，同时不再发送数据.
     */
    public void destroy() {
        if (null != mAdasManager) {
            mAdasManager.removeAdasDataListener(mAdasDataStateListener);
            mAdasManager.unregisterADUPropertyCallback();
            mAdasManager.unregisterCruiseState(mCruiseStateCallback);
            mAdasManager.removeAdasServiceConnectListener(mServiceConnectListener);
        }

        mServiceReady = false;
    }

    //客户端连接状态监听
    private final AdasServiceConnectListener mServiceConnectListener = new AdasServiceConnectListener() {
        @Override
        public void onServiceReady(boolean serviceReady) {
            mServiceReady = serviceReady;
            if (mServiceReady) {
                registerListener();
            }
        }
    };


    private final AdasDataStateListener mAdasDataStateListener = new AdasDataStateListener() {
        @Override
        public void onAvailable(long minRouteLength) {
            mMinRouteLength = minRouteLength;
        }

        @Override
        public void onAdasStart(long rate) {
            mAdasTransferRate = rate;
        }

        @Override
        public void onChangeRate(long rate) {
            mAdasTransferRate = rate;
        }

        @Override
        public void onAdasStop() {

        }
    };


    private final CruiseStateCallback mCruiseStateCallback = new CruiseStateCallback() {
        @Override
        public void onCruiseStateChanged(CruiseState cruiseState) {

        }
    };


    private final PropertyCallback mPropertyCallback = new PropertyCallback() {
        @Override
        public void onPropertyChange(int propertyId, byte[] bytes) {

        }
    };


}
