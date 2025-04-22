package com.fy.navi.adas;

import android.content.Context;
import android.util.Log;

import androidx.annotation.NonNull;

import com.gm.cn.adassdk.AdasManager;
import com.gm.cn.adassdk.AdasServiceConnectListener;

public final class AdasClient {
    /**
     * 本类Tag
     */
    private static final String TAG = AdasClient.class.getSimpleName();

    /**
     * 车端高级辅助驾驶系统 管理类
     */
    private AdasManager mAdasManager;


    /**
     * 获取AdasClient实例
     * @return
     */
    public static AdasClient getInstance() {
        return SingleHolder.INSTANCE;
    }

    /**
     * 创建AdasClient实例
     * @return
     */
    private final static class SingleHolder {
        private static final AdasClient INSTANCE = new AdasClient();
    }

    /**
     * 防止构造函数创建实例
     */
    private AdasClient() {
    }

    /**
     * 启动Adas客户端，尝试连接.
     * @param context Context.
     */
    public void start(@NonNull final Context context) {
        Log.d(TAG, "start");
        //获取AdasManager实例
        mAdasManager = AdasManager.getInstance(context);
        //添加连接状态监听
        mAdasManager.addAdasServiceConnectListener(mServiceConnectListener);
    }

    //客户端连接状态监听
    private final AdasServiceConnectListener mServiceConnectListener = new AdasServiceConnectListener() {
        @Override
        public void onServiceReady(final boolean serviceReady) {//连接状态回调
            Log.d(TAG, "connection status: " + serviceReady);
            //serviceReady == true 初始化SuperCruiseManager和L2PPManager
            //serviceReady == false 反初始化SuperCruiseManager和L2PPManager
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
