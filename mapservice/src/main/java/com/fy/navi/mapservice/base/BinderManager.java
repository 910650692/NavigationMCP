package com.fy.navi.mapservice.base;

import android.os.RemoteException;
import android.util.Log;

import com.fy.navi.mapservice.IBinderPool;
import com.fy.navi.mapservice.IBinderPoolCallback;
import com.fy.navi.mapservice.apimanager.IEngineStatusCallback;
import com.fy.navi.mapservice.common.INaviAutoApiBinder;


import java.util.ArrayList;

public final class BinderManager {

    public static final String TAG = BinderManager.class.getSimpleName();
    private final ArrayList<IEngineStatusCallback> mEngineStatusCallbackList = new ArrayList<>();
    private volatile IBinderPool mBinderPool;

    public static BinderManager getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static final class SingletonHolder {
        private static final BinderManager INSTANCE = new BinderManager();
    }

    private BinderManager() {

    }


    private final IBinderPoolCallback.Stub mBinderPoolCallback = new IBinderPoolCallback.Stub() {
        @Override
        public void onEngineInitSuccess() {
            for (IEngineStatusCallback callback : mEngineStatusCallbackList) {
                if (null != callback) {
                    callback.onInitSuccess();
                }
            }
        }

        @Override
        public void onEngineInitFailed() {
            for (IEngineStatusCallback callback : mEngineStatusCallbackList) {
                if (null != callback) {
                    callback.onInitFailed();
                }
            }
        }
    };


    /**
     * 绑定service成功后，更新BinderPool.
     *
     * @param binderPool IBinderPool, Service返回的IBinder.
     */
    public synchronized void updateBinderPool(final IBinderPool binderPool) {
        if (binderPool == null) {
            Log.e(TAG, "updateBinderPool: binderPool null");
            return;
        }
        this.mBinderPool = binderPool;
        judgeEngineStatus();
    }

    /**
     * 移除IBinder.
     */
    public synchronized void removeBinderPool() {
        mBinderPool = null;
    }

    /**
     * 添加SDK引擎初始化状态监听接口.
     *
     * @param callback IEngineStatusCallback.
     */
    public synchronized void addEngineInitCallback(final IEngineStatusCallback callback) {
        if (null == callback) {
            return;
        }
        if (!mEngineStatusCallbackList.contains(callback)) {
            mEngineStatusCallbackList.add(callback);
        }

        judgeEngineStatus();
    }

    /**
     * 绑定成功后根据engine初始化状态执行分发或初始化引擎.
     */
    private void judgeEngineStatus() {
        Log.d(TAG, "BinderManager judgeEngine");
        if (isBonded()) {
            try {
                final boolean engineInit = mBinderPool.getEngineInitStatus(MapSdk.getInstance().getPackageName());
                if (engineInit) {
                    for (IEngineStatusCallback callback : mEngineStatusCallbackList) {
                        if (null != callback) {
                            callback.onInitSuccess();
                        }
                    }
                } else {
                    Log.d(TAG, "BinderManager start initEngine");
                    final String pkgName = MapSdk.getInstance().getPackageName();
                    mBinderPool.addBindPoolCallback(pkgName, mBinderPoolCallback);
                    mBinderPool.startInitEngine(pkgName);
                }
            } catch (RemoteException exception) {
                Log.e(TAG, "getEngineInitStatus error: " + exception.getMessage());
            }
        }
    }

    private boolean isBonded() {
        return mBinderPool != null && mBinderPool.asBinder().pingBinder();
    }


    /**
     * 获取NaviAutoApi接口实现类.
     *
     * @return INaviAutoApiBinder.
     */
    public INaviAutoApiBinder getNaviAutoBinder() {
        INaviAutoApiBinder naviAutoApiBinder = null;
        if (isBonded()) {
            try {
                naviAutoApiBinder =  INaviAutoApiBinder.Stub.asInterface(mBinderPool.queryBinder(
                        MapSdk.getInstance().getPackageName(), BinderType.NAVI_AUTO_API.name()));
            } catch (RemoteException exception) {
                Log.e(TAG, "query naviAutoBinder error: " + exception.getMessage());
            }
        }

        return naviAutoApiBinder;
    }

}
