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
        return SingletonHolder.mInstance;
    }

    private static class SingletonHolder {
        private static final BinderManager mInstance = new BinderManager();
    }

    private BinderManager() {}


    private final IBinderPoolCallback.Stub mBinderPoolCallback = new IBinderPoolCallback.Stub() {
        @Override
        public void onEngineInitSuccess() {
            for (IEngineStatusCallback callback : mEngineStatusCallbackList) {
                if (null != callback) {
                    try {
                        callback.onInitSuccess();
                    } catch (Exception exception) {
                        Log.e(TAG, "dispatch initSuccess error: " + exception.getMessage());
                    }
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


    public synchronized void updateBinderPool(IBinderPool binderPool) {
        if (binderPool == null) {
            Log.e(TAG, "updateBinderPool: binderPool null");
            return;
        }
        this.mBinderPool = binderPool;
        judgeEngineStatus();
    }

    public synchronized void removeBinderPool() {
        mBinderPool = null;
    }

    public synchronized void addEngineInitCallback(IEngineStatusCallback callback) {
        if (null == callback) {
            return;
        }
        if (!mEngineStatusCallbackList.contains(callback)) {
            mEngineStatusCallbackList.add(callback);
        }

        judgeEngineStatus();
    }

    //绑定成功后根据engine初始化状态执行分发或初始化引擎
    private void judgeEngineStatus() {
        Log.d(TAG, "BinderManager judgeEngine");
        if (isBonded()) {
            try {
                boolean engineInit = mBinderPool.getEngineInitStatus(MapSdk.getInstance().getPackageName());
                if (engineInit) {
                    for (IEngineStatusCallback callback : mEngineStatusCallbackList) {
                        if (null != callback) {
                            callback.onInitSuccess();
                        }
                    }
                } else {
                    Log.d(TAG, "BinderManager start initEngine");
                    String pkgName = MapSdk.getInstance().getPackageName();
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


    //获取NaviAutoApi接口实现类
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
