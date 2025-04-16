package com.fy.navi.exportservice;

import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.os.RemoteCallbackList;
import android.os.RemoteException;
import android.util.Log;

import androidx.core.app.ActivityCompat;

import com.fy.navi.NaviService;
import com.fy.navi.exportservice.binderimpl.NaviAutoApiBinder;
import com.fy.navi.mapservice.IBinderPool;
import com.fy.navi.mapservice.IBinderPoolCallback;
import com.fy.navi.mapservice.base.BinderType;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;

import java.util.HashMap;

public final class BinderPool extends IBinderPool.Stub {

    private static final String TAG = BinderPool.class.getSimpleName();

    private IEngineObserver mEngineObserver;
    private final RemoteCallbackList<IBinderPoolCallback> mBinderPoolCallbackList = new RemoteCallbackList<>();
    private final HashMap<String, Binder> mBinderMap = new HashMap<>();
    private boolean mInCallback = false;

    private static final BinderPool INSTANCE = new BinderPool();

    public static BinderPool getInstance() {
        return INSTANCE;
    }

    private BinderPool() {
        //引擎初始化状态监听
        mEngineObserver = new IEngineObserver() {
            @Override
            public void onInitEngineSuccess() {
                if (mInCallback) {
                    return;
                }

                try {
                    mInCallback = true;
                    final int count = mBinderPoolCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        final IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                        if (null != binderPoolCallback) {
                            binderPoolCallback.onEngineInitSuccess();
                        }
                    }
                } catch (RemoteException re) {
                    Log.e(TAG, "dispatch initEngineSuccess error: " + re.getMessage());
                } finally {
                    mBinderPoolCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

            @Override
            public void onInitEngineFail(final int code, final String msg) {
                if (mInCallback) {
                    return;
                }

                try {
                    mInCallback = true;
                    final int count = mBinderPoolCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        final IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                        if (null != binderPoolCallback) {
                            binderPoolCallback.onEngineInitFailed();
                        }
                    }
                } catch (RemoteException exception) {
                    Log.e(TAG, "dispatch onInitEngineFail error: " + exception.getMessage());
                } finally {
                    mBinderPoolCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }
        };
        EnginePackage.getInstance().addEngineObserver(TAG, mEngineObserver);
    }

    @Override
    public void addBindPoolCallback(final String pckName, final IBinderPoolCallback binderPoolCallback) {
        mBinderPoolCallbackList.register(binderPoolCallback, pckName);
    }

    @Override
    public boolean getEngineInitStatus(final String pckName) {
        final boolean initStatus = EnginePackage.getInstance().engineStatus();
        Log.d(TAG, pckName + "getEngineInit: " + initStatus);
        return initStatus;
    }

    @Override
    public void startInitEngine(final String pkgName) {
        Log.d(TAG, pkgName + "startInitEngine");
        if (null != AppContext.getInstance().getMContext()) {
            final Intent intent = new Intent(AppContext.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppContext.getInstance().getMContext(), intent);
        } else {
            Log.e(TAG, "application not created");
        }
    }

    @Override
    public IBinder queryBinder(final String packName, final String binderName) {
        Log.d(TAG, packName + " queryBinder: " + binderName);
        return getBinder(binderName, packName);
    }

    /**
     * 根据Binder名称获取对应实现.
     *
     * @param binderCode 名称.
     * @param packName client包名.
     *
     * @return 对应binder实现.
     */
    private synchronized IBinder getBinder(final String binderCode, final String packName) {
        Log.d(TAG, packName + "getBinder " + binderCode);
        Binder binder = mBinderMap.get(binderCode);
        if (binder == null) {
            if (BinderType.NAVI_AUTO_API.name().equals(binderCode)) {
                binder = new NaviAutoApiBinder();
                mBinderMap.put(binderCode, binder);
            }
        }

        return binder;
    }



}
