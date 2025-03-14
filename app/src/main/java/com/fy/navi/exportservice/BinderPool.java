package com.fy.navi.exportservice;

import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.os.RemoteCallbackList;
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
    private final RemoteCallbackList<IBinderPoolCallback> mBinderPoolCallbackList = new RemoteCallbackList<>();
    private final HashMap<String, Binder> mBinderMap = new HashMap<>();
    private boolean mInCallback = false;

    private static final BinderPool mInstance = new BinderPool();

    public static BinderPool getInstance() {
        return mInstance;
    }

    private BinderPool() {
        //引擎初始化状态监听
        IEngineObserver mEngineObserver = new IEngineObserver() {
            @Override
            public void onInitEngineSuccess() {
                if (mInCallback) {
                    return;
                }

                try {
                    mInCallback = true;
                    int count = mBinderPoolCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                        if (null != binderPoolCallback) {
                            binderPoolCallback.onEngineInitSuccess();
                        }
                    }
                } catch (Exception exception) {
                    Log.e(TAG, "dispatch initEngineSuccess error: " + exception.getMessage());
                } finally {
                    mBinderPoolCallbackList.finishBroadcast();
                    mInCallback = false;
                }
            }

            @Override
            public void onInitEngineFail(int code, String msg) {
                if (mInCallback) {
                    return;
                }

                try {
                    mInCallback = true;
                    int count = mBinderPoolCallbackList.beginBroadcast();
                    for (int i = 0; i < count; i++) {
                        IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                        if (null != binderPoolCallback) {
                            binderPoolCallback.onEngineInitFailed();
                        }
                    }
                } catch (Exception exception) {
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
    public void addBindPoolCallback(String pckName, IBinderPoolCallback binderPoolCallback) {
        mBinderPoolCallbackList.register(binderPoolCallback, pckName);
    }

    @Override
    public boolean getEngineInitStatus(String pckName) {
        boolean initStatus = EnginePackage.getInstance().engineStatus();
        Log.d(TAG, pckName + "getEngineInit: " + initStatus);
        return initStatus;
    }

    @Override
    public void startInitEngine(String pkgName) {
        Log.d(TAG, pkgName + "startInitEngine");
        if (null != AppContext.mContext) {
            Intent intent = new Intent(AppContext.mContext, NaviService.class);
            ActivityCompat.startForegroundService(AppContext.mContext, intent);
        } else {
            Log.e(TAG, "application not created");
        }
    }

    @Override
    public IBinder queryBinder(String packName, String binderName) {
        Log.d(TAG, packName + " queryBinder: " + binderName);
        return getBinder(binderName, packName);
    }

    private synchronized IBinder getBinder(String binderCode, String packName) {
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
