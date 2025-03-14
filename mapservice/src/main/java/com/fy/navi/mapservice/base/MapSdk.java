package com.fy.navi.mapservice.base;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Looper;
import android.os.Message;
import android.os.Process;
import android.os.RemoteException;
import android.os.UserHandle;
import android.util.Log;

import com.fy.navi.mapservice.IBinderPool;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.NoSuchElementException;

public class MapSdk {

    private static final String TAG = MapSdk.class.getSimpleName();

    private final String EXPORT_SERVICE_PKG = "com.fy.navi.hmi";
    private final String EXPORT_MAP_SERVICE_CLS = "com.fy.navi.exportservice.MapService";
    private final int DELAY_MILLIS = 2 * 1000;
    private final int MSG_CONNECT_SERVICE = 0x00000;
    private final HandlerThread keepConnectHandlerThread;
    private final Handler keepConnectHandler;

    private final ArrayList<ServiceStateCallback> callbackList = new ArrayList<>();
    private WeakReference<Context> mContextReference;
    private IBinder mIBinder;
    private String packageName = "";
    private boolean mConnected;

    private static class SingletonHolder {
        private static final MapSdk mInstance = new MapSdk();
    }

    private MapSdk() {
        keepConnectHandlerThread = new HandlerThread(getClass().getSimpleName());
        startKeepConnectHandlerThread();
        keepConnectHandler = new Handler(keepConnectHandlerThread.getLooper(), mHandlerCallback);
    }

    public static MapSdk getInstance() {
        return SingletonHolder.mInstance;
    }

    public void startConnect(Context context) {
        if (mConnected) {
            Log.d(TAG, "MapSdk already connected");
        } else {
            Log.d(TAG, "MapSdk startConnect");
            mContextReference = new WeakReference<>(context);
            packageName = context.getPackageName();
            tryConnectService(0);
        }
    }

    private void stopConnect() {
        BinderManager.getInstance().removeBinderPool();
        if (mIBinder != null) {
            mIBinder.unlinkToDeath(deathRecipient, 0);
            mIBinder = null;
        }
    }

    private void registerCallback(ServiceStateCallback callback) {
        synchronized (callbackList) {
            if (!callbackList.contains(callback)) {
                callbackList.add(callback);
            }
        }
    }

    private void unregisterCallback(ServiceStateCallback callback) {
        synchronized (callbackList) {
            callbackList.remove(callback);
        }
    }

    /**
     * 与service连接和断开连接时回调
     */
    private final ServiceConnection serviceConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder iBinder) {
            Log.d(TAG, "onServiceConnected");
            try {
                mConnected = true;
                mIBinder = iBinder;
                mIBinder.linkToDeath(deathRecipient, 0);
                keepConnectHandler.removeMessages(MSG_CONNECT_SERVICE);
                BinderManager.getInstance().updateBinderPool(IBinderPool.Stub.asInterface(iBinder));
            } catch (RemoteException e) {
                Log.e(TAG, "The target IBinder's process has already died: " + e);
                tryConnectService(0);
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            Log.d(TAG, "onServiceDisconnected");
            mConnected = false;
            tryConnectService(500);
        }
    };

    /**
     * 绑定的service死亡时回调
     */
    private final IBinder.DeathRecipient deathRecipient = () -> {
        Log.w(TAG, "binderDied callback");
        tryConnectService(500);
    };

    private final Handler.Callback mHandlerCallback = message -> {
        if (mIBinder != null && mIBinder.pingBinder()) {
            Log.i(TAG, "Service connected");
            return true;
        }
        Intent intent = new Intent();
        intent.setClassName(EXPORT_SERVICE_PKG, EXPORT_MAP_SERVICE_CLS);
        Bundle bundle = new Bundle();
        bundle.putString("packageName",  getPackageName());
        intent.putExtras(bundle);
        try {
            Context context = mContextReference.get();
            if (null != context) {
                context.bindService(intent, serviceConnection, Context.BIND_AUTO_CREATE);
            }
        } catch (SecurityException e) {
            Log.e(TAG, "The caller does not have permission to access the service or the service cannot be found.");
        } finally {
            tryConnectService(DELAY_MILLIS);
        }
        return true;
    };

    private void startKeepConnectHandlerThread() {
        if (!keepConnectHandlerThread.isAlive()) {
            keepConnectHandlerThread.start();
        }
    }

    private void tryConnectService(long delayMillis) {
        try {
            if (mIBinder != null && !mIBinder.pingBinder()) {
                Log.d(TAG, "tryConnectService: link to death");
                stopConnect();
            }
        } catch (NoSuchElementException e) {
            Log.e(TAG, "the given recipient has not been registered with the IBinder, and the IBinder is still alive.");
        } catch (NullPointerException e) {
            Log.e(TAG, "unlinkToDeath NullPointer - " + e);
        }
        Log.d(TAG, "tryConnectService");
        keepConnectHandler.removeMessages(MSG_CONNECT_SERVICE);
        Message message = Message.obtain();
        message.what = MSG_CONNECT_SERVICE;
        keepConnectHandler.sendMessageDelayed(message, delayMillis);
    }

    public String getPackageName() {
        return packageName;
    }

}