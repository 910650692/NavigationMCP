package com.fy.navi.mapservice.base;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.HandlerThread;
import android.os.IBinder;
import android.os.Message;
import android.os.RemoteException;
import android.util.Log;

import com.fy.navi.mapservice.IBinderPool;

import java.lang.ref.WeakReference;
import java.util.NoSuchElementException;

public final class MapSdk {

    private static final String TAG = MapSdk.class.getSimpleName();

    private final int mConnectMsg = 0x00000;
    private final HandlerThread mKeepThread;
    private final Handler mKeepHandle;

    private WeakReference<Context> mContextReference;
    private String mClientPkg;
    private IBinder mIBinder;
    private boolean mConnected;

    private final static class SingletonHolder {
        private static final MapSdk INSTANCE = new MapSdk();
    }

    private MapSdk() {
        mKeepThread = new HandlerThread(getClass().getSimpleName());
        startKeepConnectHandlerThread();
        final Handler.Callback handlerCallback = message -> {
            if (mIBinder != null && mIBinder.pingBinder()) {
                Log.i(TAG, "Service connected");
                return true;
            }
            final Intent intent = new Intent();
            intent.setClassName("com.fy.navi.hmi", "com.fy.navi.exportservice.MapService");
            final Bundle bundle = new Bundle();
            bundle.putString("packageName", getPackageName());
            intent.putExtras(bundle);
            try {
                final Context context = mContextReference.get();
                if (null != context) {
                    context.bindService(intent, mServiceConnection, Context.BIND_AUTO_CREATE);
                }
            } catch (SecurityException e) {
                Log.e(TAG, "The caller does not have permission to access the service or the service cannot be found.");
            } finally {
                tryConnectService(2000);
            }
            return true;
        };
        mKeepHandle = new Handler(mKeepThread.getLooper(), handlerCallback);
    }

    public static MapSdk getInstance() {
        return SingletonHolder.INSTANCE;
    }

    /**
     * 开始绑定远程服务.
     *
     * @param context Context.
     */
    public void startConnect(final Context context) {
        if (mConnected) {
            Log.d(TAG, "MapSdk already connected");
        } else {
            Log.d(TAG, "MapSdk startConnect");
            mContextReference = new WeakReference<>(context);
            mClientPkg = context.getPackageName();
            tryConnectService(0);
        }
    }

    /**
     * 取消连接.
     */
    private void stopConnect() {
        BinderManager.getInstance().removeBinderPool();
        if (mIBinder != null) {
            mIBinder.unlinkToDeath(mDeathRecipient, 0);
            mIBinder = null;
        }
    }

    /**
     * 与service连接和断开连接时回调
     */
    private final ServiceConnection mServiceConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(final ComponentName name, final IBinder binder) {
            Log.d(TAG, "onServiceConnected");
            try {
                mConnected = true;
                mIBinder = binder;
                mIBinder.linkToDeath(mDeathRecipient, 0);
                mKeepHandle.removeMessages(mConnectMsg);
                BinderManager.getInstance().updateBinderPool(IBinderPool.Stub.asInterface(binder));
            } catch (RemoteException e) {
                Log.e(TAG, "The target IBinder's process has already died: " + e);
                tryConnectService(0);
            }
        }

        @Override
        public void onServiceDisconnected(final ComponentName name) {
            Log.d(TAG, "onServiceDisconnected");
            mConnected = false;
            tryConnectService(500);
        }
    };

    /**
     * 绑定的service死亡时回调
     */
    private final IBinder.DeathRecipient mDeathRecipient = () -> {
        Log.w(TAG, "binderDied callback");
        tryConnectService(500);
    };

    /**
     * 启动保持连接的线程.
     */
    private void startKeepConnectHandlerThread() {
        if (!mKeepThread.isAlive()) {
            mKeepThread.start();
        }
    }

    /**
     * 尝试绑定远程服务.
     *
     * @param delayMillis long,延迟毫秒.
     */
    private void tryConnectService(final long delayMillis) {
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
        mKeepHandle.removeMessages(mConnectMsg);
        final Message message = Message.obtain();
        message.what = mConnectMsg;
        mKeepHandle.sendMessageDelayed(message, delayMillis);
    }

    public String getPackageName() {
        return mClientPkg;
    }

}