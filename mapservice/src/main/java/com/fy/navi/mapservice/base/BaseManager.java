package com.fy.navi.mapservice.base;

import android.os.IBinder;
import android.os.IInterface;
import android.os.RemoteException;
import android.util.Log;

import com.fy.navi.mapservice.apimanager.IEngineStatusCallback;

public abstract class BaseManager<B extends IInterface> {

    protected static final String TAG = BaseManager.class.getSimpleName();

    protected B mBinder = null;

    public BaseManager() {}


    protected final IEngineStatusCallback mEngineStatusCallback = new IEngineStatusCallback() {
        @Override
        public void onInitSuccess() {
            try {
                mBinder = updateBinder();
                mBinder.asBinder().linkToDeath(deathRecipient, 0);
                unRegisterBinderResultCallback();
                registerBinderResultCallback();
                onEngineInit(true);
            } catch (RemoteException e) {
                Log.e(TAG, "onUpdateBinderCallback remoteException: " + e);
            }
        }

        @Override
        public void onInitFailed() {
            if (null != mBinder) {
                mBinder.asBinder().unlinkToDeath(deathRecipient, 0);
                mBinder = null;
            }
            onEngineInit(false);
        }
    };

    private final IBinder.DeathRecipient deathRecipient = new IBinder.DeathRecipient() {
        @Override
        public void binderDied() {
            if (mBinder == null) {
                return;
            }
            mBinder.asBinder().unlinkToDeath(this, 0);
            mBinder = null;
        }
    };

    protected boolean checkBinder() {
        return null != mBinder && mBinder.asBinder().pingBinder();
    }

    protected abstract B updateBinder() throws RemoteException;

    protected abstract void registerBinderResultCallback() throws RemoteException;

    protected abstract void unRegisterBinderResultCallback() throws RemoteException;

    protected abstract void onEngineInit(boolean success);

}
