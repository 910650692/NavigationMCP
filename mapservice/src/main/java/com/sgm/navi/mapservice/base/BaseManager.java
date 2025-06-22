package com.sgm.navi.mapservice.base;

import android.os.IInterface;
import android.os.RemoteException;

public abstract class BaseManager<B extends IInterface> {

    protected static final String TAG = BaseManager.class.getSimpleName();

    public BaseManager() {

    }

    protected abstract boolean checkBinder();

    protected abstract B updateBinder() throws RemoteException;

    protected abstract void registerBinderResultCallback() throws RemoteException;

    protected abstract void unRegisterBinderResultCallback() throws RemoteException;

    protected abstract void onEngineInit(boolean success);

}
