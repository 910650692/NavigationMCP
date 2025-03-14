package com.fy.navi.service.adapter.position.bls.source;

import android.content.Context;

import com.fy.navi.service.adapter.position.bls.listener.ILocBackFusionDataSource;


public abstract class BaseLocBackFusionDataSource implements ILocBackFusionDataSource {
    protected final ILocBackFusionDataObserver mFusionDataObserver;
    protected boolean mIsEnable = true;//后端融合是否可用

    public BaseLocBackFusionDataSource(Context context, ILocBackFusionDataObserver observer) {
        mFusionDataObserver = observer;
    }

    @Override
    public synchronized void setDrBackFusionEnable(boolean enable) {
//        LogUtils.i("LocBackFusionDataSource", " setEnable old=" + mIsEnable + " new=" + enable);
        if (mIsEnable != enable) {
            if (enable) {
                onStart();
            } else {
                onStop();
            }
            mIsEnable = enable;
        }
    }

    public void setRecordEnable(boolean enable) {

    }

    abstract protected void onStart();

    abstract protected void onStop();

}
