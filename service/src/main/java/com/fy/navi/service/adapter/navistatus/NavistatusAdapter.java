package com.fy.navi.service.adapter.navistatus;


import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.navistatus.NaviStatus;

import java.util.ArrayList;

public final class NavistatusAdapter {
    private boolean mIsGuidanceActive = false;
    private String mNaviStatus = NaviStatus.NaviStatusType.NO_STATUS;
    private String mLastNaviStatus = NaviStatus.NaviStatusType.NO_STATUS;
    private ArrayList<INaviStatusCallback> mNaviStatusCallbacks = new ArrayList<>();

    private NavistatusAdapter() {
    }
    /**
     * 注册回调
     * @param resultCallback 回调
     * */
    public void registerCallback(final INaviStatusCallback resultCallback) {
        if (!mNaviStatusCallbacks.contains(resultCallback)) {
            mNaviStatusCallbacks.add(resultCallback);
        }
    }
    /**
     * 反注册回调
     * @param resultCallback 回调
     * */
    public void unRegisterCallback(final INaviStatusCallback resultCallback) {
        mNaviStatusCallbacks.remove(resultCallback);
    }
    /**
     * 设置导航状态
     * @param naviStatus 回调
     * */
    @NaviStatus.NaviStatusType
    public void setNaviStatus(final String naviStatus) {
        if (naviStatus == NaviStatus.NaviStatusType.NAVING) {
            mIsGuidanceActive = true;
        }

        if (naviStatus == NaviStatus.NaviStatusType.NO_STATUS) {
            mIsGuidanceActive = false;
        }
        synchronized (NavistatusAdapter.class) {
            this.mLastNaviStatus = this.mNaviStatus;
            this.mNaviStatus = String.valueOf(naviStatus);
            ThreadManager.getInstance().postDelay(()->{
                for (INaviStatusCallback callback : mNaviStatusCallbacks) {
                    if (callback != null) {
                        callback.onNaviStatusChange(this.mNaviStatus);
                    }
                }
            },0);
        }
    }

    public String getCurrentNaviStatus() {
        return this.mNaviStatus;
    }

    public String getLastNaviStatus() {
        return this.mLastNaviStatus;
    }

    public boolean isGuidanceActive() {
        return this.mIsGuidanceActive;
    }

    public static NavistatusAdapter getInstance() {
        return Helper.RA;
    }

    private static final class Helper {
        private static final NavistatusAdapter RA = new NavistatusAdapter();
    }
}
