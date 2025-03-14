package com.fy.navi.service.adapter.navistatus;


import com.android.utils.thread.ThreadManager;
import com.fy.navi.service.define.navistatus.NaviStatus;

import java.util.ArrayList;


public class NavistatusAdapter {

    private boolean initFlag = false;
    private boolean isGuidanceActive = false;
    private String naviStatus = NaviStatus.NaviStatusType.NO_STATUS;
    private String last_naviStatus = NaviStatus.NaviStatusType.NO_STATUS;
    private ArrayList<INaviStatusCallback> naviStatusCallbacks = new ArrayList<>();

    private NavistatusAdapter() {
    }

    public void registerCallback(INaviStatusCallback resultCallback) {
        if (!naviStatusCallbacks.contains(resultCallback)) {
            naviStatusCallbacks.add(resultCallback);
        }
    }

    public void unRegisterCallback(INaviStatusCallback resultCallback) {
        naviStatusCallbacks.remove(resultCallback);
    }

    @NaviStatus.NaviStatusType
    public void setNaviStatus(String naviStatus) {
        if (naviStatus == NaviStatus.NaviStatusType.NAVING) {
            isGuidanceActive = true;
        }

        if (naviStatus == NaviStatus.NaviStatusType.NO_STATUS) {
            isGuidanceActive = false;
        }
        synchronized (NavistatusAdapter.class) {
            this.last_naviStatus = this.naviStatus;
            this.naviStatus = String.valueOf(naviStatus);
            ThreadManager.getInstance().postDelay(()->{
                for (INaviStatusCallback callback : naviStatusCallbacks) {
                    if (callback != null) {
                        callback.onNaviStatusChange(this.naviStatus);
                    }
                }
            },0);
        }
    }

    public String getCurrentNaviStatus() {
        return this.naviStatus;
    }

    public String getLastNaviStatus() {
        return this.last_naviStatus;
    }

    public boolean isGuidanceActive() {
        return this.isGuidanceActive;
    }


    public void backToLastNaviStatus() {
        if (!this.naviStatus.equals(this.last_naviStatus)) {
            synchronized (NavistatusAdapter.class) {
                String tmp = this.last_naviStatus;
                this.last_naviStatus = this.naviStatus;
                this.naviStatus = tmp;
                ThreadManager.getInstance().postDelay(() -> {
                    for (INaviStatusCallback callback : naviStatusCallbacks) {
                        if (callback != null) {
                            callback.onNaviStatusChange(this.naviStatus);
                        }
                    }
                }, 0);
            }
        }
    }

    public static NavistatusAdapter getInstance() {
        return Helper.ra;
    }

    private static final class Helper {
        private static final NavistatusAdapter ra = new NavistatusAdapter();
    }
}
