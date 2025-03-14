package com.fy.navi.service.logicpaket.navistatus;

import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;

import java.util.Hashtable;


public class NaviStatusPackage implements INaviStatusCallback {

    private NavistatusAdapter mNavistatusAdapter;
    private Hashtable<String, NaviStatusCallback> naviStatusCallbackMap;

    public static NaviStatusPackage getInstance() {
        return NaviStatusPackage.Helper.lPackage;
    }

    private static final class Helper {
        private static final NaviStatusPackage lPackage = new NaviStatusPackage();
    }

    private NaviStatusPackage() {
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mNavistatusAdapter.registerCallback(NaviStatusPackage.this);
        naviStatusCallbackMap = new Hashtable<>();
    }

    public void registerObserver(String key, NaviStatusCallback naviStatusCallback) {
        naviStatusCallbackMap.put(key, naviStatusCallback);
    }

    public void unregisterObserver(String key) {
        naviStatusCallbackMap.remove(key);
    }

    public String getCurrentNaviStatus() {
        return this.mNavistatusAdapter.getCurrentNaviStatus();
    }

    public String getLastNaviStatus() {
        return this.mNavistatusAdapter.getLastNaviStatus();
    }

    public boolean isGuidanceActive() {
        return this.mNavistatusAdapter.isGuidanceActive();
    }

    @Override
    public void onNaviStatusChange(String naviStatus) {
        for (NaviStatusCallback callback : naviStatusCallbackMap.values()) {
            if (null != callback) {
                callback.onNaviStatusChange(naviStatus);
            }
        }
    }

}
