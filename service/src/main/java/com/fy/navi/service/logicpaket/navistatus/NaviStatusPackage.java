package com.fy.navi.service.logicpaket.navistatus;

import com.fy.navi.service.adapter.navistatus.INaviStatusCallback;
import com.fy.navi.service.adapter.navistatus.NavistatusAdapter;

import java.util.Hashtable;


public final class NaviStatusPackage implements INaviStatusCallback {

    private NavistatusAdapter mNavistatusAdapter;
    private Hashtable<String, NaviStatusCallback> mNaviStatusCallbackMap;

    public static NaviStatusPackage getInstance() {
        return NaviStatusPackage.Helper.NAVI_STATUS_PACKAGE;
    }

    private static final class Helper {
        private static final NaviStatusPackage NAVI_STATUS_PACKAGE = new NaviStatusPackage();
    }

    private NaviStatusPackage() {
        mNavistatusAdapter = NavistatusAdapter.getInstance();
        mNavistatusAdapter.registerCallback(NaviStatusPackage.this);
        mNaviStatusCallbackMap = new Hashtable<>();
    }

    /**
     * @param key the key of the observer
     * @param naviStatusCallback the observer
     */
    public void registerObserver(final String key, final NaviStatusCallback naviStatusCallback) {
        mNaviStatusCallbackMap.put(key, naviStatusCallback);
    }

    /**
     * @param key the key of the observer
     */
    public void unregisterObserver(final String key) {
        mNaviStatusCallbackMap.remove(key);
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
    public void onNaviStatusChange(final String naviStatus) {
        for (NaviStatusCallback callback : mNaviStatusCallbackMap.values()) {
            if (null != callback) {
                callback.onNaviStatusChange(naviStatus);
            }
        }
    }

}
