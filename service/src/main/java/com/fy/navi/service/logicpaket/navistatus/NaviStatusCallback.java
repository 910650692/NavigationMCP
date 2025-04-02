package com.fy.navi.service.logicpaket.navistatus;

public interface NaviStatusCallback {
    /**
     * @param naviStatus the status of the navigation
     */
    void onNaviStatusChange(String naviStatus);
}
