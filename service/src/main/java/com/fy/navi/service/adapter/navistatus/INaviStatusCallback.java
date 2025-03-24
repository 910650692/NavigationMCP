package com.fy.navi.service.adapter.navistatus;

public interface INaviStatusCallback {
    /**
     * 导航状态回调
     * @param naviStatus 导航状态
     * */
    void onNaviStatusChange(String naviStatus);
}
