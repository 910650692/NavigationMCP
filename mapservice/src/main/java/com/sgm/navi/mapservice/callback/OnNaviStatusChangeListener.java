package com.sgm.navi.mapservice.callback;

public interface OnNaviStatusChangeListener {

    /**
     * Map状态改变.
     *
     * @param status String, 见INaviConstant.NaviStatusType.
     */
    void onNaviStatusChange(String status);

    /**
     * 开启导航五分钟后通知
     */
    void onNaviStartAfterFiveMinutes();

    /**
     * 主动停止导航通知
     */
    void onNaviManualStop();
}
