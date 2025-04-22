package com.fy.navi.service.logicpaket.engine;

public interface IActivateObserver {

    /**
     * 正在激活
     */
    void onActivating();

    /**
     * 网络激活失败
     * @param failedCount 网络激活失败次数
     */
    void onNetActivateFailed(int failedCount);

}
