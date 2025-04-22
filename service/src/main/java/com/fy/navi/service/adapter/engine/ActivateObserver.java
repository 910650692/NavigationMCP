package com.fy.navi.service.adapter.engine;

public interface ActivateObserver {

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
