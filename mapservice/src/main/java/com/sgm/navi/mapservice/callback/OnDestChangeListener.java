package com.sgm.navi.mapservice.callback;

public interface OnDestChangeListener {
    /**
     * 目的地变更通知回调（开始导航和导航途中变更两种情况）
     *
     * @param destInfo 目的地信息
     */
    void onDestChangeListener(String destInfo);
}
