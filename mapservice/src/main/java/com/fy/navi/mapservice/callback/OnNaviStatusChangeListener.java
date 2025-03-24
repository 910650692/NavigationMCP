package com.fy.navi.mapservice.callback;

public interface OnNaviStatusChangeListener {

    /**
     * Map状态改变.
     *
     * @param status String, 见INaviConstant.NaviStatusType.
     */
    void onNaviStatusChange(String status);
}
