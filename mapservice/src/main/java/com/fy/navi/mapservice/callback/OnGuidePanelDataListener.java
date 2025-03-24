package com.fy.navi.mapservice.callback;

public interface OnGuidePanelDataListener {

    /**
     * 引导面板状态改变.
     *
     * @param panelDataStatus int, 见INaviConstant.GuidePanelStatus.
     */
    void onPanelData(int panelDataStatus);
}
