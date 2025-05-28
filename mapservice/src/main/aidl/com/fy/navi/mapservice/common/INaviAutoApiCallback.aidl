package com.fy.navi.mapservice.common;

interface INaviAutoApiCallback {

    void onPanelData(int panelDataStatus);

    void onSpeedLimitChange(int curSpeed, int limitSpeed);

    void onTurnInfoChange(String turnInfo);

}