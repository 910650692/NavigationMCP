package com.fy.navi.mapservice.common;

interface INaviAutoApiCallback {

    void onSpeedLimitChange(int curSpeed, int limitSpeed);

    void onTurnInfoChange(String turnInfo);

    void onNaviArrival();

    void onNaviStop();

}