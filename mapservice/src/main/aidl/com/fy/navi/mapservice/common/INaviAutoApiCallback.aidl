package com.fy.navi.mapservice.common;

interface INaviAutoApiCallback {

    void onTurnInfoChange(String turnInfo);

    void onNaviArrival();

    void onNaviStop();

}