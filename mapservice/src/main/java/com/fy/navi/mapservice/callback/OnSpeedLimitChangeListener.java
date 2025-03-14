package com.fy.navi.mapservice.callback;

public interface OnSpeedLimitChangeListener {

    //当前道路限速值，单位km/h
    void onSpeedLimitChange(int curSpeed, int limitSpeed);
}
