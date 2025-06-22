package com.sgm.navi.mapservice.callback;

public interface OnSpeedLimitChangeListener {

    /**
     * 当前道路限速回调.
     *
     * @param curSpeed 当前车速，单位km/h
     * @param limitSpeed 道路限速，单位km/h，为0不限速.
     */
    void onSpeedLimitChange(int curSpeed, int limitSpeed);
}
