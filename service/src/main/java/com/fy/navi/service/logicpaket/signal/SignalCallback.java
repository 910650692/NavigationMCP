package com.fy.navi.service.logicpaket.signal;

public interface SignalCallback {
    /**
     * 车速变化(频率500ms)
     * @param speed 单位m/s
     */
    default void onSpeedChanged(float speed) {

    }

    /**
     * 挡位变化
     * @param gear 挡位
     * PARKED = 0;停车
     * NEUTRAL = 1;空档
     * FORWARD = 2;前进
     * REVERSE = 3;倒车
     * INVALID = 4;无效
     */
    default void onGearChanged(int gear) {

    }

    /**
     * 系统状态
     * @param state 系统状态
     * OFF = 0
     * ACC = 1
     * RUN = 2
     * CRANK(START) = 3
     * SLEEP = 4
     */
    default void onSystemStateChanged(int state) {

    }
}
