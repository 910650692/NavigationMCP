package com.fy.navi.service.adapter.signal;

public interface SignalAdapterCallback {
    /**
     * 车速变化
     * @param speed
     */
    default void onSpeedChanged(float speed) {

    }

    /**
     * 档位变化
     * @param gear
     */
    default void onGearChanged(int gear) {

    }

    /**
     * 车辆状态变化
     * @param state
     */
    default void onSystemStateChanged(int state) {

    }
}
