package com.fy.navi.service.adapter.signal;

public interface SignalAdapterCallback {
    /**
     * 车速变化 km/h
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

    /**
     * 续航里程
     * @param value 单位km
     */
    default void onRangeRemainingSignalChanged(float value) {

    }

    /**
     * 高压电池续航里程
     * @param value 单位km
     */
    default void onHighVoltageBatteryPropulsionRangeChanged(float value) {

    }

    /**
     * NOP状态变化
     * @param state 状态
     */
    default void onLaneCenteringWarningIndicationRequestIdcmAChanged(int state) {

    }

    /**
     * L2++ NOP播报状态变化
     * @param state 状态
     */
    default void onNaviOnADASStateChanged(int state) {

    }

    /**
     * 导航音量变化
     *
     * @param volume 0-63
     */
    default void onNaviVolumeChanged(int volume) {

    }

    default void onFuelLevelPercentSignalChanged(Float value) {

    }
}
