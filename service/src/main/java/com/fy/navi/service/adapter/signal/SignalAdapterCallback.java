package com.fy.navi.service.adapter.signal;

public interface SignalAdapterCallback {
    default void onSpeedChanged(float speed) {}

    default void onGearChanged(int gear) {}

    default void onSystemStateChanged(int state) {}
}
