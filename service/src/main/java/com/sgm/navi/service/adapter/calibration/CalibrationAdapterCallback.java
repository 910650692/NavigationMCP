package com.sgm.navi.service.adapter.calibration;

public interface CalibrationAdapterCallback {
    /**
     * hud雪地模式开关变化
     * @param snowMode 状态
     */
    default void onHudSnowModeChanged(boolean snowMode) {

    }
}
