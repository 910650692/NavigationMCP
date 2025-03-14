package com.fy.navi.service.adapter.position.bls.listener;

import com.autonavi.gbl.pos.model.LocSignData;


public interface IDrSensorListener {
    /*陀螺仪*/
    void onLocGyroInfo(LocSignData locGyro, boolean isRaw);

    /*加速度计*/
    void onLocAcce3dInfo(LocSignData locAcce3d, boolean isRaw);

    /*车速脉冲*/
    void onLocPulseInfo(LocSignData locPulse, boolean isRaw);

    void onSensorError(String error);
}
