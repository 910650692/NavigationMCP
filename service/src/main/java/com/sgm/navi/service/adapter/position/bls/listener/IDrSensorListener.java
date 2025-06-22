package com.sgm.navi.service.adapter.position.bls.listener;

import com.autonavi.gbl.pos.model.LocSignData;


public interface IDrSensorListener {
    /*陀螺仪信号 10HZ*/
    void onLocGyroInfo(LocSignData locSignData);

    /*加速度计信号 10HZ*/
    void onLocAcce3dInfo(LocSignData locSignData);

    /*车速脉冲信号 10HZ*/
    void onLocPulseInfo(LocSignData locSignData);

}
