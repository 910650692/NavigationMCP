package com.fy.navi.service.adapter.position.bls.listener;

import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSpeedometer;
import com.fy.navi.service.define.position.LocGpgsvWrapper;

public interface ILocationListener {

    /*GNSS信号 1HZ*/
    void onGpsInfo(LocGnss locGnss);

    /*星历信号 1HZ*/
    void onGSVInfo(LocGpgsvWrapper wrapper);

    /*仪表车速 1HZ*/
    void onLocMeterInfo(LocSpeedometer locSpeedometer);

    /*星历数据*/
    void onSatelliteNum(int num);

    /*GPS 超时*/
    void onGpsCheckTimeOut();
}
