package com.fy.navi.service.adapter.position.bls.listener;

import com.autonavi.gbl.pos.model.LocGnss;
import com.autonavi.gbl.pos.model.LocSignData;
import com.fy.navi.service.define.position.LocGpgsvWrapper;


public interface ILocBackFusionDataSource {
    void init();

    void unInit();

    void setDrBackFusionEnable(boolean enable);

    void setRecordEnable(boolean enable);

    /*高德标定结果信息*/
    void updateSensorPara(String s);

    /*脉冲车速*/
    void onPulseSpeedChanged(float speed);

    /*挡位变化*/
    void onGearChanged(int gear);

    interface ILocBackFusionDataObserver {
        /*陀螺仪信号 10HZ*/
        void onLocGyroInfo(LocSignData locSignData);

        /*加速度计信号 10HZ*/
        void onLocAcce3dInfo(LocSignData locSignData);

        /*车速脉冲信号 10HZ*/
        void onLocPulseInfo(LocSignData locSignData);

        /*GNSS信号 1HZ*/
        void onGpsInfo(LocGnss locGnss);

        /*星历信号 1HZ 要求频率1s一次*/
        void onGSVInfo(LocGpgsvWrapper wrapper);
    }
}
