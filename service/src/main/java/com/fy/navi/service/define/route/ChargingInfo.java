package com.fy.navi.service.define.route;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class ChargingInfo {
    private short mType; // 0非法值 1充电站 2非充电站 3自定义充电站
    private short mMinArrivalPercent; // 到达途经点/终点最低电量百分比，单位：%
    private long mPower; // 充电站功率，单位kW
    private long mVoltage; // 充电站电压，单位V
    private long mAmperage; // 充电站电流，单位A

    @Override
    public String toString() {
        return "ChargingInfo{" +
                "type=" + mType +
                ", minArrivalPercent=" + mMinArrivalPercent +
                ", power=" + mPower +
                ", voltage=" + mVoltage +
                ", amperage=" + mAmperage +
                '}';
    }
}
