package com.sgm.navi.service.define.route;

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
    private boolean isAutoAdd; // 如果是补能规划添加的设为 true
    private long chargeTime;// 需要充电时常，单位秒
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
