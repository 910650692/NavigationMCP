package com.fy.navi.service.define.route;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/5
 */
public class ChargingInfo {
    public short type; // 0非法值 1充电站 2非充电站 3自定义充电站
    public short minArrivalPercent; // 到达途经点/终点最低电量百分比，单位：%
    public long power; // 充电站功率，单位kW
    public long voltage; // 充电站电压，单位V
    public long amperage; // 充电站电流，单位A

    public short getType() {
        return type;
    }

    public void setType(short type) {
        this.type = type;
    }

    public short getMinArrivalPercent() {
        return minArrivalPercent;
    }

    public void setMinArrivalPercent(short minArrivalPercent) {
        this.minArrivalPercent = minArrivalPercent;
    }

    public long getPower() {
        return power;
    }

    public void setPower(long power) {
        this.power = power;
    }

    public long getVoltage() {
        return voltage;
    }

    public void setVoltage(long voltage) {
        this.voltage = voltage;
    }

    public long getAmperage() {
        return amperage;
    }

    public void setAmperage(long amperage) {
        this.amperage = amperage;
    }

    @Override
    public String toString() {
        return "ChargingInfo{" +
                "type=" + type +
                ", minArrivalPercent=" + minArrivalPercent +
                ", power=" + power +
                ", voltage=" + voltage +
                ", amperage=" + amperage +
                '}';
    }
}
