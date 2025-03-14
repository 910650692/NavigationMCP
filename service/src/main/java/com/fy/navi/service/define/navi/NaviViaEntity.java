package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;
import com.fy.navi.service.define.route.ChargingInfo;

public class NaviViaEntity {
    private String pid;
    private GeoPoint realPos; // 实际位置坐标(必选)
    private String name; // POI名称(可选)
    private String address; // 地址
    private ChargingInfo chargeInfo; // 充电桩信息
    private String distance;
    private String arriveTime;
    private String arriveDay;

    public String getArriveDay() {
        return arriveDay;
    }

    public void setArriveDay(String arriveDay) {
        this.arriveDay = arriveDay;
    }

    public GeoPoint getRealPos() {
        return realPos;
    }

    public void setRealPos(GeoPoint realPos) {
        this.realPos = realPos;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public ChargingInfo getChargeInfo() {
        return chargeInfo;
    }

    public void setChargeInfo(ChargingInfo chargeInfo) {
        this.chargeInfo = chargeInfo;
    }

    public String getDistance() {
        return distance;
    }

    public void setDistance(String distance) {
        this.distance = distance;
    }

    public String getArriveTime() {
        return arriveTime;
    }

    public void setArriveTime(String arriveTime) {
        this.arriveTime = arriveTime;
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }
}
