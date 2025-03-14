package com.fy.navi.service.define.route;

import java.io.Serializable;

public class RouteChargeStationNumberInfo implements Serializable {
    public String totalNumber;
    public String minPower;
    public String maxPower;

    public RouteChargeStationNumberInfo() {
        this.totalNumber = "";
        this.minPower = "";
        this.maxPower = "";
    }

    public RouteChargeStationNumberInfo(String totalNumberLiteObj, String minPowerLiteObj, String maxPowerLiteObj) {
        this.totalNumber = totalNumberLiteObj;
        this.minPower = minPowerLiteObj;
        this.maxPower = maxPowerLiteObj;
    }
}