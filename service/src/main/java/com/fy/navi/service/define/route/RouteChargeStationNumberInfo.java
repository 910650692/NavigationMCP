package com.fy.navi.service.define.route;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteChargeStationNumberInfo implements Serializable {
    private String mTotalNumber;
    private String mMinPower;
    private String mMaxPower;

    public RouteChargeStationNumberInfo() {
        this.mTotalNumber = "";
        this.mMinPower = "";
        this.mMaxPower = "";
    }

    public RouteChargeStationNumberInfo(final String totalNumberLiteObj, final String minPowerLiteObj, final String maxPowerLiteObj) {
        this.mTotalNumber = totalNumberLiteObj;
        this.mMinPower = minPowerLiteObj;
        this.mMaxPower = maxPowerLiteObj;
    }
}