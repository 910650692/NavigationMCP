package com.fy.navi.service.define.route;

import java.io.Serializable;

public class RouteAlterChargePriceInfo implements Serializable {
    public String lowestPriceValue;
    public String lowestPriceUnit;

    public RouteAlterChargePriceInfo() {
        this.lowestPriceValue = "";
        this.lowestPriceUnit = "";
    }

    public RouteAlterChargePriceInfo(String lowestPriceValueLiteObj, String lowestPriceUnitLiteObj) {
        this.lowestPriceValue = lowestPriceValueLiteObj;
        this.lowestPriceUnit = lowestPriceUnitLiteObj;
    }
}