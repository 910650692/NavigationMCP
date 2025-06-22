package com.sgm.navi.service.define.route;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlterChargePriceInfo implements Serializable {
    private String mLowestPriceValue;
    private String mLowestPriceUnit;

    public RouteAlterChargePriceInfo() {
        this.mLowestPriceValue = "";
        this.mLowestPriceUnit = "";
    }

    public RouteAlterChargePriceInfo(final String lowestPriceValueLiteObj, final String lowestPriceUnitLiteObj) {
        this.mLowestPriceValue = lowestPriceValueLiteObj;
        this.mLowestPriceUnit = lowestPriceUnitLiteObj;
    }
}