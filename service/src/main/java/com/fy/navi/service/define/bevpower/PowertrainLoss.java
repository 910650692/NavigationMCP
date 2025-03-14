package com.fy.navi.service.define.bevpower;

import java.io.Serializable;

public class PowertrainLoss implements Serializable {
    public float powerdemand;
    public float costValue;

    public PowertrainLoss() {
        this.powerdemand = 0.0F;
        this.costValue = 0.0F;
    }

    public PowertrainLoss(float powerdemandLiteObj, float costValueLiteObj) {
        this.powerdemand = powerdemandLiteObj;
        this.costValue = costValueLiteObj;
    }
}
