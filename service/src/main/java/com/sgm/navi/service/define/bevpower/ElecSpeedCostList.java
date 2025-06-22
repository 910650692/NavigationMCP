package com.sgm.navi.service.define.bevpower;

import java.io.Serializable;

public class ElecSpeedCostList implements Serializable {
    public int speed;
    public float costValue;

    public ElecSpeedCostList() {
        this.speed = 0;
        this.costValue = 0.0F;
    }

    public ElecSpeedCostList(int speedLiteObj, float costValueLiteObj) {
        this.speed = speedLiteObj;
        this.costValue = costValueLiteObj;
    }
}
