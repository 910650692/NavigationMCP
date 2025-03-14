package com.fy.navi.service.define.bevpower;

import java.io.Serializable;

public class ElecCommonParameter implements Serializable {
    public float access;
    public float decess;

    public ElecCommonParameter() {
        this.access = 0.0F;
        this.decess = 0.0F;
    }

    public ElecCommonParameter(float accessLiteObj, float decessLiteObj) {
        this.access = accessLiteObj;
        this.decess = decessLiteObj;
    }
}