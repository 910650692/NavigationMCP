package com.sgm.navi.service.define.user.account;

public class UserCarBindInfo {
    public String model;
    public String device;
    public String manufacture;

    public UserCarBindInfo() {
        this.model = "";
        this.device = "";
        this.manufacture = "";
    }

    public UserCarBindInfo(String modelLiteObj, String deviceLiteObj, String manufactureLiteObj) {
        this.model = modelLiteObj;
        this.device = deviceLiteObj;
        this.manufacture = manufactureLiteObj;
    }
}
