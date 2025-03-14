package com.fy.navi.service.define.user.usertrack;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/27
 */
public class FootprintResultInfo {

    public int code;
    public String message;

    public int curSwitch = 2;

    public boolean isLogin;
    public SummaryModuleCardInfo city;
    public SummaryModuleCardInfo driver;
    public SummaryModuleCardInfo point;

    public ArrayList<FootprintNaviRecordInfo> record;

    public boolean isLogin() {
        return isLogin;
    }

    public void setLogin(boolean login) {
        isLogin = login;
    }

    public int getCurSwitch() {
        return curSwitch;
    }

    public void setCurSwitch(int curSwitch) {
        this.curSwitch = curSwitch;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public SummaryModuleCardInfo getCity() {
        return city;
    }

    public void setCity(SummaryModuleCardInfo city) {
        this.city = city;
    }

    public SummaryModuleCardInfo getDriver() {
        return driver;
    }

    public void setDriver(SummaryModuleCardInfo driver) {
        this.driver = driver;
    }

    public SummaryModuleCardInfo getPoint() {
        return point;
    }

    public void setPoint(SummaryModuleCardInfo point) {
        this.point = point;
    }

    public ArrayList<FootprintNaviRecordInfo> getRecord() {
        return record;
    }

    public void setRecord(ArrayList<FootprintNaviRecordInfo> record) {
        this.record = record;
    }
}
