package com.sgm.navi.service.define.user.usertrack;

import java.util.ArrayList;


public class FootprintResultInfo {

    private int mCode;
    private String mMessage;

    private int mCurSwitch = 2;

    private boolean mIsLogin;
    private SummaryModuleCardInfo mCity;
    private SummaryModuleCardInfo mDriver;
    private SummaryModuleCardInfo mPoint;

    private ArrayList<FootprintNaviRecordInfo> mRecord;

    public boolean isLogin() {
        return mIsLogin;
    }

    public void setLogin(final boolean login) {
        mIsLogin = login;
    }

    public int getCurSwitch() {
        return mCurSwitch;
    }

    public void setCurSwitch(final int curSwitch) {
        this.mCurSwitch = curSwitch;
    }

    public int getCode() {
        return mCode;
    }

    public void setCode(final int code) {
        this.mCode = code;
    }

    public String getMessage() {
        return mMessage;
    }

    public void setMessage(final String message) {
        this.mMessage = message;
    }

    public SummaryModuleCardInfo getCity() {
        return mCity;
    }

    public void setCity(final SummaryModuleCardInfo city) {
        this.mCity = city;
    }

    public SummaryModuleCardInfo getDriver() {
        return mDriver;
    }

    public void setDriver(final SummaryModuleCardInfo driver) {
        this.mDriver = driver;
    }

    public SummaryModuleCardInfo getPoint() {
        return mPoint;
    }

    public void setPoint(final SummaryModuleCardInfo point) {
        this.mPoint = point;
    }

    public ArrayList<FootprintNaviRecordInfo> getRecord() {
        return mRecord;
    }

    public void setRecord(final ArrayList<FootprintNaviRecordInfo> record) {
        this.mRecord = record;
    }
}
