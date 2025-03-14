package com.fy.navi.service.define.user.msgpush;


public class MsgPushRequestInfo {
    public int mEAosRequestType;
    public int mEReqProtol;
    public int mEReqMethod;
    public long mTimeOut;
    public long mGroup;

    public String message = "";

    public double lon;
    public double lat;
    public String name;
    public String address;

    public String sourceId = "autocpp";
    public String bizTypeFlag = "aimpoi";
    public boolean isReliable = true;
    public int expiration = 1800;

    public String bizType = "aimpoi";
    public String data = "";

    public String getBizTypeFlag() {
        return bizTypeFlag;
    }

    public void setBizTypeFlag(String bizTypeFlag) {
        this.bizTypeFlag = bizTypeFlag;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
    }

    public int getmEAosRequestType() {
        return mEAosRequestType;
    }

    public void setmEAosRequestType(int mEAosRequestType) {
        this.mEAosRequestType = mEAosRequestType;
    }

    public int getmEReqProtol() {
        return mEReqProtol;
    }

    public void setmEReqProtol(int mEReqProtol) {
        this.mEReqProtol = mEReqProtol;
    }

    public int getmEReqMethod() {
        return mEReqMethod;
    }

    public void setmEReqMethod(int mEReqMethod) {
        this.mEReqMethod = mEReqMethod;
    }

    public long getmTimeOut() {
        return mTimeOut;
    }

    public void setmTimeOut(long mTimeOut) {
        this.mTimeOut = mTimeOut;
    }

    public long getmGroup() {
        return mGroup;
    }

    public void setmGroup(long mGroup) {
        this.mGroup = mGroup;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getSourceId() {
        return sourceId;
    }

    public void setSourceId(String sourceId) {
        this.sourceId = sourceId;
    }

    public boolean isReliable() {
        return isReliable;
    }

    public void setReliable(boolean reliable) {
        isReliable = reliable;
    }

    public int getExpiration() {
        return expiration;
    }

    public void setExpiration(int expiration) {
        this.expiration = expiration;
    }
}
