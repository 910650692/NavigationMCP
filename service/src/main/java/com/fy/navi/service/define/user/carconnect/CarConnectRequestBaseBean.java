package com.fy.navi.service.define.user.carconnect;


public class CarConnectRequestBaseBean {
    // 除非特殊业务否则使用者不需要设置,每个业务的构造函数中已经设置好
    // 业务类型
    private int mEAosRequestType;
    // http or https协议
    private int mEReqProtol;
    // GET or POST or HEAD
    private int mEReqMethod;
    // 总业务超时，单位毫秒，默认30000毫秒
    private long mTimeOut;
    // 扩展功能，组
    private long mGroup;

    // 停车位置
    private int bizType = 0;
    //停车位置的格式 json格式
    private String data = "";

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

    public int getBizType() {
        return bizType;
    }

    public void setBizType(int bizType) {
        this.bizType = bizType;
    }
}
