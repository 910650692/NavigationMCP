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
    private int mBizType = 0;
    //停车位置的格式 json格式
    private String mData = "";

    /**
     * getmEAosRequestType
     * @return int
     */
    public int getmEAosRequestType() {
        return mEAosRequestType;
    }

    /**
     * setmEAosRequestType
     * @param aosRequestType
     */
    public void setmEAosRequestType(final int aosRequestType) {
        this.mEAosRequestType = aosRequestType;
    }

    /**
     * getmEReqProtol
     * @return int
     */
    public int getmEReqProtol() {
        return mEReqProtol;
    }

    /**
     * setmEReqProtol
     * @param reqProtol
     */
    public void setmEReqProtol(final int reqProtol) {
        this.mEReqProtol = reqProtol;
    }

    /**
     * getmEReqMethod
     * @return int
     */
    public int getEReqMethod() {
        return mEReqMethod;
    }

    /**
     * setmEReqMethod
     * @param reqMethod
     */
    public void setmEReqMethod(final int reqMethod) {
        this.mEReqMethod = reqMethod;
    }

    /**
     * getmTimeOut
     * @return long
     */
    public long getmTimeOut() {
        return mTimeOut;
    }

    /**
     * setmTimeOut
     * @param timeOut
     */
    public void setmTimeOut(final long timeOut) {
        this.mTimeOut = timeOut;
    }

    /**
     * getmGroup
     * @return long
     */
    public long getmGroup() {
        return mGroup;
    }

    /**
     * setmGroup
     * @param group
     */
    public void setmGroup(final long group) {
        this.mGroup = group;
    }

    public int getBizType() {
        return mBizType;
    }

    public void setBizType(final int bizType) {
        this.mBizType = bizType;
    }

    public String getData() {
        return mData;
    }

    public void setData(final String mData) {
        this.mData = mData;
    }
}
