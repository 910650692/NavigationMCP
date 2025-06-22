package com.sgm.navi.service.define.user.carconnect;

import com.sgm.navi.service.define.user.msgpush.PropertyValueInfo;

import java.util.ArrayList;

public class CarConnectResponseBaseBean {
    // 请求类型
    private int mEAosRequestType;
    // 网络错误码
    private int mNetErrorCode;
    // 网络请求的状态
    private int mNetworkStatus;
    // 请求句柄
    private long mReqHandle;
    // http ack code;如 HTTP 200 OK
    private int mHttpAckCode;
    // :"错误码", code ：1,表示成功，其他值为异常值，无法穷举，每个业务有自己的定义。
    private int mCode;
    // 服务器时间戳
    private String mTimestamp;
    // 错误描述
    private String message;
    // 服务版本
    private String mVersion;
    // "true/false,是否成功
    private String mResult;
    // http header
    private ArrayList<PropertyValueInfo> mHeaders;

    /**
     * getEAosRequestType
     * @return int
     */
    public int getEAosRequestType() {
        return mEAosRequestType;
    }

    /**
     * setEAosRequestType
     * @param eaosRequestType
     */
    public void setEAosRequestType(final int eaosRequestType) {
        this.mEAosRequestType = eaosRequestType;
    }

    /**
     * getNetErrorCode
     * @return int
     */
    public int getNetErrorCode() {
        return mNetErrorCode;
    }

    /**
     * setNetErrorCode
     * @param netErrorCode
     */
    public void setNetErrorCode(final int netErrorCode) {
        this.mNetErrorCode = netErrorCode;
    }

    /**
     * getNetworkStatus
     * @return int
     */
    public int getNetworkStatus() {
        return mNetworkStatus;
    }

    /**
     * setNetworkStatus
     * @param networkStatus
     */
    public void setNetworkStatus(final int networkStatus) {
        this.mNetworkStatus = networkStatus;
    }

    /**
     * getReqHandle
     * @return longg
     */
    public long getReqHandle() {
        return mReqHandle;
    }

    /**
     * setReqHandle
     * @param reqHandle
     */
    public void setReqHandle(final long reqHandle) {
        this.mReqHandle = reqHandle;
    }

    /**
     * getHttpAckCode
     * @return int
     */
    public int getHttpAckCode() {
        return mHttpAckCode;
    }

    /**
     * setHttpAckCode
     * @param httpAckCode
     */
    public void setHttpAckCode(final int httpAckCode) {
        this.mHttpAckCode = httpAckCode;
    }

    public int getCode() {
        return mCode;
    }

    public void setCode(final int code) {
        this.mCode = code;
    }

    public String getTimestamp() {
        return mTimestamp;
    }

    public void setTimestamp(final String timestamp) {
        this.mTimestamp = timestamp;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(final String message) {
        this.message = message;
    }

    public String getVersion() {
        return mVersion;
    }

    public void setVersion(final String version) {
        this.mVersion = version;
    }

    public String getResult() {
        return mResult;
    }

    public void setResult(final String result) {
        this.mResult = result;
    }

    public ArrayList<PropertyValueInfo> getHeaders() {
        return mHeaders;
    }

    public void setHeaders(final ArrayList<PropertyValueInfo> headers) {
        this.mHeaders = headers;
    }
}
