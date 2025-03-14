package com.fy.navi.service.define.user.carconnect;

import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;

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
    private int code;
    // 服务器时间戳
    private String timestamp;
    // 错误描述
    private String message;
    // 服务版本
    private String version;
    // "true/false,是否成功
    private String result;
    // http header
    public ArrayList<PropertyValueInfo> headers;

    public int getmEAosRequestType() {
        return mEAosRequestType;
    }

    public void setmEAosRequestType(int mEAosRequestType) {
        this.mEAosRequestType = mEAosRequestType;
    }

    public int getmNetErrorCode() {
        return mNetErrorCode;
    }

    public void setmNetErrorCode(int mNetErrorCode) {
        this.mNetErrorCode = mNetErrorCode;
    }

    public int getmNetworkStatus() {
        return mNetworkStatus;
    }

    public void setmNetworkStatus(int mNetworkStatus) {
        this.mNetworkStatus = mNetworkStatus;
    }

    public long getmReqHandle() {
        return mReqHandle;
    }

    public void setmReqHandle(long mReqHandle) {
        this.mReqHandle = mReqHandle;
    }

    public int getmHttpAckCode() {
        return mHttpAckCode;
    }

    public void setmHttpAckCode(int mHttpAckCode) {
        this.mHttpAckCode = mHttpAckCode;
    }

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public ArrayList<PropertyValueInfo> getHeaders() {
        return headers;
    }

    public void setHeaders(ArrayList<PropertyValueInfo> headers) {
        this.headers = headers;
    }
}
