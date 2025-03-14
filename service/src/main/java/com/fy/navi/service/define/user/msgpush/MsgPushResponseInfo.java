package com.fy.navi.service.define.user.msgpush;

import java.util.ArrayList;

public class MsgPushResponseInfo {
    // 请求类型
    public int mEAosRequestType;
    // 网络错误码
    public int mNetErrorCode;
    // 网络请求的状态
    public int mNetworkStatus;
    // 请求句柄
    public long mReqHandle;
    // http ack code;如 HTTP 200 OK
    public int mHttpAckCode;
    // :"错误码", code ：1,表示成功，其他值为异常值，无法穷举，每个业务有自己的定义。
    public int code;
    // 服务器时间戳
    public String timestamp;
    // 错误描述
    public String message;
    // 服务版本
    public String version;
    // "true/false,是否成功
    public String result;
    // http header
    // key/value数组
    public ArrayList<PropertyValueInfo> property;

    public int errorCode;
    public String errorMessage;
    public long taskId;

    public int getErrorCode() {
        return errorCode;
    }

    public void setErrorCode(int errorCode) {
        this.errorCode = errorCode;
    }

    public String getErrorMessage() {
        return errorMessage;
    }

    public void setErrorMessage(String errorMessage) {
        this.errorMessage = errorMessage;
    }

    public long getTaskId() {
        return taskId;
    }

    public void setTaskId(long taskId) {
        this.taskId = taskId;
    }

    public ArrayList<PropertyValueInfo> getProperty() {
        return property;
    }

    public void setProperty(ArrayList<PropertyValueInfo> property) {
        this.property = property;
    }

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

}
