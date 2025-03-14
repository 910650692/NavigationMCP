package com.fy.navi.service.define.user.wechat;

import com.fy.navi.service.define.user.msgpush.PropertyValueInfo;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/31
 */
public class BLResponseBean {

    public int mEAosRequestType; // 请求类型
    public int mNetErrorCode; //网络错误码
    public int mNetworkStatus; //网络请求的状态
    public long mReqHandle;
    public int mHttpAckCode;
    public int code; // 错误码 1,表示成功，其他值为异常值
    public String timestamp;
    public String message; // 错误描述
    public String version;
    public String result; // true/false,是否成功
    public ArrayList<PropertyValueInfo> headers;

    public String avatar; // 微信头像
    public String nickname; // 微信昵称

    public String imgStr; //  base 64的二维码字符串
    public String qrcodeId; // 二维码id

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

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getImgStr() {
        return imgStr;
    }

    public void setImgStr(String imgStr) {
        this.imgStr = imgStr;
    }

    public String getQrcodeId() {
        return qrcodeId;
    }

    public void setQrcodeId(String qrcodeId) {
        this.qrcodeId = qrcodeId;
    }
}
