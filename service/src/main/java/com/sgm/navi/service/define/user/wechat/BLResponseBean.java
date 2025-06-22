package com.sgm.navi.service.define.user.wechat;

import com.sgm.navi.service.define.user.msgpush.PropertyValueInfo;

import java.util.ArrayList;


public class BLResponseBean {

    private int mEAosRequestType; // 请求类型
    private int mNetErrorCode; //网络错误码
    private int mNetworkStatus; //网络请求的状态
    private long mReqHandle;
    private int mHttpAckCode;
    private int mCode; // 错误码 1,表示成功，其他值为异常值
    private String mTimestamp;
    private String mMessage; // 错误描述
    private String mVersion;
    private String mResult; // true/false,是否成功
    private ArrayList<PropertyValueInfo> mHeaders;

    private String mAvatar; // 微信头像
    private String mNickname; // 微信昵称

    private String mImgStr; //  base 64的二维码字符串
    private String mQRcodeId; // 二维码id

    public int getEAosRequestType() {
        return mEAosRequestType;
    }

    public void setEAosRequestType(final int eaosRequestType) {
        this.mEAosRequestType = eaosRequestType;
    }

    public int getNetErrorCode() {
        return mNetErrorCode;
    }

    public void setNetErrorCode(final int netErrorCode) {
        this.mNetErrorCode = netErrorCode;
    }

    public int getNetworkStatus() {
        return mNetworkStatus;
    }

    public void setNetworkStatus(final int networkStatus) {
        this.mNetworkStatus = networkStatus;
    }

    public long getReqHandle() {
        return mReqHandle;
    }

    public void setReqHandle(final long reqHandle) {
        this.mReqHandle = reqHandle;
    }

    public int getHttpAckCode() {
        return mHttpAckCode;
    }

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
        return mMessage;
    }

    public void setMessage(final String message) {
        this.mMessage = message;
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

    public String getAvatar() {
        return mAvatar;
    }

    public void setAvatar(final String avatar) {
        this.mAvatar = avatar;
    }

    public String getNickname() {
        return mNickname;
    }

    public void setNickname(final String nickname) {
        this.mNickname = nickname;
    }

    public String getImgStr() {
        return mImgStr;
    }

    public void setImgStr(final String imgStr) {
        this.mImgStr = imgStr;
    }

    public String getQrcodeId() {
        return mQRcodeId;
    }

    public void setQrcodeId(final String qrcodeId) {
        this.mQRcodeId = qrcodeId;
    }
}
