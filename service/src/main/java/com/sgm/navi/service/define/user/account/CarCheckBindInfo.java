package com.sgm.navi.service.define.user.account;

public class CarCheckBindInfo {
    public String mUserId = "";
    public String mAuthId = "";
    public String mToken = "";
    public String mMobileCode = "";
    public String mDeviceCode = "";
    private int mCode;
    private String mResult = "";
    private String mMessage = "";
    private String mTimestamp = "";
    private String mVersion = "";
    private CarCheckBindResult mCarCheckBindResult;
    private AccountProfileInfo mAccountProfileInfo;
    private AccountProfileCarBind mAccountProfileCarBind;

    public String getUserId() {
        return mUserId;
    }

    public void setUserId(String userId) {
        this.mUserId = userId;
    }

    public String getAuthId() {
        return mAuthId;
    }

    public void setAuthId(String authId) {
        this.mAuthId = authId;
    }

    public String getToken() {
        return mToken;
    }

    public void setToken(String token) {
        this.mToken = token;
    }

    public String getMobileCode() {
        return mMobileCode;
    }

    public void setMobileCode(String mobileCode) {
        this.mMobileCode = mobileCode;
    }

    public String getDeviceCode() {
        return mDeviceCode;
    }

    public void setDeviceCode(String deviceCode) {
        this.mDeviceCode = deviceCode;
    }

    public int getCode() {
        return mCode;
    }

    public void setCode(int mCode) {
        this.mCode = mCode;
    }

    public String getResult() {
        return mResult;
    }

    public void setResult(String mResult) {
        this.mResult = mResult;
    }

    public String getMessage() {
        return mMessage;
    }

    public void setMessage(String mMessage) {
        this.mMessage = mMessage;
    }

    public String getTimestamp() {
        return mTimestamp;
    }

    public void setTimestamp(String mTimestamp) {
        this.mTimestamp = mTimestamp;
    }

    public String getVersion() {
        return mVersion;
    }

    public void setVersion(String mVersion) {
        this.mVersion = mVersion;
    }

    public CarCheckBindResult getCarCheckBindResult() {
        return mCarCheckBindResult;
    }

    public void setCarCheckBindResult(CarCheckBindResult carCheckBindResult) {
        this.mCarCheckBindResult = carCheckBindResult;
    }

    public AccountProfileInfo getAccountProfileInfo() {
        return mAccountProfileInfo;
    }

    public void setAccountProfileInfo(AccountProfileInfo accountProfileInfo) {
        this.mAccountProfileInfo = accountProfileInfo;
    }

    public AccountProfileCarBind getAccountProfileCarBind() {
        return mAccountProfileCarBind;
    }

    public void setAccountProfileCarBind(AccountProfileCarBind accountProfileCarBind) {
        this.mAccountProfileCarBind = accountProfileCarBind;
    }
}
