package com.sgm.navi.service.define.user.account;

import java.util.ArrayList;


public class AccountUserInfo {
    private int mCode;
    private String mResult;
    private String mMessage;
    private String mTimestamp;
    private String mVersion;
    private int mStatus = -1;
    private int mRemain;
    private ArrayList<String> mReason;
    private String mId;
    private byte[] mBuffer;
    private int mTimeout;
    private String mUid;
    private String mUsername;
    private String mNickname;
    private String mAvatar;
    private String mMobile;
    private String mEmail;
    private AccountProfileInfo mProfileInfo;

    public int getCode() {
        return mCode;
    }

    public void setCode(final int code) {
        this.mCode = code;
    }

    public String getResult() {
        return mResult;
    }

    public void setResult(final String result) {
        this.mResult = result;
    }

    public String getMessage() {
        return mMessage;
    }

    public void setMessage(final String message) {
        this.mMessage = message;
    }

    public String getTimestamp() {
        return mTimestamp;
    }

    public void setTimestamp(final String timestamp) {
        this.mTimestamp = timestamp;
    }

    public String getVersion() {
        return mVersion;
    }

    public void setVersion(final String version) {
        this.mVersion = version;
    }

    public int getStatus() {
        return mStatus;
    }

    public void setStatus(final int status) {
        this.mStatus = status;
    }

    public int getRemain() {
        return mRemain;
    }

    public void setRemain(final int remain) {
        this.mRemain = remain;
    }

    public ArrayList<String> getReason() {
        return mReason;
    }

    public void setReason(final ArrayList<String> reason) {
        this.mReason = reason;
    }

    public String getId() {
        return mId;
    }

    public void setId(final String id) {
        this.mId = id;
    }

    public byte[] getBuffer() {
        return mBuffer;
    }

    public void setBuffer(final byte[] buffer) {
        this.mBuffer = buffer;
    }

    public int getTimeout() {
        return mTimeout;
    }

    public void setTimeout(final int timeout) {
        this.mTimeout = timeout;
    }

    public String getUid() {
        return mUid;
    }

    public void setUid(final String uid) {
        this.mUid = uid;
    }

    public String getUsername() {
        return mUsername;
    }

    public void setUsername(final String username) {
        this.mUsername = username;
    }

    public String getNickname() {
        return mNickname;
    }

    public void setNickname(final String nickname) {
        this.mNickname = nickname;
    }

    public String getAvatar() {
        return mAvatar;
    }

    public void setAvatar(final String avatar) {
        this.mAvatar = avatar;
    }

    public String getMobile() {
        return mMobile;
    }

    public void setMobile(final String mobile) {
        this.mMobile = mobile;
    }

    public String getEmail() {
        return mEmail;
    }

    public void setEmail(final String email) {
        this.mEmail = email;
    }

    public AccountProfileInfo getProfileInfo() {
        return mProfileInfo;
    }

    public void setProfileInfo(final AccountProfileInfo profileInfo) {
        this.mProfileInfo = profileInfo;
    }
}
