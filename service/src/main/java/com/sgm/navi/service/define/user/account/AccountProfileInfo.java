package com.sgm.navi.service.define.user.account;


public class AccountProfileInfo {
    private String mUid = "";
    private String mUsername = "";
    private String mNickname = "";
    private String mAvatar = "";
    private String mMobile = "";
    private String mEmail = "";

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
}
