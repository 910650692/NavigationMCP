package com.fy.navi.service.define.user.account;

import java.util.ArrayList;

/**
 * @Description
 * @Author fh
 * @date 2024/12/22
 */
public class AccountUserInfo {
    public int code;
    public String result;
    public String message;
    public String timestamp;
    public String version;
    public int status = -1;
    public int remain;
    public ArrayList<String> reason;
    public String id;
    public byte[] buffer;
    public int timeout;
    public String uid;
    public String username;
    public String nickname;
    public String avatar;
    public String mobile;
    public String email;
    public AccountProfileInfo profileInfo;

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getResult() {
        return result;
    }

    public void setResult(String result) {
        this.result = result;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public String getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(String timestamp) {
        this.timestamp = timestamp;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public int getRemain() {
        return remain;
    }

    public void setRemain(int remain) {
        this.remain = remain;
    }

    public ArrayList<String> getReason() {
        return reason;
    }

    public void setReason(ArrayList<String> reason) {
        this.reason = reason;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public byte[] getBuffer() {
        return buffer;
    }

    public void setBuffer(byte[] buffer) {
        this.buffer = buffer;
    }

    public int getTimeout() {
        return timeout;
    }

    public void setTimeout(int timeout) {
        this.timeout = timeout;
    }

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getNickname() {
        return nickname;
    }

    public void setNickname(String nickname) {
        this.nickname = nickname;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public String getMobile() {
        return mobile;
    }

    public void setMobile(String mobile) {
        this.mobile = mobile;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public AccountProfileInfo getProfileInfo() {
        return profileInfo;
    }

    public void setProfileInfo(AccountProfileInfo profileInfo) {
        this.profileInfo = profileInfo;
    }
}
