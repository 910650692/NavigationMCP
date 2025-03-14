package com.fy.navi.service.define.user.group;

import com.fy.navi.service.define.bean.GeoPoint;

public class GroupMemberBean {

    public String uid;
    public String imgUrl;
    public String nickName;
    public String teamNick;
    public String source;
    public GeoPoint locInfo;
    public long locUpdateTime;
    public long joinTime;
    public boolean online;

    public String userName;

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public String getImgUrl() {
        return imgUrl;
    }

    public void setImgUrl(String imgUrl) {
        this.imgUrl = imgUrl;
    }

    public String getNickName() {
        return nickName;
    }

    public void setNickName(String nickName) {
        this.nickName = nickName;
    }

    public String getTeamNick() {
        return teamNick;
    }

    public void setTeamNick(String teamNick) {
        this.teamNick = teamNick;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public GeoPoint getLocInfo() {
        return locInfo;
    }

    public void setLocInfo(GeoPoint locInfo) {
        this.locInfo = locInfo;
    }

    public long getLocUpdateTime() {
        return locUpdateTime;
    }

    public void setLocUpdateTime(long locUpdateTime) {
        this.locUpdateTime = locUpdateTime;
    }

    public long getJoinTime() {
        return joinTime;
    }

    public void setJoinTime(long joinTime) {
        this.joinTime = joinTime;
    }

    public boolean isOnline() {
        return online;
    }

    public void setOnline(boolean online) {
        this.online = online;
    }
}
