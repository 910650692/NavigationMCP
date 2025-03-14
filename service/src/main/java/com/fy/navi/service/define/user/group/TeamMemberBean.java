package com.fy.navi.service.define.user.group;


import com.fy.navi.service.define.bean.GeoPoint;

public class TeamMemberBean {

    public boolean online;
    public long locationUpdateTime;
    public String uid;
    public GeoPoint locInfo;

    public boolean isOnline() {
        return online;
    }

    public void setOnline(boolean online) {
        this.online = online;
    }

    public long getLocationUpdateTime() {
        return locationUpdateTime;
    }

    public void setLocationUpdateTime(long locationUpdateTime) {
        this.locationUpdateTime = locationUpdateTime;
    }

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public GeoPoint getLocInfo() {
        return locInfo;
    }

    public void setLocInfo(GeoPoint locInfo) {
        this.locInfo = locInfo;
    }
}
