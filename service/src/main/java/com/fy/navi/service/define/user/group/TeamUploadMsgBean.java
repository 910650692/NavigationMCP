package com.fy.navi.service.define.user.group;

public class TeamUploadMsgBean {

    public double lon;
    public double lat;
    public String uid;
    public String channel;
    public String teamid;
    public String teamStamp;
    public String memberStamp;

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public String getChannel() {
        return channel;
    }

    public void setChannel(String channel) {
        this.channel = channel;
    }

    public String getTeamid() {
        return teamid;
    }

    public void setTeamid(String teamid) {
        this.teamid = teamid;
    }

    public String getTeamStamp() {
        return teamStamp;
    }

    public void setTeamStamp(String teamStamp) {
        this.teamStamp = teamStamp;
    }

    public String getMemberStamp() {
        return memberStamp;
    }

    public void setMemberStamp(String memberStamp) {
        this.memberStamp = memberStamp;
    }
}
