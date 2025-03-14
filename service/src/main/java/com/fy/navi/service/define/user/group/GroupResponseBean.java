package com.fy.navi.service.define.user.group;

import java.util.ArrayList;

public class GroupResponseBean {

    public int code;
    public String message;
    public byte[] buffer;
    public String url = "";
    public String teamId = "";
    public String teamNumber = "";
    public String teamStamp = "";
    public String memberStamp = "";
    public GroupBaseInfoBean team = new GroupBaseInfoBean();
    public ArrayList<GroupMemberBean> friends = new ArrayList();
    public ArrayList<GroupMemberBean> members = new ArrayList();

    public int getCode() {
        return code;
    }

    public void setCode(int code) {
        this.code = code;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public byte[] getBuffer() {
        return buffer;
    }

    public void setBuffer(byte[] buffer) {
        this.buffer = buffer;
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getTeamId() {
        return teamId;
    }

    public void setTeamId(String teamId) {
        this.teamId = teamId;
    }

    public String getTeamNumber() {
        return teamNumber;
    }

    public void setTeamNumber(String teamNumber) {
        this.teamNumber = teamNumber;
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

    public GroupBaseInfoBean getTeam() {
        return team;
    }

    public void setTeam(GroupBaseInfoBean team) {
        this.team = team;
    }

    public ArrayList<GroupMemberBean> getFriends() {
        return friends;
    }

    public void setFriends(ArrayList<GroupMemberBean> friends) {
        this.friends = friends;
    }

    public ArrayList<GroupMemberBean> getMembers() {
        return members;
    }

    public void setMembers(ArrayList<GroupMemberBean> members) {
        this.members = members;
    }
}
