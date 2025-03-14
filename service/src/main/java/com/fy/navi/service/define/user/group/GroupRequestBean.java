package com.fy.navi.service.define.user.group;


import java.util.ArrayList;

public class GroupRequestBean {

    public long taskId;
    public int reqType;
    public String teamNick = "";
    public String teamName = "";
    public String announcement = "";
    public String url = "";
    public String teamId = "";
    public String teamNumber = "";

    public GroupDestinationBean destination = new GroupDestinationBean();
    public ArrayList<String> kickIds = new ArrayList();
    public ArrayList<String> inviteIds = new ArrayList();

    public long getTaskId() {
        return taskId;
    }

    public void setTaskId(long taskId) {
        this.taskId = taskId;
    }

    public int getReqType() {
        return reqType;
    }

    public void setReqType(int reqType) {
        this.reqType = reqType;
    }

    public String getTeamNick() {
        return teamNick;
    }

    public void setTeamNick(String teamNick) {
        this.teamNick = teamNick;
    }

    public String getTeamName() {
        return teamName;
    }

    public void setTeamName(String teamName) {
        this.teamName = teamName;
    }

    public String getAnnouncement() {
        return announcement;
    }

    public void setAnnouncement(String announcement) {
        this.announcement = announcement;
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

    public GroupDestinationBean getDestination() {
        return destination;
    }

    public void setDestination(GroupDestinationBean destination) {
        this.destination = destination;
    }

    public ArrayList<String> getKickIds() {
        return kickIds;
    }

    public void setKickIds(ArrayList<String> kickIds) {
        this.kickIds = kickIds;
    }

    public ArrayList<String> getInviteIds() {
        return inviteIds;
    }

    public void setInviteIds(ArrayList<String> inviteIds) {
        this.inviteIds = inviteIds;
    }
}
