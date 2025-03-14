package com.fy.navi.service.define.user.group;


public class GroupBaseInfoBean {
    public String teamId;
    public String chatId;
    public String teamName;
    public String teamNumber;
    public String leaderId;
    public long dissloveTime;
    public long createdTime;
    public long lastUpdate;
    public long editTime;
    public String content;
    public GroupDestinationBean destination;

    public String getTeamId() {
        return teamId;
    }

    public void setTeamId(String teamId) {
        this.teamId = teamId;
    }

    public String getChatId() {
        return chatId;
    }

    public void setChatId(String chatId) {
        this.chatId = chatId;
    }

    public String getTeamName() {
        return teamName;
    }

    public void setTeamName(String teamName) {
        this.teamName = teamName;
    }

    public String getTeamNumber() {
        return teamNumber;
    }

    public void setTeamNumber(String teamNumber) {
        this.teamNumber = teamNumber;
    }

    public String getLeaderId() {
        return leaderId;
    }

    public void setLeaderId(String leaderId) {
        this.leaderId = leaderId;
    }

    public long getDissloveTime() {
        return dissloveTime;
    }

    public void setDissloveTime(long dissloveTime) {
        this.dissloveTime = dissloveTime;
    }

    public long getCreatedTime() {
        return createdTime;
    }

    public void setCreatedTime(long createdTime) {
        this.createdTime = createdTime;
    }

    public long getLastUpdate() {
        return lastUpdate;
    }

    public void setLastUpdate(long lastUpdate) {
        this.lastUpdate = lastUpdate;
    }

    public long getEditTime() {
        return editTime;
    }

    public void setEditTime(long editTime) {
        this.editTime = editTime;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public GroupDestinationBean getDestination() {
        return destination;
    }

    public void setDestination(GroupDestinationBean destination) {
        this.destination = destination;
    }
}
