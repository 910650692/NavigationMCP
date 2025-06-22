package com.sgm.navi.service.define.user.group;

import java.util.ArrayList;

public class MsgPushItemBean {

    public long messageId; //消息id
    public int messageType; //消息类型 服务端枚举值，客户端不需要关注
    public int status; // 消息状态: 1 在线消息, 3 离线消息
    public String id; //
    public String bizType; //消息业务类型 旧版本区分消息类型使用，客户端不需要关注
    public String clientId; //设备id,长连接推送唯一标示
    public String sourceId; //消息来源
    public String userId; //用户ID
    public String createTime; //创建时间
    public String expiration; //有效期
    public String sendTime; //发送时间
    public String text; //文本
    public String title; //标题
    public String version; //版本
    public String accessKey;   // 业务key
    public String deviceId; //设备ID
    public String sessionId; // 会话ID
    public boolean isReaded; // 已读状态: true 已读, false 未读
    public int sendType; //场景发送类型
    public String traceId; // 客户端请求轨迹
    public int linkMode; // 同步模式

    public int state = -1;
    public int supNum = -1;
    public ArrayList<TeamMemberBean> groupMembers = new ArrayList();

    public TeamInfoBean content = new TeamInfoBean();

    public int getState() {
        return state;
    }

    public void setState(int state) {
        this.state = state;
    }

    public int getSupNum() {
        return supNum;
    }

    public void setSupNum(int supNum) {
        this.supNum = supNum;
    }

    public ArrayList<TeamMemberBean> getGroupMembers() {
        return groupMembers;
    }

    public void setGroupMembers(ArrayList<TeamMemberBean> groupMembers) {
        this.groupMembers = groupMembers;
    }

    public TeamInfoBean getContent() {
        return content;
    }

    public void setContent(TeamInfoBean content) {
        this.content = content;
    }

    public String getCreateTime() {
        return createTime;
    }

    public void setCreateTime(String createTime) {
        this.createTime = createTime;
    }

    public long getMessageId() {
        return messageId;
    }

    public void setMessageId(long messageId) {
        this.messageId = messageId;
    }

    public int getMessageType() {
        return messageType;
    }

    public void setMessageType(int messageType) {
        this.messageType = messageType;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getBizType() {
        return bizType;
    }

    public void setBizType(String bizType) {
        this.bizType = bizType;
    }

    public String getClientId() {
        return clientId;
    }

    public void setClientId(String clientId) {
        this.clientId = clientId;
    }

    public String getSourceId() {
        return sourceId;
    }

    public void setSourceId(String sourceId) {
        this.sourceId = sourceId;
    }

    public String getUserId() {
        return userId;
    }

    public void setUserId(String userId) {
        this.userId = userId;
    }

    public String getExpiration() {
        return expiration;
    }

    public void setExpiration(String expiration) {
        this.expiration = expiration;
    }

    public String getSendTime() {
        return sendTime;
    }

    public void setSendTime(String sendTime) {
        this.sendTime = sendTime;
    }

    public String getText() {
        return text;
    }

    public void setText(String text) {
        this.text = text;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getAccessKey() {
        return accessKey;
    }

    public void setAccessKey(String accessKey) {
        this.accessKey = accessKey;
    }

    public String getDeviceId() {
        return deviceId;
    }

    public void setDeviceId(String deviceId) {
        this.deviceId = deviceId;
    }

    public String getSessionId() {
        return sessionId;
    }

    public void setSessionId(String sessionId) {
        this.sessionId = sessionId;
    }

    public boolean isReaded() {
        return isReaded;
    }

    public void setReaded(boolean readed) {
        isReaded = readed;
    }

    public int getSendType() {
        return sendType;
    }

    public void setSendType(int sendType) {
        this.sendType = sendType;
    }

    public String getTraceId() {
        return traceId;
    }

    public void setTraceId(String traceId) {
        this.traceId = traceId;
    }

    public int getLinkMode() {
        return linkMode;
    }

    public void setLinkMode(int linkMode) {
        this.linkMode = linkMode;
    }


}
