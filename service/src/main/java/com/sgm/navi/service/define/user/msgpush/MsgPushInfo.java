package com.sgm.navi.service.define.user.msgpush;

import com.sgm.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

public class MsgPushInfo {
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

    public String autoPushText;
    public String autoPushTitle;
    public String imgUrl; //图片Url
    public String actionUri; //动作Url
    public String subContent; //内容

    public String address; //地址
    public String name; //名称
    public String poiId; //poiId
    public String poiType; //poi类型
    public int lon; //经度
    public int lat; //纬度
    public int navLon; //经度(导航坐标)
    public int navLat; //纬度(导航坐标)
    public String parent; //parent 终点的父POIID
    public int childType; //childtype 终点的父POI与子POI的关系类型
    public String towardsAngle; //POI门脸朝向
    public String floorNo; //终点的楼层信息
    public int endPoiExtension; //该POI有扩展信息 0x0001:标识multi_navi多到达点，强制查询POI 0x0002:标示shop_mark底商，强制查询POI

    public int sendMode; //手机端发送模式：0=自动发送，1=手动发送
    public int isOnline; //是否是在线算路
    public String naviId; // 导航ID，同一次导航此值不变，第一次请求导航服务时，服务下发的导航会话ID，在同一次导航场景中，所有请求需要加上这个ID，方便日志查找和问题调查
    public MobileRouteParamInfo routeParam; // 手机端算路请求参数，来源于手机端route xml请求串中的信息、及手机端算路返回的信息

    //linkid序列，来源于手机端算路返回的linkId List信息
    // 起点信息：第一个为起点显示点， ；后面的为起点投影点，指抓路后匹配到道路上的点，来源于手机端算路返回信息中的起点坐标
    // 起点显示点，来源于route xml请求中startpoint信息;起点投影点，指抓路后匹配到道路上点，来源于route返回信息中的起点坐标
    public ArrayList<RoutePathProjectPoints> startPoints;

    // 途径点信息
    public ArrayList<RoutePathProjectPoints> viaPoints;

    // 途径点信息 相比viaPoints，该结构更加符合路线还原协议的设计。
    // 途经点显
    // 示点坐标信息（可能含多个）
    public ArrayList<GeoPoint> display_points;
    // 途经点到每个path上的投影点信息
    public ArrayList<RoutePathProjectPoints> path_project_points;

    // 终点信息：第一个为终点显示点， ；后面的为终点投影点，指抓路后匹配到道路上的点，来源于手机端算路返回信息中的终点坐标
    public ArrayList<RoutePathProjectPoints> endPoints;

    // linkid序列，来源于route返回的linkidlist信息（不支持多路线）
    public ArrayList<RoutePathProjectPoints> paths;

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getPoiType() {
        return poiType;
    }

    public void setPoiType(String poiType) {
        this.poiType = poiType;
    }

    public int getLon() {
        return lon;
    }

    public void setLon(int lon) {
        this.lon = lon;
    }

    public int getLat() {
        return lat;
    }

    public void setLat(int lat) {
        this.lat = lat;
    }

    public int getNavLon() {
        return navLon;
    }

    public void setNavLon(int navLon) {
        this.navLon = navLon;
    }

    public int getNavLat() {
        return navLat;
    }

    public void setNavLat(int navLat) {
        this.navLat = navLat;
    }

    public String getParent() {
        return parent;
    }

    public void setParent(String parent) {
        this.parent = parent;
    }

    public int getChildType() {
        return childType;
    }

    public void setChildType(int childType) {
        this.childType = childType;
    }

    public String getTowardsAngle() {
        return towardsAngle;
    }

    public void setTowardsAngle(String towardsAngle) {
        this.towardsAngle = towardsAngle;
    }

    public String getFloorNo() {
        return floorNo;
    }

    public void setFloorNo(String floorNo) {
        this.floorNo = floorNo;
    }

    public int getEndPoiExtension() {
        return endPoiExtension;
    }

    public void setEndPoiExtension(int endPoiExtension) {
        this.endPoiExtension = endPoiExtension;
    }

    public int getSendMode() {
        return sendMode;
    }

    public void setSendMode(int sendMode) {
        this.sendMode = sendMode;
    }

    public int getIsOnline() {
        return isOnline;
    }

    public void setIsOnline(int isOnline) {
        this.isOnline = isOnline;
    }

    public String getNaviId() {
        return naviId;
    }

    public void setNaviId(String naviId) {
        this.naviId = naviId;
    }

    public MobileRouteParamInfo getRouteParam() {
        return routeParam;
    }

    public void setRouteParam(MobileRouteParamInfo routeParam) {
        this.routeParam = routeParam;
    }

    public ArrayList<RoutePathProjectPoints> getStartPoints() {
        return startPoints;
    }

    public void setStartPoints(ArrayList<RoutePathProjectPoints> startPoints) {
        this.startPoints = startPoints;
    }

    public ArrayList<RoutePathProjectPoints> getViaPoints() {
        return viaPoints;
    }

    public void setViaPoints(ArrayList<RoutePathProjectPoints> viaPoints) {
        this.viaPoints = viaPoints;
    }

    public ArrayList<RoutePathProjectPoints> getEndPoints() {
        return endPoints;
    }

    public void setEndPoints(ArrayList<RoutePathProjectPoints> endPoints) {
        this.endPoints = endPoints;
    }

    public ArrayList<RoutePathProjectPoints> getPaths() {
        return paths;
    }

    public void setPaths(ArrayList<RoutePathProjectPoints> paths) {
        this.paths = paths;
    }

    public ArrayList<GeoPoint> getDisplay_points() {
        return display_points;
    }

    public void setDisplay_points(ArrayList<GeoPoint> display_points) {
        this.display_points = display_points;
    }

    public ArrayList<RoutePathProjectPoints> getPath_project_points() {
        return path_project_points;
    }

    public void setPath_project_points(ArrayList<RoutePathProjectPoints> path_project_points) {
        this.path_project_points = path_project_points;
    }

    public String getImgUrl() {
        return imgUrl;
    }

    public void setImgUrl(String imgUrl) {
        this.imgUrl = imgUrl;
    }

    public String getActionUri() {
        return actionUri;
    }

    public void setActionUri(String actionUri) {
        this.actionUri = actionUri;
    }

    public String getSubContent() {
        return subContent;
    }

    public void setSubContent(String subContent) {
        this.subContent = subContent;
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

    public String getAutoPushText() {
        return autoPushText;
    }

    public void setAutoPushText(String autoPushText) {
        this.autoPushText = autoPushText;
    }

    public String getAutoPushTitle() {
        return autoPushTitle;
    }

    public void setAutoPushTitle(String autoPushTitle) {
        this.autoPushTitle = autoPushTitle;
    }

}
