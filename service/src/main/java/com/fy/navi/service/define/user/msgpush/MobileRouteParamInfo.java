package com.fy.navi.service.define.user.msgpush;

import java.util.ArrayList;

public class MobileRouteParamInfo {
    public int sendMode; //手机端发送模式：0=自动发送，1=手动发送
    public int isOnline; //是否是在线算路：1=在线算路，0=离线算路
    public int playStyle; //客户端播报模式 @range [ 0: 无效, 1: 老手模式, 2: 新手模式, 3: 英文模式 ]

    // 语音提示类型 @range [ 0: 默认语音, 1: 林志玲性感语音, 2: 郭德纲语音, 3: 周星驰语音, 4: 中国好声音, 5: 百变欢笑语音, 6: 超俪嗨语音,
    // 7: 央视春晚语音, 8: 易烊千玺语音, 9: 林志玲语音, 10: 小甜心语音,
    // 11: 标准男中音, 12: 豪爽东北话, 13: 朴实河南话, 14: 快乐湖南话,
    // 15: 麻辣四川话, 16: 贴心台湾话, 17: 动感广东话, 18: TFBOYS 王源, 19: TFBOYS 王俊凯 ]
    public int soundType;
    // 客户端上传的请求类型，取值参考RouteType
    public int routeMode;
    //导航ID，同一次导航此值不变，第一次请求导航服务时，服务下发的导航会话ID，在同一次导航场景中，所有请求需要加上这个ID，方便日志查找和问题调查
    public String naviId;
    //算路策略，具体可参考bl_route_option_dto.h中的RouteStrategy定
    public String type;
    //算路标记
    public String flag;
    //路径下发信息包含内容
    public String contentOption;
    // 车辆信息
    public MobileVehicleInfo vehicle;
    // 起点标签信息
    public MobileLocation location;
    // 终点标签信息 MobileDestination
    // 终点处道路类型 @range [ 0: 普通路, 1: 高架上, 2: 高架下, 3: 主路, 4: 辅路, 5: 隧道, 7: 环岛 ]
    public int destinationType;
    // 终点POI名称
    public String name;
    // 终点POI的ID
    public String poiId;
    // 终点POI的TypeCode
    public String typeCode;
    // 父子POI关系类型
    public String parentRel;
    // 终点父POI节点的ID
    public String parentId;
    // 室内子POI楼层信息
    public String floor;
    // 扩展信息标记，表明具有哪些导航扩展信息，二进制每一位标示一个字段的有无
    // @range [ & 0x01 != 0: multi_navi,多到达点,强制查询POI, & 0x02 != 0: shop_mark,底商，强制查询POI ]
    public int naviExtCode;
    // 终点poi的朝向角
    public double dAngle;

    // 手机端算路请求中的途径点参数列表，来源于route xml请求中viapoint信息
    public ArrayList<MobileRouteViaPointInfo> routeViaPoints;
    // 起点信息：第一个为起点显示点，来源于route xml请求中startpoint信息；后面的为起点投影点，指抓路后匹配到道路上的点，来源于手机端算路返回信息中的起点坐标
    public ArrayList<RoutePathProjectPoints> startPoints;
    // 途径点信息：第一个为途径点显示点，来源于route xml请求中viapoint信息；后面的为途径点投影点，指抓路后匹配到道路上的点，来源于手机端算路返回信息中的途径点坐标
    public ArrayList<RoutePathProjectPoints> viaPoints;
    // 终点信息：第一个为终点显示点，来源于route xml请求中endpoint信息；后面的为终点投影点，指抓路后匹配到道路上的点，来源于手机端算路返回信息中的终点坐标
    public ArrayList<RoutePathProjectPoints> endPoints;

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

    public int getPlayStyle() {
        return playStyle;
    }

    public void setPlayStyle(int playStyle) {
        this.playStyle = playStyle;
    }

    public int getSoundType() {
        return soundType;
    }

    public void setSoundType(int soundType) {
        this.soundType = soundType;
    }

    public int getRouteMode() {
        return routeMode;
    }

    public void setRouteMode(int routeMode) {
        this.routeMode = routeMode;
    }

    public String getNaviId() {
        return naviId;
    }

    public void setNaviId(String naviId) {
        this.naviId = naviId;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getFlag() {
        return flag;
    }

    public void setFlag(String flag) {
        this.flag = flag;
    }

    public String getContentOption() {
        return contentOption;
    }

    public void setContentOption(String contentOption) {
        this.contentOption = contentOption;
    }

    public MobileVehicleInfo getVehicle() {
        return vehicle;
    }

    public void setVehicle(MobileVehicleInfo vehicle) {
        this.vehicle = vehicle;
    }

    public MobileLocation getLocation() {
        return location;
    }

    public void setLocation(MobileLocation location) {
        this.location = location;
    }

    public int getDestinationType() {
        return destinationType;
    }

    public void setDestinationType(int destinationType) {
        this.destinationType = destinationType;
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

    public String getTypeCode() {
        return typeCode;
    }

    public void setTypeCode(String typeCode) {
        this.typeCode = typeCode;
    }

    public String getParentRel() {
        return parentRel;
    }

    public void setParentRel(String parentRel) {
        this.parentRel = parentRel;
    }

    public String getParentId() {
        return parentId;
    }

    public void setParentId(String parentId) {
        this.parentId = parentId;
    }

    public String getFloor() {
        return floor;
    }

    public void setFloor(String floor) {
        this.floor = floor;
    }

    public int getNaviExtCode() {
        return naviExtCode;
    }

    public void setNaviExtCode(int naviExtCode) {
        this.naviExtCode = naviExtCode;
    }

    public double getdAngle() {
        return dAngle;
    }

    public void setdAngle(double dAngle) {
        this.dAngle = dAngle;
    }

    public ArrayList<MobileRouteViaPointInfo> getRouteViaPoints() {
        return routeViaPoints;
    }

    public void setRouteViaPoints(ArrayList<MobileRouteViaPointInfo> routeViaPoints) {
        this.routeViaPoints = routeViaPoints;
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
}
