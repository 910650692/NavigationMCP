package com.fy.navi.service.define.route;

import java.util.List;

/**
 * 路线Item详细信息.
 */
public class RouteLineInfo {
    private long pathID;//路线ID
    private int type;//类型
    private String length;//路线长度
    private String label;//路线长度
    private String travelTime;//到达时间
    private String staticTravelTime;//静态到达时间
    private String tollCost;//路线总收费金额
    private String trafficLightCount;//红绿灯个数
    private String naviID;//导航标识
    private boolean isOnline;//是否在线算路
    private boolean elecRouteBool = true;//是否充电算路
    private String elecRouteLabel;//是否在线算路
    private long dis;//数字长度

    private List<RouteLineSegmentInfo> routeLineSegmentInfos;
    boolean canBeArrive = true;

    int remainPercent;

    public boolean isOnline() {
        return isOnline;
    }

    public void setOnline(boolean online) {
        isOnline = online;
    }

    public String getNaviID() {
        return naviID;
    }

    public void setNaviID(String naviID) {
        this.naviID = naviID;
    }

    public String getTrafficLightCount() {
        return trafficLightCount;
    }

    public void setTrafficLightCount(String trafficLightCount) {
        this.trafficLightCount = trafficLightCount;
    }

    public String getTollCost() {
        return tollCost;
    }

    public void setTollCost(String tollCost) {
        this.tollCost = tollCost;
    }

    public String getStaticTravelTime() {
        return staticTravelTime;
    }

    public void setStaticTravelTime(String staticTravelTime) {
        this.staticTravelTime = staticTravelTime;
    }

    public String getTravelTime() {
        return travelTime;
    }

    public void setTravelTime(String travelTime) {
        this.travelTime = travelTime;
    }

    public String getLabel() {
        return label;
    }

    public void setLabel(String label) {
        this.label = label;
    }

    public String getLength() {
        return length;
    }

    public void setLength(String length) {
        this.length = length;
    }

    public long getDis() {
        return dis;
    }

    public void setDis(long dis) {
        this.dis = dis;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public long getPathID() {
        return pathID;
    }

    public void setPathID(long pathID) {
        this.pathID = pathID;
    }

    public void setRouteLineSegmentInfos(List<RouteLineSegmentInfo> routeLineSegmentInfos) {
        this.routeLineSegmentInfos = routeLineSegmentInfos;
    }

    public List<RouteLineSegmentInfo> getRouteLineSegmentInfos() {
        return routeLineSegmentInfos;
    }

    public boolean getElecRoute() {
        return elecRouteBool;
    }

    public void setElecRoute(boolean elecRouteBool) {
        this.elecRouteBool = elecRouteBool;
    }

    public void setElecRouteLabel(String elecRouteLabel) {
        this.elecRouteLabel = elecRouteLabel;
    }

    public String getElecRouteLabel() {
        return elecRouteLabel;
    }

    public boolean isCanBeArrive() {
        return canBeArrive;
    }

    public void setCanBeArrive(boolean canBeArrive) {
        this.canBeArrive = canBeArrive;
    }

    public void setRemainPercent(int remainPercent) {
        this.remainPercent = remainPercent;
    }

    public int getRemainPercent() {
        return remainPercent;
    }
}
