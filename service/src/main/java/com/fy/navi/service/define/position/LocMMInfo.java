package com.fy.navi.service.define.position;

import java.math.BigInteger;
import java.util.ArrayList;

public class LocMMInfo {

    /*** 时间戳。单位：毫秒。此时间戳与计算地图匹配反馈的信号的时间戳一致 */
    private BigInteger ticktime;
    /***道路数量，当nCount == 0时表示feedBackNode中的信息不可使用，此时无地图匹配反馈 */
    private int count;
    /***到概率最大的道路的起始节点距离 */
    private double toRoadStartDist;
    /***到概率最大的道路的末尾节点距离 */
    private double toRoadEndDist;
    /***到概率最大的道路的末尾节点距离 */
    ArrayList<LocMMNode> feedbackNodes;
    /***高架识别插件是否已开启*/
    private boolean viaductStartFlag;

    /***是否有离线地图数据*/
    private boolean hasMapData;

    /***GPS信号是否在道路上*/
    private boolean gpsOnRoad;

    /***是否使用云+端数据匹配*/
    private boolean isOnline;

    /***在线地图数据版本号，isOnline==true时有效*/
    private int onlineVersion;

    /***离线地图数据版本号，用于标识当前车标所在城市的离线地图数据版本号，该值来源于数据模块*/
    private String offlineDataVersion;

    public LocMMInfo() {
    }

    public BigInteger getTicktime() {
        return ticktime;
    }

    public void setTicktime(BigInteger ticktime) {
        this.ticktime = ticktime;
    }

    public int getCount() {
        return count;
    }

    public void setCount(int count) {
        this.count = count;
    }

    public double getToRoadStartDist() {
        return toRoadStartDist;
    }

    public void setToRoadStartDist(double toRoadStartDist) {
        this.toRoadStartDist = toRoadStartDist;
    }

    public double getToRoadEndDist() {
        return toRoadEndDist;
    }

    public void setToRoadEndDist(double toRoadEndDist) {
        this.toRoadEndDist = toRoadEndDist;
    }

    public ArrayList<LocMMNode> getFeedbackNodes() {
        return feedbackNodes;
    }

    public void setFeedbackNodes(ArrayList<LocMMNode> feedbackNodes) {
        this.feedbackNodes = feedbackNodes;
    }

    public boolean isViaductStartFlag() {
        return viaductStartFlag;
    }

    public void setViaductStartFlag(boolean viaductStartFlag) {
        this.viaductStartFlag = viaductStartFlag;
    }

    public boolean isHasMapData() {
        return hasMapData;
    }

    public void setHasMapData(boolean hasMapData) {
        this.hasMapData = hasMapData;
    }

    public boolean isGpsOnRoad() {
        return gpsOnRoad;
    }

    public void setGpsOnRoad(boolean gpsOnRoad) {
        this.gpsOnRoad = gpsOnRoad;
    }

    public boolean isOnline() {
        return isOnline;
    }

    public void setOnline(boolean online) {
        isOnline = online;
    }

    public int getOnlineVersion() {
        return onlineVersion;
    }

    public void setOnlineVersion(int onlineVersion) {
        this.onlineVersion = onlineVersion;
    }

    public String getOfflineDataVersion() {
        return offlineDataVersion;
    }

    public void setOfflineDataVersion(String offlineDataVersion) {
        this.offlineDataVersion = offlineDataVersion;
    }
}
