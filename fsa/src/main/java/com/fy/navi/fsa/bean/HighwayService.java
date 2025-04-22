package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class HighwayService {
    /**
     * 服务区信息类型
     * 0 – 高速信息订阅列表，包含深度信息
     * 1 – 高速路信息，这里的服务区信息只有名称和距离，没有深度信息
     */
    private int type;
    /**
     * 高速信息订阅列表
     */
    private ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos;
    /**
     * 高速路信息
     */
    private HighwayTotalInfo highwayTotalInfo;

    public HighwayService() {
    }

    public HighwayService(int type, ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos, HighwayTotalInfo highwayTotalInfo) {
        this.type = type;
        this.highWaySubscribeInfos = highWaySubscribeInfos;
        this.highwayTotalInfo = highwayTotalInfo;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public ArrayList<HighwaySubscribeInfo> getHighWaySubscribeInfos() {
        return highWaySubscribeInfos;
    }

    public void setHighWaySubscribeInfos(ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos) {
        this.highWaySubscribeInfos = highWaySubscribeInfos;
    }

    public HighwayTotalInfo getHighwayTotalInfo() {
        return highwayTotalInfo;
    }

    public void setHighwayTotalInfo(HighwayTotalInfo highwayTotalInfo) {
        this.highwayTotalInfo = highwayTotalInfo;
    }

    @Override
    public String toString() {
        return "HighwayService{" +
                "type=" + type +
                ", highWaySubscribeInfos=" + highWaySubscribeInfos +
                ", highwayTotalInfo=" + highwayTotalInfo +
                '}';
    }
}
