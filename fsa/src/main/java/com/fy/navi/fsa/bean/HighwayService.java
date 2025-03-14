package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class HighwayService {
    private int type;
    private ArrayList<HighwaySubscribeInfo> highWaySubscribeInfos;
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
