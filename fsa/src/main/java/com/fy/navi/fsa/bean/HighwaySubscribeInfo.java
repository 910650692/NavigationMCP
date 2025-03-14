package com.fy.navi.fsa.bean;

public class HighwaySubscribeInfo {
    private int type;
    private HighWayEntranceInfo highWayEntranceInfo;
    private TollStation tollStation;
    private HighWayServiceAreaInfo serviceAreaInfo;
    private HighWayExitInfo highWayExitInfo;
    private FastWayExitInfo fastWayExitInfo;

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public HighWayEntranceInfo getHighWayEntranceInfo() {
        return highWayEntranceInfo;
    }

    public void setHighWayEntranceInfo(HighWayEntranceInfo highWayEntranceInfo) {
        this.highWayEntranceInfo = highWayEntranceInfo;
    }

    public TollStation getTollStation() {
        return tollStation;
    }

    public void setTollStation(TollStation tollStation) {
        this.tollStation = tollStation;
    }

    public HighWayServiceAreaInfo getServiceAreaInfo() {
        return serviceAreaInfo;
    }

    public void setServiceAreaInfo(HighWayServiceAreaInfo serviceAreaInfo) {
        this.serviceAreaInfo = serviceAreaInfo;
    }

    public HighWayExitInfo getHighWayExitInfo() {
        return highWayExitInfo;
    }

    public void setHighWayExitInfo(HighWayExitInfo highWayExitInfo) {
        this.highWayExitInfo = highWayExitInfo;
    }

    public FastWayExitInfo getFastWayExitInfo() {
        return fastWayExitInfo;
    }

    public void setFastWayExitInfo(FastWayExitInfo fastWayExitInfo) {
        this.fastWayExitInfo = fastWayExitInfo;
    }

    @Override
    public String toString() {
        return "HighwaySubscribeInfo{" +
                "type=" + type +
                ", highWayEntranceInfo=" + highWayEntranceInfo +
                ", tollStation=" + tollStation +
                ", serviceAreaInfo=" + serviceAreaInfo +
                ", highWayExitInfo=" + highWayExitInfo +
                ", fastWayExitInfo=" + fastWayExitInfo +
                '}';
    }
}
