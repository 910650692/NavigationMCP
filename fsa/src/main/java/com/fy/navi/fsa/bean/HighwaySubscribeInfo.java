package com.fy.navi.fsa.bean;

public class HighwaySubscribeInfo {
    /**
     * 服务区信息类型
     * 0 – 非法值
     * 1 – 高速收费站
     * 2 – 高速入口
     * 3 – 高速出口
     * 4 – 高速服务区
     * 5 – 快速路出口
     */
    private int type;
    /**
     * 高速入口
     */
    private HighWayEntranceInfo highWayEntranceInfo;
    /**
     * 高速收费站
     */
    private TollStation tollStation;
    /**
     * 高速服务区
     */
    private HighWayServiceAreaInfo serviceAreaInfo;
    /**
     * 高速出口
     */
    private HighWayExitInfo highWayExitInfo;
    /**
     * 快速路出口
     */
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
