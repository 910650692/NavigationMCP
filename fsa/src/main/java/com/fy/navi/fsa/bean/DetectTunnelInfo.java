package com.fy.navi.fsa.bean;

/**
 *    隧道信息
 */
public class DetectTunnelInfo {
    /**
     * tunnelLength	int	隧道长度
     */
    private int tunnelLength;
    /**
     * distToTunnelEntrance	int	距离隧道入口的距离
     */
    private int distToTunnelEntrance;
    /**
     * distToTunnelExit	int	距离隧道出口的距离
     */
    private int distToTunnelExit;

    public DetectTunnelInfo() {
    }

    public DetectTunnelInfo(int tunnelLength, int distToTunnelEntrance, int distToTunnelExit) {
        this.tunnelLength = tunnelLength;
        this.distToTunnelEntrance = distToTunnelEntrance;
        this.distToTunnelExit = distToTunnelExit;
    }

    public int getTunnelLength() {
        return tunnelLength;
    }

    public void setTunnelLength(int tunnelLength) {
        this.tunnelLength = tunnelLength;
    }

    public int getDistToTunnelEntrance() {
        return distToTunnelEntrance;
    }

    public void setDistToTunnelEntrance(int distToTunnelEntrance) {
        this.distToTunnelEntrance = distToTunnelEntrance;
    }

    public int getDistToTunnelExit() {
        return distToTunnelExit;
    }

    public void setDistToTunnelExit(int distToTunnelExit) {
        this.distToTunnelExit = distToTunnelExit;
    }

    @Override
    public String toString() {
        return "DetectTunnelInfo{" +
                "tunnelLength=" + tunnelLength +
                ", distToTunnelEntrance=" + distToTunnelEntrance +
                ", distToTunnelExit=" + distToTunnelExit +
                '}';
    }
}
