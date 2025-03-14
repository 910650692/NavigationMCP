package com.fy.navi.fsa.bean;

public class DetectTunnelInfo {
    private int tunnelLength;
    private int distToTunnelEntrance;
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
