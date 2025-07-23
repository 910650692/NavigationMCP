package com.sgm.navi.service.adapter.l2;

public class RoadGroupData {
    private int roadLength;
    private int roadTime;
    private int status;

    public int getRoadLength() {
        return roadLength;
    }

    public void setRoadLength(int roadLength) {
        this.roadLength = roadLength;
    }

    public int getRoadTime() {
        return roadTime;
    }

    public void setRoadTime(int roadTime) {
        this.roadTime = roadTime;
    }

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "RoadGroupData{" +
                "roadLength=" + roadLength +
                ", roadTime=" + roadTime +
                ", status=" + status +
                '}';
    }
}
