package com.fy.navi.fsa.bean;

/**
 * 高速入口
 */
public class HighWayEntranceInfo {
    /**
     * 高速入口道路名称
     */
    private String roadName;
    /**
     * 到此高速入口的距离
     */
    private int distance;

    public HighWayEntranceInfo() {
    }

    public HighWayEntranceInfo(String roadName, int distance) {
        this.roadName = roadName;
        this.distance = distance;
    }

    public String getRoadName() {
        return roadName;
    }

    public void setRoadName(String roadName) {
        this.roadName = roadName;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    @Override
    public String toString() {
        return "HighWayEntranceInfo{" +
                "roadName='" + roadName + '\'' +
                ", distance=" + distance +
                '}';
    }
}
