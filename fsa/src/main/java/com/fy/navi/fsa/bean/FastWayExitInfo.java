package com.fy.navi.fsa.bean;

public class FastWayExitInfo {
    private String exitID;
    private String exitRoadName;
    private String exitDirectionName;
    private int distance;

    public FastWayExitInfo() {
    }

    public FastWayExitInfo(String exitID, String exitRoadName, String exitDirectionName, int distance) {
        this.exitID = exitID;
        this.exitRoadName = exitRoadName;
        this.exitDirectionName = exitDirectionName;
        this.distance = distance;
    }

    public String getExitID() {
        return exitID;
    }

    public void setExitID(String exitID) {
        this.exitID = exitID;
    }

    public String getExitRoadName() {
        return exitRoadName;
    }

    public void setExitRoadName(String exitRoadName) {
        this.exitRoadName = exitRoadName;
    }

    public String getExitDirectionName() {
        return exitDirectionName;
    }

    public void setExitDirectionName(String exitDirectionName) {
        this.exitDirectionName = exitDirectionName;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    @Override
    public String toString() {
        return "FastWayExitInfo{" +
                "exitID='" + exitID + '\'' +
                ", exitRoadName='" + exitRoadName + '\'' +
                ", exitDirectionName='" + exitDirectionName + '\'' +
                ", distance=" + distance +
                '}';
    }
}
