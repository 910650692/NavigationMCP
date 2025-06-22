package com.sgm.navi.fsa.bean;

/**
 * 快速路出口
 */
public class FastWayExitInfo {
    /**
     * exitID	String	快速路出口ID
     */
    private String exitID;
    /**
     * exitRoadName	String	快速路出口路名
     */
    private String exitRoadName;
    /**
     * exitDirectionName	String	快速路出口方向名称
     */
    private String exitDirectionName;
    /**
     * distance	int	到此快速路出口的剩余距离
     */
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
