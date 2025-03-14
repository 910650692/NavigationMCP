package com.fy.navi.fsa.bean;

public class HighwayInfo {
    private String curHighwayRoadName;
    private String exitHighwayID;
    private String exitHighwayDirectName;
    private int exitRemainDist;
    private String exitHighwayNextRoadName;
    private int nextGPRemainDist;
    private String tollGateName;
    private int tollGateRemainDist;
    private String serviceAreaName;
    private int serviceAreaRemainDist;
    private String nextServiceAreaName;
    private int nextServiceAreaRemainDist;

    public String getCurHighwayRoadName() {
        return curHighwayRoadName;
    }

    public void setCurHighwayRoadName(String curHighwayRoadName) {
        this.curHighwayRoadName = curHighwayRoadName;
    }

    public String getExitHighwayID() {
        return exitHighwayID;
    }

    public void setExitHighwayID(String exitHighwayID) {
        this.exitHighwayID = exitHighwayID;
    }

    public String getExitHighwayDirectName() {
        return exitHighwayDirectName;
    }

    public void setExitHighwayDirectName(String exitHighwayDirectName) {
        this.exitHighwayDirectName = exitHighwayDirectName;
    }

    public int getExitRemainDist() {
        return exitRemainDist;
    }

    public void setExitRemainDist(int exitRemainDist) {
        this.exitRemainDist = exitRemainDist;
    }

    public String getExitHighwayNextRoadName() {
        return exitHighwayNextRoadName;
    }

    public void setExitHighwayNextRoadName(String exitHighwayNextRoadName) {
        this.exitHighwayNextRoadName = exitHighwayNextRoadName;
    }

    public int getNextGPRemainDist() {
        return nextGPRemainDist;
    }

    public void setNextGPRemainDist(int nextGPRemainDist) {
        this.nextGPRemainDist = nextGPRemainDist;
    }

    public String getTollGateName() {
        return tollGateName;
    }

    public void setTollGateName(String tollGateName) {
        this.tollGateName = tollGateName;
    }

    public int getTollGateRemainDist() {
        return tollGateRemainDist;
    }

    public void setTollGateRemainDist(int tollGateRemainDist) {
        this.tollGateRemainDist = tollGateRemainDist;
    }

    public String getServiceAreaName() {
        return serviceAreaName;
    }

    public void setServiceAreaName(String serviceAreaName) {
        this.serviceAreaName = serviceAreaName;
    }

    public int getServiceAreaRemainDist() {
        return serviceAreaRemainDist;
    }

    public void setServiceAreaRemainDist(int serviceAreaRemainDist) {
        this.serviceAreaRemainDist = serviceAreaRemainDist;
    }

    public String getNextServiceAreaName() {
        return nextServiceAreaName;
    }

    public void setNextServiceAreaName(String nextServiceAreaName) {
        this.nextServiceAreaName = nextServiceAreaName;
    }

    public int getNextServiceAreaRemainDist() {
        return nextServiceAreaRemainDist;
    }

    public void setNextServiceAreaRemainDist(int nextServiceAreaRemainDist) {
        this.nextServiceAreaRemainDist = nextServiceAreaRemainDist;
    }

    @Override
    public String toString() {
        return "HighwayInfo{" +
                "curHighwayRoadName='" + curHighwayRoadName + '\'' +
                ", exitHighwayID='" + exitHighwayID + '\'' +
                ", exitHighwayDirectName='" + exitHighwayDirectName + '\'' +
                ", exitRemainDist=" + exitRemainDist +
                ", exitHighwayNextRoadName='" + exitHighwayNextRoadName + '\'' +
                ", nextGPRemainDist=" + nextGPRemainDist +
                ", tollGateName='" + tollGateName + '\'' +
                ", tollGateRemainDist=" + tollGateRemainDist +
                ", serviceAreaName='" + serviceAreaName + '\'' +
                ", serviceAreaRemainDist=" + serviceAreaRemainDist +
                ", nextServiceAreaName='" + nextServiceAreaName + '\'' +
                ", nextServiceAreaRemainDist=" + nextServiceAreaRemainDist +
                '}';
    }
}
