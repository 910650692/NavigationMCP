package com.sgm.navi.fsa.bean;

/**
 * 高速路信息
 */
public class HighwayInfo {
    /**
     * 当前高速路名
     */
    private String curHighwayRoadName;
    /**
     * 高速出口唯一标识
     */
    private String exitHighwayID;
    /**
     * 高速出口方向名称
     */
    private String exitHighwayDirectName;
    /**
     * 出口剩余距离
     */
    private int exitRemainDist;
    /**
     * 高速出口下一个道路名称
     */
    private String exitHighwayNextRoadName;
    /**
     * 距离下一个诱导点的剩余距离
     */
    private int nextGPRemainDist;
    /**
     * 最近的高速收费口名称
     */
    private String tollGateName;
    /**
     * 最近的高速收费口的剩余距离
     */
    private int tollGateRemainDist;
    /**
     * 最近的服务区名称
     */
    private String serviceAreaName;
    /**
     * 最近的服务区的剩余距离
     */
    private int serviceAreaRemainDist;
    /**
     * 下一个服务区名称
     */
    private String nextServiceAreaName;
    /**
     * 下一个最近服务区的剩余距离
     */
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
