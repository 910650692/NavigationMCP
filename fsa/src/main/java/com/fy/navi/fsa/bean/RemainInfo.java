package com.fy.navi.fsa.bean;

/**
 * 1.1.6、获取导航态剩余距离、时间
 */
public class RemainInfo {
    /**
     * 剩余距离
     */
    private int remainDistance;
    /**
     * 剩余时间
     */
    private int remainTime;
    /**
     * 备选路线剩余距离
     */
    private int[] auxRemainDist;
    /**
     * 备选路线剩余时间
     */
    private int[] auxRemainTime;
    /**
     * 备选路线标签信息
     */
    private String[] auxLabelInfo;
    /**
     * 三条路线的剩余红绿灯数量
     */
    private int[] remainTrafficLight;
    /**
     * 备选路线相对收费信息
     */
    private int[] auxTollInfo;

    private String arrivalTime;//预计到达时间
    private int arrivalDay;//预计到达天数

    public String getArrivalTime() {
        return arrivalTime;
    }

    public void setArrivalTime(String arrivalTime) {
        this.arrivalTime = arrivalTime;
    }

    public int getArrivalDay() {
        return arrivalDay;
    }

    public void setArrivalDay(int arrivalDay) {
        this.arrivalDay = arrivalDay;
    }

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getRemainTime() {
        return remainTime;
    }

    public void setRemainTime(int remainTime) {
        this.remainTime = remainTime;
    }

    public int[] getAuxRemainDist() {
        return auxRemainDist;
    }

    public void setAuxRemainDist(int[] auxRemainDist) {
        this.auxRemainDist = auxRemainDist;
    }

    public int[] getAuxRemainTime() {
        return auxRemainTime;
    }

    public void setAuxRemainTime(int[] auxRemainTime) {
        this.auxRemainTime = auxRemainTime;
    }

    public String[] getAuxLabelInfo() {
        return auxLabelInfo;
    }

    public void setAuxLabelInfo(String[] auxLabelInfo) {
        this.auxLabelInfo = auxLabelInfo;
    }

    public int[] getRemainTrafficLight() {
        return remainTrafficLight;
    }

    public void setRemainTrafficLight(int[] remainTrafficLight) {
        this.remainTrafficLight = remainTrafficLight;
    }

    public int[] getAuxTollInfo() {
        return auxTollInfo;
    }

    public void setAuxTollInfo(int[] auxTollInfo) {
        this.auxTollInfo = auxTollInfo;
    }
}
