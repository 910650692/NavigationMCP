package com.fy.navi.fsa.bean;

public class RemainInfo {
    private int remainDistance;
    private int remainTime;
    private int[] auxRemainDist;
    private int[] auxRemainTime;
    private String[] auxLabelInfo;
    private int[] remainTrafficLight;
    private int[] auxTollInfo;

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
