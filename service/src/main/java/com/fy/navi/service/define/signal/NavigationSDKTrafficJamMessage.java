package com.fy.navi.service.define.signal;

public class NavigationSDKTrafficJamMessage {
    private Integer nOABttnStngReq; //
    private Integer nOANaviStat; //
    private Integer disTTfcJmRd; //
    private Integer disTTfcJmRdAv; //
    private Integer disOTfcJmRd; //
    private Integer disOTfcJmRdAv; //
    private Integer tfcJmRdAvgSpd; //
    private Integer tfcJmRdAvgSpdAv; //

    public Integer getnOABttnStngReq() {
        return nOABttnStngReq;
    }

    public void setnOABttnStngReq(Integer nOABttnStngReq) {
        this.nOABttnStngReq = nOABttnStngReq;
    }

    public Integer getnOANaviStat() {
        return nOANaviStat;
    }

    public void setnOANaviStat(Integer nOANaviStat) {
        this.nOANaviStat = nOANaviStat;
    }

    public Integer getDisTTfcJmRd() {
        return disTTfcJmRd;
    }

    public void setDisTTfcJmRd(Integer disTTfcJmRd) {
        this.disTTfcJmRd = disTTfcJmRd;
    }

    public Integer getDisTTfcJmRdAv() {
        return disTTfcJmRdAv;
    }

    public void setDisTTfcJmRdAv(Integer disTTfcJmRdAv) {
        this.disTTfcJmRdAv = disTTfcJmRdAv;
    }

    public Integer getDisOTfcJmRd() {
        return disOTfcJmRd;
    }

    public void setDisOTfcJmRd(Integer disOTfcJmRd) {
        this.disOTfcJmRd = disOTfcJmRd;
    }

    public Integer getDisOTfcJmRdAv() {
        return disOTfcJmRdAv;
    }

    public void setDisOTfcJmRdAv(Integer disOTfcJmRdAv) {
        this.disOTfcJmRdAv = disOTfcJmRdAv;
    }

    public Integer getTfcJmRdAvgSpd() {
        return tfcJmRdAvgSpd;
    }

    public void setTfcJmRdAvgSpd(Integer tfcJmRdAvgSpd) {
        this.tfcJmRdAvgSpd = tfcJmRdAvgSpd;
    }

    public Integer getTfcJmRdAvgSpdAv() {
        return tfcJmRdAvgSpdAv;
    }

    public void setTfcJmRdAvgSpdAv(Integer tfcJmRdAvgSpdAv) {
        this.tfcJmRdAvgSpdAv = tfcJmRdAvgSpdAv;
    }

    @Override
    public String toString() {
        return "NavigationSDKTrafficJamMessage{" +
                "nOABttnStngReq=" + nOABttnStngReq +
                ", nOANaviStat=" + nOANaviStat +
                ", disTTfcJmRd=" + disTTfcJmRd +
                ", disTTfcJmRdAv=" + disTTfcJmRdAv +
                ", disOTfcJmRd=" + disOTfcJmRd +
                ", disOTfcJmRdAv=" + disOTfcJmRdAv +
                ", tfcJmRdAvgSpd=" + tfcJmRdAvgSpd +
                ", tfcJmRdAvgSpdAv=" + tfcJmRdAvgSpdAv +
                '}';
    }
}
