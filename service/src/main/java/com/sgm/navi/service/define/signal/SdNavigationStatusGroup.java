package com.sgm.navi.service.define.signal;

import java.util.Arrays;

public class SdNavigationStatusGroup {
    private Integer naviStat = 0; // 导航状态
    private Integer naviStatCrntRdLvl = 0; // 道路等级
    private Integer naviStatCrntRdLvl_Inv = 0; // 道路等级是否有效
    private Integer naviStatCrntRdMpConf = 0; // 绑路状态
    private Integer naviStatDistToHDStrt = 0; // 到可开启辅助驾驶起点的距离
    private Integer naviStatDistToHDStrt_Inv = 0; // HD起点距离是否有效
    private Integer naviStatDistToViaPoint = 0; // 途径点距离
    private Integer naviStatDistToViaPoint_Inv = 0; // 途径点距离是否有效
    private Integer naviStatRmnDist = 0; // 剩余距离
    private Integer naviStatRmnDist_Inv = 0; // 剩余距离是否有效

    private final Integer[] signalArray = new Integer[10];

    public Integer[] toArray() {
        signalArray[0] = naviStat;
        signalArray[1] = naviStatCrntRdMpConf;
        signalArray[2] = naviStatCrntRdLvl;
        signalArray[3] = naviStatCrntRdLvl_Inv;
        signalArray[4] = naviStatRmnDist;
        signalArray[5] = naviStatRmnDist_Inv;
        signalArray[6] = naviStatDistToViaPoint;
        signalArray[7] = naviStatDistToViaPoint_Inv;
        signalArray[8] = naviStatDistToHDStrt;
        signalArray[9] = naviStatDistToHDStrt_Inv;
        return signalArray;
    }

    public Integer getNaviStat() {
        return naviStat;
    }

    public void setNaviStat(Integer naviStat) {
        this.naviStat = naviStat;
    }

    public Integer getNaviStatCrntRdLvl() {
        return naviStatCrntRdLvl;
    }

    public void setNaviStatCrntRdLvl(Integer naviStatCrntRdLvl) {
        this.naviStatCrntRdLvl = naviStatCrntRdLvl;
    }

    public Integer getNaviStatCrntRdLvl_Inv() {
        return naviStatCrntRdLvl_Inv;
    }

    public void setNaviStatCrntRdLvl_Inv(Integer naviStatCrntRdLvl_Inv) {
        this.naviStatCrntRdLvl_Inv = naviStatCrntRdLvl_Inv;
    }

    public Integer getNaviStatCrntRdMpConf() {
        return naviStatCrntRdMpConf;
    }

    public void setNaviStatCrntRdMpConf(Integer naviStatCrntRdMpConf) {
        this.naviStatCrntRdMpConf = naviStatCrntRdMpConf;
    }

    public Integer getNaviStatDistToHDStrt() {
        return naviStatDistToHDStrt;
    }

    public void setNaviStatDistToHDStrt(Integer naviStatDistToHDStrt) {
        this.naviStatDistToHDStrt = naviStatDistToHDStrt;
    }

    public Integer getNaviStatDistToHDStrt_Inv() {
        return naviStatDistToHDStrt_Inv;
    }

    public void setNaviStatDistToHDStrt_Inv(Integer naviStatDistToHDStrt_Inv) {
        this.naviStatDistToHDStrt_Inv = naviStatDistToHDStrt_Inv;
    }

    public Integer getNaviStatDistToViaPoint() {
        return naviStatDistToViaPoint;
    }

    public void setNaviStatDistToViaPoint(Integer naviStatDistToViaPoint) {
        this.naviStatDistToViaPoint = naviStatDistToViaPoint;
    }

    public Integer getNaviStatDistToViaPoint_Inv() {
        return naviStatDistToViaPoint_Inv;
    }

    public void setNaviStatDistToViaPoint_Inv(Integer naviStatDistToViaPoint_Inv) {
        this.naviStatDistToViaPoint_Inv = naviStatDistToViaPoint_Inv;
    }

    public Integer getNaviStatRmnDist() {
        return naviStatRmnDist;
    }

    public void setNaviStatRmnDist(Integer naviStatRmnDist) {
        this.naviStatRmnDist = naviStatRmnDist;
    }

    public Integer getNaviStatRmnDist_Inv() {
        return naviStatRmnDist_Inv;
    }

    public void setNaviStatRmnDist_Inv(Integer naviStatRmnDist_Inv) {
        this.naviStatRmnDist_Inv = naviStatRmnDist_Inv;
    }

    @Override
    public String toString() {
        return "{naviStatus=" + naviStat +
                ", onGuideRoad=" + naviStatCrntRdMpConf +
                ", roadClass_inv=" + naviStatCrntRdLvl_Inv +
                ", roadClass=" + naviStatCrntRdLvl +
                ", distVia_Inv=" + naviStatDistToViaPoint_Inv +
                ", distVia=" + naviStatDistToViaPoint +
                ", remainDist_Inv=" + naviStatRmnDist_Inv +
                ", remainDist=" + naviStatRmnDist +
                '}';
    }
}
