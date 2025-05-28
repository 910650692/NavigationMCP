package com.fy.navi.service.define.signal;

public class SdNavigationStatusGroup {
    private Integer naviStat; // 导航状态
    private Integer naviStatCrntRdLvl; // 道路等级
    private Integer naviStatCrntRdLvl_Inv; // 道路等级是否有效
    private Integer naviStatCrntRdMpConf; // 道路匹配置信度
    private Integer naviStatDistToHDStrt; // HD起点距离
    private Integer naviStatDistToHDStrt_Inv; // HD起点距离是否有效
    private Integer naviStatDistToViaPoint; // 途径点距离
    private Integer naviStatDistToViaPoint_Inv; // 途径点距离是否有效
    private Integer naviStatRmnDist; // 剩余距离
    private Integer naviStatRmnDist_Inv; // 剩余距离是否有效

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
        return "SdNavigationStatusGroup{" +
                "naviStat=" + naviStat +
                ", naviStatCrntRdLvl=" + naviStatCrntRdLvl +
                ", naviStatCrntRdLvl_Inv=" + naviStatCrntRdLvl_Inv +
                ", naviStatCrntRdMpConf=" + naviStatCrntRdMpConf +
                ", naviStatDistToHDStrt=" + naviStatDistToHDStrt +
                ", naviStatDistToHDStrt_Inv=" + naviStatDistToHDStrt_Inv +
                ", naviStatDistToViaPoint=" + naviStatDistToViaPoint +
                ", naviStatDistToViaPoint_Inv=" + naviStatDistToViaPoint_Inv +
                ", naviStatRmnDist=" + naviStatRmnDist +
                ", naviStatRmnDist_Inv=" + naviStatRmnDist_Inv +
                '}';
    }
}
