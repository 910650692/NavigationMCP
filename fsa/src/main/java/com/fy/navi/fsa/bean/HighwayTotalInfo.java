package com.fy.navi.fsa.bean;

public class HighwayTotalInfo {
    private int showType;
    private HighwayInfo highwayInfo;

    public HighwayTotalInfo() {
    }

    public HighwayTotalInfo(int showType, HighwayInfo highwayInfo) {
        this.showType = showType;
        this.highwayInfo = highwayInfo;
    }

    public HighwayInfo getHighwayInfo() {
        return highwayInfo;
    }

    public void setHighwayInfo(HighwayInfo highwayInfo) {
        this.highwayInfo = highwayInfo;
    }

    public int getShowType() {
        return showType;
    }

    public void setShowType(int showType) {
        this.showType = showType;
    }

    @Override
    public String toString() {
        return "HighwayTotalInfo{" +
                "showType=" + showType +
                ", highwayInfo=" + highwayInfo +
                '}';
    }
}
