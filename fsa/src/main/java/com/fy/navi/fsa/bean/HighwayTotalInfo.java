package com.fy.navi.fsa.bean;

/**
 * 高速路信息
 */
public class HighwayTotalInfo {
    /**
     * 导航信息显示状态
     * -1 – 无效值
     * 0 – 展示
     * 1 – 更新
     * 2 – 隐藏
     */
    private int showType;
    /**
     * 高速路信息
     */
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
