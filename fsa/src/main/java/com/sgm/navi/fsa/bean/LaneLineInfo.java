package com.sgm.navi.fsa.bean;

/**
 * 获取车道线信息
 */
public class LaneLineInfo {
    /**
     * 导航信息显示状态
     * -1 – 无效值
     * 0 – 展示
     * 1 – 更新
     * 2 – 隐藏
     */
    private int showType;
    /**
     * 车道线详细信息
     */
    private LaneInfo laneInfo;

    public int getShowType() {
        return showType;
    }

    public void setShowType(int showType) {
        this.showType = showType;
    }

    public LaneInfo getLaneInfo() {
        return laneInfo;
    }

    public void setLaneInfo(LaneInfo laneInfo) {
        this.laneInfo = laneInfo;
    }


}
