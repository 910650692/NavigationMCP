package com.sgm.navi.fsa.bean;

import java.util.ArrayList;

/**
 * 车道线信息
 */
public class LaneItem {
    /**
     * 车道线类型信息
     */
    private LaneTypeInfo laneTypeInfo;
    /**
     * 车道线数量变化类型
     * 0 – 无变化
     * 1 – 增加
     * 2 – 减少
     */
    private int laneVariationType;
    /**
     * 车道线方向信息
     */
    private ArrayList<LaneDirection> directionList;

    public LaneTypeInfo getLaneTypeInfo() {
        return laneTypeInfo;
    }

    public void setLaneTypeInfo(LaneTypeInfo laneTypeInfo) {
        this.laneTypeInfo = laneTypeInfo;
    }

    public int getLaneVariationType() {
        return laneVariationType;
    }

    public void setLaneVariationType(int laneVariationType) {
        this.laneVariationType = laneVariationType;
    }

    public ArrayList<LaneDirection> getDirectionList() {
        return directionList;
    }

    public void setDirectionList(ArrayList<LaneDirection> directionList) {
        this.directionList = directionList;
    }

    @Override
    public String toString() {
        return "LaneItem{" +
                "laneTypeInfo=" + laneTypeInfo +
                ", laneVariationType=" + laneVariationType +
                ", directionList=" + directionList +
                '}';
    }
}
