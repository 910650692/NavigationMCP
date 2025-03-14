package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class LaneItem {
    private LaneTypeInfo laneTypeInfo;
    private int laneVariationType;
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
