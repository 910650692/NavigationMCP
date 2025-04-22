package com.fy.navi.fsa.bean;

import java.util.ArrayList;

public class LaneInfo {
    /**
     * 到车道线的剩余距离
     */
    private int remainDistance;
    /**
     * 当前车道线的位置
     */
    private GeoPoint position;
    /**
     * 车道线信息
     */
    private ArrayList<LaneItem> itemList;

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public GeoPoint getPosition() {
        return position;
    }

    public void setPosition(GeoPoint position) {
        this.position = position;
    }

    public ArrayList<LaneItem> getItemList() {
        return itemList;
    }

    public void setItemList(ArrayList<LaneItem> itemList) {
        this.itemList = itemList;
    }

    @Override
    public String toString() {
        return "LaneInfo{" +
                "remainDistance=" + remainDistance +
                ", position=" + position +
                ", itemList=" + itemList +
                '}';
    }
}
