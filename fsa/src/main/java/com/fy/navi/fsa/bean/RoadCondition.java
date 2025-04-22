package com.fy.navi.fsa.bean;

/**
 * 1.1.16、获取路况数据
 */
public class RoadCondition {
    /**
     * 当前路段的路况类型
     * 0 – 不合法数据
     * 1 – 路况良好
     * 2 – 轻微拥堵
     * 3 – 拥堵
     * 4 – 严重拥堵
     */
    private int roadConditionType;
    /**
     * 当前路段的长度，单位：米
     */
    private int distance;
    /**
     * 通过这段路的预计时间，单位：秒
     */
    private int travelTime;

    public int getRoadConditionType() {
        return roadConditionType;
    }

    public void setRoadConditionType(int roadConditionType) {
        this.roadConditionType = roadConditionType;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public int getTravelTime() {
        return travelTime;
    }

    public void setTravelTime(int travelTime) {
        this.travelTime = travelTime;
    }
}
