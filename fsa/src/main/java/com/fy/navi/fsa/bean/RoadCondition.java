package com.fy.navi.fsa.bean;

public class RoadCondition {
    private int roadConditionType;
    private int distance;
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
