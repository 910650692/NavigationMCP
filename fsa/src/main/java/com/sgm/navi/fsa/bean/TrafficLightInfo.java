package com.sgm.navi.fsa.bean;

public class TrafficLightInfo {
    private int distanceTrafficLight;
    private int trafficLightColor;
    private int trafficLightTime;
    private int trafficLightAssignment;

    public int getDistanceTrafficLight() {
        return distanceTrafficLight;
    }

    public void setDistanceTrafficLight(int distanceTrafficLight) {
        this.distanceTrafficLight = distanceTrafficLight;
    }

    public int getTrafficLightColor() {
        return trafficLightColor;
    }

    public void setTrafficLightColor(int trafficLightColor) {
        this.trafficLightColor = trafficLightColor;
    }

    public int getTrafficLightTime() {
        return trafficLightTime;
    }

    public void setTrafficLightTime(int trafficLightTime) {
        this.trafficLightTime = trafficLightTime;
    }

    public int getTrafficLightAssignment() {
        return trafficLightAssignment;
    }

    public void setTrafficLightAssignment(int trafficLightAssignment) {
        this.trafficLightAssignment = trafficLightAssignment;
    }

    @Override
    public String toString() {
        return "TrafficLightInfo{" +
                "distanceTrafficLight=" + distanceTrafficLight +
                ", trafficLightColor=" + trafficLightColor +
                ", trafficLightTime=" + trafficLightTime +
                ", trafficLightAssignment=" + trafficLightAssignment +
                '}';
    }
}
