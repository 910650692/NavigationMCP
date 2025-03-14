package com.fy.navi.fsa.bean;

public class HighWayServiceAreaInfo {
    private String name;
    private int duration;
    private int distance;
    private GeoPoint position;
    private int serviceTypes;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public GeoPoint getPosition() {
        return position;
    }

    public void setPosition(GeoPoint position) {
        this.position = position;
    }

    public int getServiceTypes() {
        return serviceTypes;
    }

    public void setServiceTypes(int serviceTypes) {
        this.serviceTypes = serviceTypes;
    }

    @Override
    public String toString() {
        return "HighWayServiceAreaInfo{" +
                "name='" + name + '\'' +
                ", duration=" + duration +
                ", distance=" + distance +
                ", position=" + position +
                ", serviceTypes=" + serviceTypes +
                '}';
    }
}
