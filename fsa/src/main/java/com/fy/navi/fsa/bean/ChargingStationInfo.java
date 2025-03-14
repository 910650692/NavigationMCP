package com.fy.navi.fsa.bean;

public class ChargingStationInfo {
    private GeoPoint location;
    private String name;

    public ChargingStationInfo() {
    }

    public ChargingStationInfo(GeoPoint location, String name) {
        this.location = location;
        this.name = name;
    }

    public GeoPoint getLocation() {
        return location;
    }

    public void setLocation(GeoPoint location) {
        this.location = location;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return "ChargingStationInfo{" +
                "location=" + location +
                ", name='" + name + '\'' +
                '}';
    }
}
