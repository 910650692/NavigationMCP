package com.fy.navi.fsa.bean;

public class ParkInfo {
    private GeoPoint location;
    private String name;

    public ParkInfo() {
    }

    public ParkInfo(GeoPoint location, String name) {
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
        return "ParkInfo{" +
                "location=" + location +
                ", name='" + name + '\'' +
                '}';
    }
}
