package com.fy.navi.fsa.bean;

public class ServiceAreaInfo {
    private GeoPoint location;
    private String name;

    public ServiceAreaInfo() {
    }

    public ServiceAreaInfo(GeoPoint location, String name) {
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
        return "ServiceAreaInfo{" +
                "location=" + location +
                ", name='" + name + '\'' +
                '}';
    }
}
