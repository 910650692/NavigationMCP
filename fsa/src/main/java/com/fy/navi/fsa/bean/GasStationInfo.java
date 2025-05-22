package com.fy.navi.fsa.bean;

public class GasStationInfo {
    //位置
    private GeoPoint location;
    //名字
    private String name;

    public GasStationInfo() {
    }

    public GasStationInfo(GeoPoint location, String name) {
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
        return "GasStationInfo{" +
                "location=" + location +
                ", name='" + name + '\'' +
                '}';
    }
}
