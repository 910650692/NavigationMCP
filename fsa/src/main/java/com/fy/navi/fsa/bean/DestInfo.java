package com.fy.navi.fsa.bean;

public class DestInfo {

    private String name;
    private String address;
    private GeoPoint location;

    public DestInfo() {}

    public DestInfo(String name, String address, GeoPoint location) {
        this.name = name;
        this.address = address;
        this.location = location;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
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
        return "DestInfo{" +
                "name='" + name + '\'' +
                ", address='" + address + '\'' +
                ", location=" + location +
                '}';
    }
}
