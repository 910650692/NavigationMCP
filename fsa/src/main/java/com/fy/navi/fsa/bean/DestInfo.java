package com.fy.navi.fsa.bean;

/**
 * 1.1.7、获取导航目的地的名称和坐标
 */
public class DestInfo {
    /**
     * 目的地名称
     */
    private String name;
    /**
     * 目的地地址
     */
    private String address;
    /**
     * 目的地经纬度
     */
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
