package com.sgm.navi.fsa.bean;

/**
 * 服务区信息
 */
public class ServiceAreaInfo {
    /**
     * 位置
     */
    private GeoPoint location;
    /**
     * 服务区名字
     */
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
