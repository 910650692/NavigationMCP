package com.sgm.navi.fsa.bean;

public class PoiInfoForExport {
    private GeoPoint location;
    private String name;

    public PoiInfoForExport() {
    }

    public PoiInfoForExport(GeoPoint location, String name) {
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
        return "PoiInfoForExport{" +
                "location=" + location +
                ", name='" + name + '\'' +
                '}';
    }
}
