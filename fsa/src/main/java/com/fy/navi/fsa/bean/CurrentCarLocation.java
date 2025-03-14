package com.fy.navi.fsa.bean;

public class CurrentCarLocation {
    private GeoPoint location;

    public CurrentCarLocation() {
    }

    public CurrentCarLocation(GeoPoint location) {
        this.location = location;
    }

    public GeoPoint getLocation() {
        return location;
    }

    public void setLocation(GeoPoint location) {
        this.location = location;
    }

    @Override
    public String toString() {
        return "CurrentCarLocation{" +
                "location=" + location +
                '}';
    }
}
