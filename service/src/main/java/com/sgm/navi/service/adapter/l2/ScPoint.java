package com.sgm.navi.service.adapter.l2;

public class ScPoint {
    private double lon;
    private double lat;
    private double dist;

    public ScPoint() {
    }

    public ScPoint(double lon, double lat, double dist) {
        this.lon = lon;
        this.lat = lat;
        this.dist = dist;
    }

    public double getLon() {
        return lon;
    }

    public void setLon(double lon) {
        this.lon = lon;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    public double getDist() {
        return dist;
    }

    public void setDist(double dist) {
        this.dist = dist;
    }
}
