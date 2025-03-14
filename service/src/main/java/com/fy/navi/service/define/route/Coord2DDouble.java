package com.fy.navi.service.define.route;

public class Coord2DDouble {
    public double lon;
    public double lat;

    public Coord2DDouble() {
        this.lon = 0.0;
        this.lat = 0.0;
    }

    public Coord2DDouble(double lonLiteObj, double latLiteObj) {
        this.lon = lonLiteObj;
        this.lat = latLiteObj;
    }
}
