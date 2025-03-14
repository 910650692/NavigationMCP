package com.fy.navi.service.define.route;

public class Coord3DDouble {
    public double lon;
    public double lat;
    public double z;

    public Coord3DDouble() {
        this.lon = 0.0;
        this.lat = 0.0;
        this.z = 0.0;
    }

    public Coord3DDouble(double lonLiteObj, double latLiteObj, double zLiteObj) {
        this.lon = lonLiteObj;
        this.lat = latLiteObj;
        this.z = zLiteObj;
    }
}
