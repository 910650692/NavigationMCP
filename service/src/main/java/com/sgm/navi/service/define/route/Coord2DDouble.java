package com.sgm.navi.service.define.route;

import java.io.Serializable;

public class Coord2DDouble implements Serializable {
    public double getLon() {
        return mLon;
    }

    public void setLon(final double lon) {
        this.mLon = lon;
    }

    public double getLat() {
        return mLat;
    }

    public void setLat(final double lat) {
        this.mLat = lat;
    }

    private double mLon;
    private double mLat;

    public Coord2DDouble() {
        this.mLon = 0.0;
        this.mLat = 0.0;
    }

    public Coord2DDouble(final double lonLiteObj, final double latLiteObj) {
        this.mLon = lonLiteObj;
        this.mLat = latLiteObj;
    }
}
