package com.sgm.navi.service.define.route;

import java.io.Serializable;

public class Coord3DDouble implements Serializable {
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

    public double getZ() {
        return mZ;
    }

    public void setZ(final double z) {
        this.mZ = z;
    }

    private double mLon;
    private double mLat;
    private double mZ;

    public Coord3DDouble() {
        this.mLon = 0.0;
        this.mLat = 0.0;
        this.mZ = 0.0;
    }

    public Coord3DDouble(final double lonLiteObj, final double latLiteObj, final double liteObj) {
        this.mLon = lonLiteObj;
        this.mLat = latLiteObj;
        this.mZ = liteObj;
    }
}
