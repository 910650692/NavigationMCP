package com.fy.navi.vrbridge.bean;

public class MapLocation {
    private String mProvider;
    private int mSpeed;
    private int mBearing;
    private double mLon;
    private double mLat;

    public String getProvider() {
        return mProvider;
    }

    public void setProvider(final String provider) {
        this.mProvider = provider;
    }

    public int getSpeed() {
        return mSpeed;
    }

    public void setSpeed(final int speed) {
        this.mSpeed = speed;
    }

    public int getBearing() {
        return mBearing;
    }

    public void setBearing(final int bearing) {
        this.mBearing = bearing;
    }

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
}
