package com.fy.navi.fsa.bean;

/**
 * 位置
 */
public class GeoPoint {
    //经度
    private double lng;
    //纬度
    private double lat;

    public GeoPoint() {}

    public GeoPoint(double lng, double lat) {
        this.lng = lng;
        this.lat = lat;
    }

    public double getLng() {
        return lng;
    }

    public void setLng(double lng) {
        this.lng = lng;
    }

    public double getLat() {
        return lat;
    }

    public void setLat(double lat) {
        this.lat = lat;
    }

    @Override
    public String toString() {
        return "GeoPoint{" +
                "lng=" + lng +
                ", lat=" + lat +
                '}';
    }
}
