package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.SerializedName;

public class BaseGeoPoint implements Parcelable {

    private double lon;
    private double lat;
    private double z;

    public static final Creator<BaseGeoPoint> CREATOR = new Creator<BaseGeoPoint>() {
        @Override
        public BaseGeoPoint createFromParcel(Parcel source) {
            return new BaseGeoPoint(source);
        }

        @Override
        public BaseGeoPoint[] newArray(int size) {
            return new BaseGeoPoint[size];
        }
    };

    public BaseGeoPoint(Parcel in) {
        lon = in.readDouble();
        lat = in.readDouble();
        z = in.readDouble();
    }

    public BaseGeoPoint() {
        lat = 0.0;
        lon = 0.0;
        z = 0.0;
    }

    public BaseGeoPoint(double lon, double lat) {
        this.lon = lon;
        this.lat = lat;
        z = 0.0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeDouble(lon);
        dest.writeDouble(lat);
        dest.writeDouble(z);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "GeoPoint{" +
                "lon=" + lon +
                ", lat=" + lat +
                '}';
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

    public double getZ() {
        return z;
    }

    public void setZ(double z) {
        this.z = z;
    }
}
