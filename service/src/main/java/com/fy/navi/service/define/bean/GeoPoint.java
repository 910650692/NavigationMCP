package com.fy.navi.service.define.bean;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/8
 */
public class GeoPoint implements Parcelable {
    public double lon;
    public double lat;
    public double z;

    public GeoPoint() {
    }

    public GeoPoint(double lon, double lat) {
        this.lon = lon;
        this.lat = lat;
    }

    public GeoPoint(double lon, double lat, double z) {
        this.lon = lon;
        this.lat = lat;
        this.z = z;
    }

    protected GeoPoint(Parcel in) {
        lon = in.readDouble();
        lat = in.readDouble();
        z = in.readDouble();
    }

    public static final Creator<GeoPoint> CREATOR = new Creator<GeoPoint>() {
        @Override
        public GeoPoint createFromParcel(Parcel in) {
            return new GeoPoint(in);
        }

        @Override
        public GeoPoint[] newArray(int size) {
            return new GeoPoint[size];
        }
    };

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

    @Override
    public String toString() {
        return "GeoPoint{" +
                "lon=" + lon +
                ", lat=" + lat +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeDouble(lon);
        parcel.writeDouble(lat);
        parcel.writeDouble(z);
    }
}
