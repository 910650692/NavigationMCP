package com.fy.navi.service.define.bean;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;
public class GeoPoint implements Parcelable {
    private double mLon;
    private double mLat;
    private double mZ;

    public GeoPoint() {
    }

    public GeoPoint(final double lon, final double lat) {
        this.mLon = lon;
        this.mLat = lat;
    }

    public GeoPoint(final double lon, final double lat, final double z) {
        this.mLon = lon;
        this.mLat = lat;
        this.mZ = z;
    }

    protected GeoPoint(final Parcel in) {
        mLon = in.readDouble();
        mLat = in.readDouble();
        mZ = in.readDouble();
    }

    public static final Creator<GeoPoint> CREATOR = new Creator<GeoPoint>() {
        @Override
        public GeoPoint createFromParcel(final Parcel in) {
            return new GeoPoint(in);
        }

        @Override
        public GeoPoint[] newArray(final int size) {
            return new GeoPoint[size];
        }
    };

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

    @Override
    public String toString() {
        return "GeoPoint{" +
                "lon=" + mLon +
                ", lat=" + mLat +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(final @NonNull Parcel parcel, final int i) {
        parcel.writeDouble(mLon);
        parcel.writeDouble(mLat);
        parcel.writeDouble(mZ);
    }
}
