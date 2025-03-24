package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseGeoPoint implements Parcelable {

    private double mLon;
    private double mLat;
    private double mZ;

    public static final Creator<BaseGeoPoint> CREATOR = new Creator<BaseGeoPoint>() {
        @Override
        public BaseGeoPoint createFromParcel(final Parcel source) {
            return new BaseGeoPoint(source);
        }

        @Override
        public BaseGeoPoint[] newArray(final int size) {
            return new BaseGeoPoint[size];
        }
    };

    public BaseGeoPoint(final Parcel in) {
        mLon = in.readDouble();
        mLat = in.readDouble();
        mZ = in.readDouble();
    }

    public BaseGeoPoint() {
        mLon = 0.0;
        mLat = 0.0;
        mZ = 0.0;
    }

    public BaseGeoPoint(final double lon, final double lat) {
        mLon = lon;
        mLat = lat;
        mZ = 0.0;
    }

    public BaseGeoPoint(final double lon, final double lat, final double z) {
        mLon = lon;
        mLat = lat;
        mZ = z;
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeDouble(mLon);
        dest.writeDouble(mLat);
        dest.writeDouble(mZ);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "GeoPoint{" +
                "lon=" + mLon +
                ", lat=" + mLat +
                '}';
    }

    public double getLon() {
        return mLon;
    }

    public void setLon(final double lon) {
        mLon = lon;
    }

    public double getLat() {
        return mLat;
    }

    public void setLat(final double lat) {
        mLat = lat;
    }

    public double getZ() {
        return mZ;
    }

    public void setZ(final double z) {
        mZ = z;
    }
}
