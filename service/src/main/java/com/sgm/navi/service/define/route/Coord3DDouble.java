package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

public class Coord3DDouble implements Parcelable {
    protected Coord3DDouble(Parcel in) {
        mLon = in.readDouble();
        mLat = in.readDouble();
        mZ = in.readDouble();
    }

    public static final Creator<Coord3DDouble> CREATOR = new Creator<Coord3DDouble>() {
        @Override
        public Coord3DDouble createFromParcel(Parcel in) {
            return new Coord3DDouble(in);
        }

        @Override
        public Coord3DDouble[] newArray(int size) {
            return new Coord3DDouble[size];
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

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeDouble(mLon);
        dest.writeDouble(mLat);
        dest.writeDouble(mZ);
    }
}
