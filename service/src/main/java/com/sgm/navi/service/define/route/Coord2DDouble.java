package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;


public class Coord2DDouble implements Parcelable {
    protected Coord2DDouble(Parcel in) {
        mLon = in.readDouble();
        mLat = in.readDouble();
    }

    public static final Creator<Coord2DDouble> CREATOR = new Creator<Coord2DDouble>() {
        @Override
        public Coord2DDouble createFromParcel(Parcel in) {
            return new Coord2DDouble(in);
        }

        @Override
        public Coord2DDouble[] newArray(int size) {
            return new Coord2DDouble[size];
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

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeDouble(mLon);
        dest.writeDouble(mLat);
    }
}
