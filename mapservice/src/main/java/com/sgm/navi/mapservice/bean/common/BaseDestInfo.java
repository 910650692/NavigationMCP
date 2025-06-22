package com.sgm.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseDestInfo implements Parcelable {
    /**
     * 目的地名称
     */
    private String mName;
    /**
     * 目的地地址
     */
    private String mAddress;
    /**
     * 目的地经纬度
     */
    private BaseGeoPoint mLocation;

    public BaseDestInfo() {

    }

    public BaseDestInfo(final String name, final String address, final BaseGeoPoint location) {
        this.mName = name;
        this.mAddress = address;
        this.mLocation = location;
    }

    protected BaseDestInfo(final Parcel in) {
        mName = in.readString();
        mAddress = in.readString();
        mLocation = in.readParcelable(BaseGeoPoint.class.getClassLoader());
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeString(mName);
        dest.writeString(mAddress);
        dest.writeParcelable(mLocation, flags);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public static final Creator<BaseDestInfo> CREATOR = new Creator<BaseDestInfo>() {
        @Override
        public BaseDestInfo createFromParcel(final Parcel in) {
            return new BaseDestInfo(in);
        }

        @Override
        public BaseDestInfo[] newArray(final int size) {
            return new BaseDestInfo[size];
        }
    };

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(final String address) {
        this.mAddress = address;
    }

    public BaseGeoPoint getLocation() {
        return mLocation;
    }

    public void setLocation(final BaseGeoPoint location) {
        this.mLocation = location;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        this.mName = name;
    }

    @Override
    public String toString() {
        return "DestInfo{" +
                "name='" + mName + '\'' +
                ", address='" + mAddress + '\'' +
                ", location=" + mLocation +
                '}';
    }
}

