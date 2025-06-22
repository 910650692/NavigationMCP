package com.sgm.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseFsaPoiInfo implements Parcelable {
    private BaseGeoPoint mLocation;
    private String mName;

    public BaseFsaPoiInfo() {
    }

    public BaseFsaPoiInfo(BaseGeoPoint location, String name) {
        this.mLocation = location;
        this.mName = name;
    }

    protected BaseFsaPoiInfo(Parcel in) {
        mLocation = in.readParcelable(BaseGeoPoint.class.getClassLoader());
        mName = in.readString();
    }

    public static final Creator<BaseFsaPoiInfo> CREATOR = new Creator<BaseFsaPoiInfo>() {
        @Override
        public BaseFsaPoiInfo createFromParcel(Parcel in) {
            return new BaseFsaPoiInfo(in);
        }

        @Override
        public BaseFsaPoiInfo[] newArray(int size) {
            return new BaseFsaPoiInfo[size];
        }
    };

    public BaseGeoPoint getLocation() {
        return mLocation;
    }

    public void setLocation(BaseGeoPoint location) {
        this.mLocation = location;
    }

    public String getName() {
        return mName;
    }

    public void setName(String name) {
        this.mName = name;
    }

    @Override
    public String toString() {
        return "BaseFsaPoiInfo{" +
                "location=" + mLocation +
                ", name='" + mName + '\'' +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeParcelable(mLocation, flags);
        dest.writeString(mName);
    }
}
