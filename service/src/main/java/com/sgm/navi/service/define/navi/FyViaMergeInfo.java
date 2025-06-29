package com.sgm.navi.service.define.navi;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;


import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyViaMergeInfo implements Parcelable {
    private String poiName;
    private GeoPoint geoPoint;

    protected FyViaMergeInfo(Parcel in) {
        poiName = in.readString();
        geoPoint = in.readParcelable(GeoPoint.class.getClassLoader());
    }

    public static final Creator<FyViaMergeInfo> CREATOR = new Creator<FyViaMergeInfo>() {
        @Override
        public FyViaMergeInfo createFromParcel(Parcel in) {
            return new FyViaMergeInfo(in);
        }

        @Override
        public FyViaMergeInfo[] newArray(int size) {
            return new FyViaMergeInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(poiName);
        dest.writeParcelable(geoPoint, flags);
    }
}
