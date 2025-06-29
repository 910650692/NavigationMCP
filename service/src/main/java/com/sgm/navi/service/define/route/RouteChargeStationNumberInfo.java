package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteChargeStationNumberInfo implements Parcelable {
    private String mTotalNumber;
    private String mMinPower;
    private String mMaxPower;

    public RouteChargeStationNumberInfo() {
        this.mTotalNumber = "";
        this.mMinPower = "";
        this.mMaxPower = "";
    }

    public RouteChargeStationNumberInfo(final String totalNumberLiteObj, final String minPowerLiteObj, final String maxPowerLiteObj) {
        this.mTotalNumber = totalNumberLiteObj;
        this.mMinPower = minPowerLiteObj;
        this.mMaxPower = maxPowerLiteObj;
    }

    protected RouteChargeStationNumberInfo(Parcel in) {
        mTotalNumber = in.readString();
        mMinPower = in.readString();
        mMaxPower = in.readString();
    }

    public static final Creator<RouteChargeStationNumberInfo> CREATOR = new Creator<RouteChargeStationNumberInfo>() {
        @Override
        public RouteChargeStationNumberInfo createFromParcel(Parcel in) {
            return new RouteChargeStationNumberInfo(in);
        }

        @Override
        public RouteChargeStationNumberInfo[] newArray(int size) {
            return new RouteChargeStationNumberInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(mTotalNumber);
        dest.writeString(mMinPower);
        dest.writeString(mMaxPower);
    }
}