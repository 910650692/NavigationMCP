package com.sgm.navi.service.define.route;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlterChargePriceInfo implements Parcelable {
    private String mLowestPriceValue;
    private String mLowestPriceUnit;

    public RouteAlterChargePriceInfo() {
        this.mLowestPriceValue = "";
        this.mLowestPriceUnit = "";
    }

    public RouteAlterChargePriceInfo(final String lowestPriceValueLiteObj, final String lowestPriceUnitLiteObj) {
        this.mLowestPriceValue = lowestPriceValueLiteObj;
        this.mLowestPriceUnit = lowestPriceUnitLiteObj;
    }

    protected RouteAlterChargePriceInfo(Parcel in) {
        mLowestPriceValue = in.readString();
        mLowestPriceUnit = in.readString();
    }

    public static final Creator<RouteAlterChargePriceInfo> CREATOR = new Creator<RouteAlterChargePriceInfo>() {
        @Override
        public RouteAlterChargePriceInfo createFromParcel(Parcel in) {
            return new RouteAlterChargePriceInfo(in);
        }

        @Override
        public RouteAlterChargePriceInfo[] newArray(int size) {
            return new RouteAlterChargePriceInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(mLowestPriceValue);
        dest.writeString(mLowestPriceUnit);
    }
}