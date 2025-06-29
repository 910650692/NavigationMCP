package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlternativeChargeDetourInfo implements Parcelable {
    private int distance;
    private int time;

    public RouteAlternativeChargeDetourInfo() {
        this.distance = Integer.MAX_VALUE;
        this.time = Integer.MAX_VALUE;
    }

    public RouteAlternativeChargeDetourInfo(final int distanceLiteObj, final int timeLiteObj) {
        this.distance = distanceLiteObj;
        this.time = timeLiteObj;
    }

    protected RouteAlternativeChargeDetourInfo(Parcel in) {
        distance = in.readInt();
        time = in.readInt();
    }

    public static final Creator<RouteAlternativeChargeDetourInfo> CREATOR = new Creator<RouteAlternativeChargeDetourInfo>() {
        @Override
        public RouteAlternativeChargeDetourInfo createFromParcel(Parcel in) {
            return new RouteAlternativeChargeDetourInfo(in);
        }

        @Override
        public RouteAlternativeChargeDetourInfo[] newArray(int size) {
            return new RouteAlternativeChargeDetourInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(distance);
        dest.writeInt(time);
    }
}
