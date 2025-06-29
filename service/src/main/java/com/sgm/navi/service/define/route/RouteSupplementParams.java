package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RouteSupplementParams implements Parcelable {
    //补能点总时间
    private float mTotalDistance;
    //补能点信息
    private ArrayList<RouteSupplementInfo> mRouteSupplementInfos;

    protected RouteSupplementParams(Parcel in) {
        mTotalDistance = in.readFloat();
        mRouteSupplementInfos = in.createTypedArrayList(RouteSupplementInfo.CREATOR);
    }

    public static final Creator<RouteSupplementParams> CREATOR = new Creator<RouteSupplementParams>() {
        @Override
        public RouteSupplementParams createFromParcel(Parcel in) {
            return new RouteSupplementParams(in);
        }

        @Override
        public RouteSupplementParams[] newArray(int size) {
            return new RouteSupplementParams[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeFloat(mTotalDistance);
        dest.writeTypedList(mRouteSupplementInfos);
    }
}
