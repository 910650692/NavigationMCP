package com.sgm.navi.service.define.navi;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class FyEnergyEndPoint implements Parcelable {
    // 	电量耗尽点位置
    private GeoPoint show;
    private short segmentIdx;
    private short linkIndex;

    protected FyEnergyEndPoint(Parcel in) {
        show = in.readParcelable(GeoPoint.class.getClassLoader());
        segmentIdx = (short) in.readInt();
        linkIndex = (short) in.readInt();
    }

    public static final Creator<FyEnergyEndPoint> CREATOR = new Creator<FyEnergyEndPoint>() {
        @Override
        public FyEnergyEndPoint createFromParcel(Parcel in) {
            return new FyEnergyEndPoint(in);
        }

        @Override
        public FyEnergyEndPoint[] newArray(int size) {
            return new FyEnergyEndPoint[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeParcelable(show, flags);
        dest.writeInt((int) segmentIdx);
        dest.writeInt((int) linkIndex);
    }
}
