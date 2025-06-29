package com.sgm.navi.service.define.route;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RoutePoint implements Parcelable {
    public boolean mIsDraw = true;
    public long mPathId = 0;
    public int mType;
    public GeoPoint mPos;
    public int mAddressType; // 地点类型: 0默认 , 1 替换补能点, 2 充电站（非补能规划）

    protected RoutePoint(Parcel in) {
        mIsDraw = in.readByte() != 0;
        mPathId = in.readLong();
        mType = in.readInt();
        mPos = in.readParcelable(GeoPoint.class.getClassLoader());
        mAddressType = in.readInt();
    }

    public static final Creator<RoutePoint> CREATOR = new Creator<RoutePoint>() {
        @Override
        public RoutePoint createFromParcel(Parcel in) {
            return new RoutePoint(in);
        }

        @Override
        public RoutePoint[] newArray(int size) {
            return new RoutePoint[size];
        }
    };

    @NonNull
    @Override
    public String toString() {
        return "RoutePoint{" +
                "mIsDraw=" + mIsDraw +
                ", mPathId=" + mPathId +
                ", mType=" + mType +
                ", mPos=" + mPos +
                '}';
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeByte((byte) (mIsDraw ? 1 : 0));
        dest.writeLong(mPathId);
        dest.writeInt(mType);
        dest.writeParcelable(mPos, flags);
        dest.writeInt(mAddressType);
    }
}
