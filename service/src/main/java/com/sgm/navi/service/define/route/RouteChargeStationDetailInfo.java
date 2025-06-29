package com.sgm.navi.service.define.route;


import android.os.Parcel;
import android.os.Parcelable;


import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteChargeStationDetailInfo implements Parcelable {
    private int mSegmentIdx;
    private short mDirection;
    private Coord2DDouble mShow;
    private Coord2DDouble mProjective;
    private String mPoiID;
    private String mName;
    private String mBrandName;
    private int mMaxPower;
    private short mChargePercent;
    private int mChargeTime;
    private int mRemainingCapacity;
    private double mRemainingPercent;
    private int mIndex;
    private int mInterval = -1;

    public RouteChargeStationDetailInfo() {
        this.mSegmentIdx = 0;
        this.mDirection = 0;
        this.mShow = new Coord2DDouble();
        this.mProjective = new Coord2DDouble();
        this.mPoiID = "";
        this.mName = "";
        this.mBrandName = "";
        this.mMaxPower = 0;
        this.mChargePercent = 0;
        this.mChargeTime = 0;
        this.mRemainingCapacity = 0;
        this.mRemainingPercent = 0.0;
        this.mIndex = -1;
    }

    public RouteChargeStationDetailInfo(final int segmentIdxLiteObj, final short directionLiteObj, final Coord2DDouble showLiteObj
            , final Coord2DDouble projectiveLiteObj, final String poiIDLiteObj, final String nameLiteObj, final String brandNameLiteObj
            , final int maxPowerLiteObj, final short chargePercentLiteObj, final int chargeTimeLiteObj, final int remainingCapacityLiteObj
            , final double remainingPercentLiteObj, final int indexLiteObj) {
        this.mSegmentIdx = segmentIdxLiteObj;
        this.mDirection = directionLiteObj;
        this.mShow = showLiteObj;
        this.mProjective = projectiveLiteObj;
        this.mPoiID = poiIDLiteObj;
        this.mName = nameLiteObj;
        this.mBrandName = brandNameLiteObj;
        this.mMaxPower = maxPowerLiteObj;
        this.mChargePercent = chargePercentLiteObj;
        this.mChargeTime = chargeTimeLiteObj;
        this.mRemainingCapacity = remainingCapacityLiteObj;
        this.mRemainingPercent = remainingPercentLiteObj;
        this.mIndex = indexLiteObj;
    }

    protected RouteChargeStationDetailInfo(Parcel in) {
        mSegmentIdx = in.readInt();
        mDirection = (short) in.readInt();
        mPoiID = in.readString();
        mName = in.readString();
        mBrandName = in.readString();
        mMaxPower = in.readInt();
        mChargePercent = (short) in.readInt();
        mChargeTime = in.readInt();
        mRemainingCapacity = in.readInt();
        mRemainingPercent = in.readDouble();
        mIndex = in.readInt();
        mInterval = in.readInt();
    }

    public static final Creator<RouteChargeStationDetailInfo> CREATOR = new Creator<RouteChargeStationDetailInfo>() {
        @Override
        public RouteChargeStationDetailInfo createFromParcel(Parcel in) {
            return new RouteChargeStationDetailInfo(in);
        }

        @Override
        public RouteChargeStationDetailInfo[] newArray(int size) {
            return new RouteChargeStationDetailInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeInt(mSegmentIdx);
        dest.writeInt((int) mDirection);
        dest.writeString(mPoiID);
        dest.writeString(mName);
        dest.writeString(mBrandName);
        dest.writeInt(mMaxPower);
        dest.writeInt((int) mChargePercent);
        dest.writeInt(mChargeTime);
        dest.writeInt(mRemainingCapacity);
        dest.writeDouble(mRemainingPercent);
        dest.writeInt(mIndex);
        dest.writeInt(mInterval);
    }
}