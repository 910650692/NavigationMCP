package com.sgm.navi.service.define.route;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlterChargeStationInfo implements Parcelable {
    private String mPoiId;
    private String mName;
    private Coord3DDouble mPos;
    private int mRemainingCapacity;
    private double mRemainingPercent;
    private int mChargeTime;
    private int mChildType;
    private ArrayList<String> mTagInfos;
    private RouteChargeStationNumberInfo mSuperPlugInfo;
    private RouteChargeStationNumberInfo mFastPlugInfo;
    private RouteChargeStationNumberInfo mSlowPlugInfo;
    private RouteAlterChargePriceInfo mPriceInfo;
    private RouteAlternativeChargeDetourInfo mDetourInfo;
    //替换补能界面-距离参数
    private int mDistance;

    public RouteAlterChargeStationInfo() {
        this.mPoiId = "";
        this.mName = "";
        this.mPos = new com.sgm.navi.service.define.route.Coord3DDouble();
        this.mRemainingCapacity = 0;
        this.mRemainingPercent = 0.0;
        this.mChargeTime = 0;
        this.mChildType = 0;
        this.mTagInfos = new ArrayList();
        this.mSuperPlugInfo = new RouteChargeStationNumberInfo();
        this.mFastPlugInfo = new RouteChargeStationNumberInfo();
        this.mSlowPlugInfo = new RouteChargeStationNumberInfo();
        this.mPriceInfo = new RouteAlterChargePriceInfo();
        this.mDetourInfo = new RouteAlternativeChargeDetourInfo();
    }

    public RouteAlterChargeStationInfo(final String poiIdLiteObj, final String nameLiteObj, final Coord3DDouble posLiteObj
            , final int remainingCapacityLiteObj, final double remainingPercentLiteObj, final int chargeTimeLiteObj
            , final int childTypeLiteObj, final ArrayList<String> tagInfosLiteObj
            , final RouteChargeStationNumberInfo superPlugInfoLiteObj, final RouteChargeStationNumberInfo fastPlugInfoLiteObj
            , final RouteChargeStationNumberInfo slowPlugInfoLiteObj, final RouteAlterChargePriceInfo priceInfoLiteObj
            , final RouteAlternativeChargeDetourInfo detourInfoLiteObj) {
        this.mPoiId = poiIdLiteObj;
        this.mName = nameLiteObj;
        this.mPos = posLiteObj;
        this.mRemainingCapacity = remainingCapacityLiteObj;
        this.mRemainingPercent = remainingPercentLiteObj;
        this.mChargeTime = chargeTimeLiteObj;
        this.mChildType = childTypeLiteObj;
        this.mTagInfos = tagInfosLiteObj;
        this.mSuperPlugInfo = superPlugInfoLiteObj;
        this.mFastPlugInfo = fastPlugInfoLiteObj;
        this.mSlowPlugInfo = slowPlugInfoLiteObj;
        this.mPriceInfo = priceInfoLiteObj;
        this.mDetourInfo = detourInfoLiteObj;
    }

    protected RouteAlterChargeStationInfo(Parcel in) {
        mPoiId = in.readString();
        mName = in.readString();
        mPos = in.readParcelable(Coord3DDouble.class.getClassLoader());
        mRemainingCapacity = in.readInt();
        mRemainingPercent = in.readDouble();
        mChargeTime = in.readInt();
        mChildType = in.readInt();
        mTagInfos = in.createStringArrayList();
        mPriceInfo = in.readParcelable(RouteAlterChargePriceInfo.class.getClassLoader());
        mDistance = in.readInt();
    }

    public static final Creator<RouteAlterChargeStationInfo> CREATOR = new Creator<RouteAlterChargeStationInfo>() {
        @Override
        public RouteAlterChargeStationInfo createFromParcel(Parcel in) {
            return new RouteAlterChargeStationInfo(in);
        }

        @Override
        public RouteAlterChargeStationInfo[] newArray(int size) {
            return new RouteAlterChargeStationInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeString(mPoiId);
        dest.writeString(mName);
        dest.writeParcelable(mPos, flags);
        dest.writeInt(mRemainingCapacity);
        dest.writeDouble(mRemainingPercent);
        dest.writeInt(mChargeTime);
        dest.writeInt(mChildType);
        dest.writeStringList(mTagInfos);
        dest.writeParcelable(mPriceInfo, flags);
        dest.writeInt(mDistance);
    }
}
