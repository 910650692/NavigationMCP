package com.sgm.navi.service.define.route;


import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestAreaDetailsInfo  implements Parcelable {
    private long mRemainDist;
    private long mRemainTime;
    private String mServiceName;
    private String mServicePOIID;
    private Coord2DDouble mPos;
    private long mSapaDetail;
    private boolean mIsAdded;

    public RouteRestAreaDetailsInfo() {
        this.mRemainDist = 0L;
        this.mRemainTime = 0L;
        this.mServiceName = "";
        this.mServicePOIID = "";
        this.mPos = new Coord2DDouble();
        this.mSapaDetail = 0L;
        this.mIsAdded = false;
    }

    public RouteRestAreaDetailsInfo(final long remainDistLiteObj, final long remainTimeLiteObj, final String serviceNameLiteObj
            , final String servicePOIIDLiteObj, final Coord2DDouble posLiteObj, final long sapaDetailLiteObj
            , final boolean isAddedObj) {
        this.mRemainDist = remainDistLiteObj;
        this.mRemainTime = remainTimeLiteObj;
        this.mServiceName = serviceNameLiteObj;
        this.mServicePOIID = servicePOIIDLiteObj;
        this.mPos = posLiteObj;
        this.mSapaDetail = sapaDetailLiteObj;
        this.mIsAdded = isAddedObj;
    }

    protected RouteRestAreaDetailsInfo(Parcel in) {
        mRemainDist = in.readLong();
        mRemainTime = in.readLong();
        mServiceName = in.readString();
        mServicePOIID = in.readString();
        mSapaDetail = in.readLong();
        mIsAdded = in.readByte() != 0;
    }

    public static final Creator<RouteRestAreaDetailsInfo> CREATOR = new Creator<RouteRestAreaDetailsInfo>() {
        @Override
        public RouteRestAreaDetailsInfo createFromParcel(Parcel in) {
            return new RouteRestAreaDetailsInfo(in);
        }

        @Override
        public RouteRestAreaDetailsInfo[] newArray(int size) {
            return new RouteRestAreaDetailsInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeLong(mRemainDist);
        dest.writeLong(mRemainTime);
        dest.writeString(mServiceName);
        dest.writeString(mServicePOIID);
        dest.writeLong(mSapaDetail);
        dest.writeByte((byte) (mIsAdded ? 1 : 0));
    }
}
