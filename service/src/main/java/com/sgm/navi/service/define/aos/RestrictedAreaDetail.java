package com.sgm.navi.service.define.aos;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;


import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

@Getter
@Setter
@NoArgsConstructor
public class RestrictedAreaDetail implements Parcelable {
    private long mRequestId;
    private int mCityCode;           //1100
    private String mCityName;        //北京市
    private String mTitle;           //北京市限行政策
    private String mPolicyname;       //二环及以内外地机动车限行
    private int mRuleid;              //1766888
    private int mRing;                //0
    private int mEffect;              //1
    private int mLocal;               //2
    private int mVehicle;             //1
    private String mTime;             //2021年11月1日起，工作日和周末全天限行（节假日限制）
    private String mSummary;          //二环路及以内道路（不含外侧辅路）
    private String mDesc;             //外地机动车禁行
    private String mOtherdesc;        //“”

    protected RestrictedAreaDetail(Parcel in) {
        mRequestId = in.readLong();
        mCityCode = in.readInt();
        mCityName = in.readString();
        mTitle = in.readString();
        mPolicyname = in.readString();
        mRuleid = in.readInt();
        mRing = in.readInt();
        mEffect = in.readInt();
        mLocal = in.readInt();
        mVehicle = in.readInt();
        mTime = in.readString();
        mSummary = in.readString();
        mDesc = in.readString();
        mOtherdesc = in.readString();
    }

    public static final Creator<RestrictedAreaDetail> CREATOR = new Creator<RestrictedAreaDetail>() {
        @Override
        public RestrictedAreaDetail createFromParcel(Parcel in) {
            return new RestrictedAreaDetail(in);
        }

        @Override
        public RestrictedAreaDetail[] newArray(int size) {
            return new RestrictedAreaDetail[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel dest, int flags) {
        dest.writeLong(mRequestId);
        dest.writeInt(mCityCode);
        dest.writeString(mCityName);
        dest.writeString(mTitle);
        dest.writeString(mPolicyname);
        dest.writeInt(mRuleid);
        dest.writeInt(mRing);
        dest.writeInt(mEffect);
        dest.writeInt(mLocal);
        dest.writeInt(mVehicle);
        dest.writeString(mTime);
        dest.writeString(mSummary);
        dest.writeString(mDesc);
        dest.writeString(mOtherdesc);
    }
}
