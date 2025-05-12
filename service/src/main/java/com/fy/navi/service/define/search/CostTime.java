package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class CostTime implements Parcelable {
    @SerializedName("serviceFee")
    private String mServiceFee;
    @SerializedName("time")
    private String mTime;
    @SerializedName("electricityFee")
    private String mElectricityFee;

    protected CostTime(Parcel in) {
        mServiceFee = in.readString();
        mTime = in.readString();
        mElectricityFee = in.readString();
    }

    public static final Creator<CostTime> CREATOR = new Creator<CostTime>() {
        @Override
        public CostTime createFromParcel(Parcel in) {
            return new CostTime(in);
        }

        @Override
        public CostTime[] newArray(int size) {
            return new CostTime[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(mServiceFee);
        parcel.writeString(mTime);
        parcel.writeString(mElectricityFee);
    }

    public String getmServiceFee() {
        return mServiceFee;
    }

    public CostTime setServiceFee(String mServiceFee) {
        this.mServiceFee = mServiceFee;
        return this;
    }

    public String getTime() {
        return mTime;
    }

    public CostTime setTime(String mTime) {
        this.mTime = mTime;
        return this;
    }

    public String getmElectricityFee() {
        return mElectricityFee;
    }

    public CostTime setElectricityFee(String mElectricityFee) {
        this.mElectricityFee = mElectricityFee;
        return this;
    }
}
