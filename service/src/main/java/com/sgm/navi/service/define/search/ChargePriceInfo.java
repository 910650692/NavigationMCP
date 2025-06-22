package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChargePriceInfo implements Parcelable {
    // 时间段
    private String mTime;
    // 电费单价
    private Float mElectricityFee;
    // 服务费单价
    private Float mServiceFee;

    public ChargePriceInfo setmTime(String mTime) {
        this.mTime = mTime;
        return this;
    }

    public String getmTime() {
        return mTime;
    }

    public ChargePriceInfo setmElectricityFee(Float mElectricityFee) {
        this.mElectricityFee = mElectricityFee;
        return this;
    }

    public Float getmElectricityFee() {
        return mElectricityFee;
    }

    public ChargePriceInfo setmServiceFee(Float mServiceFee) {
        this.mServiceFee = mServiceFee;
        return this;
    }

    public Float getmServiceFee() {
        return mServiceFee;
    }

    public static final Creator<ChargePriceInfo> CREATOR = new Creator<ChargePriceInfo>() {
        @Override
        public ChargePriceInfo createFromParcel(Parcel in) {
            return new ChargePriceInfo(in);
        }

        @Override
        public ChargePriceInfo[] newArray(int size) {
            return new ChargePriceInfo[size];
        }
    };

    public ChargePriceInfo(Parcel in) {
        mTime = in.readString();
        mElectricityFee = in.readFloat();
        mServiceFee = in.readFloat();
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(mTime);
        parcel.writeFloat(mElectricityFee);
        parcel.writeFloat(mServiceFee);
    }
}
