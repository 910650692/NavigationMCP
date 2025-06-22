package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.android.utils.ConvertUtils;
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
    private boolean mIsCurrentTime;

    protected CostTime(Parcel in) {
        mServiceFee = in.readString();
        mTime = in.readString();
        mElectricityFee = in.readString();
        mIsCurrentTime = in.readBoolean();
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
        parcel.writeBoolean(mIsCurrentTime);
    }

    public String getmServiceFee() {
        if(!ConvertUtils.isNull(mServiceFee)){
            return ConvertUtils.stringFormatTwo(mServiceFee);
        }
        return "";
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
        if(!ConvertUtils.isNull(mElectricityFee)){
            return ConvertUtils.stringFormatTwo(mElectricityFee);
        }
        return "";
    }

    public CostTime setElectricityFee(String mElectricityFee) {
        this.mElectricityFee = mElectricityFee;
        return this;
    }

    public Boolean getmIsCurrentTime(){
        return mIsCurrentTime;
    }

    public CostTime setmIsCurrentTime(Boolean mIsCurrentTime){
        this.mIsCurrentTime = mIsCurrentTime;
        return this;
    }

    public String getPrice(){
        double price = ConvertUtils.str2Double(getmServiceFee()) + ConvertUtils.str2Double(getmElectricityFee());
        return  ConvertUtils.stringFormatTwo(String.valueOf(price));
    }
}
