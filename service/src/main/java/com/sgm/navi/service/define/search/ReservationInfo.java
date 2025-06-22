package com.sgm.navi.service.define.search;

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
public class ReservationInfo implements Parcelable {
    @SerializedName("preNum")
    private String mPreNum;
    @SerializedName("status")
    private Integer mStatus;
    @SerializedName("operatorId")
    private String mOperatorId;
    @SerializedName("stationId")
    private String mStationId;
    @SerializedName("equipmentId")
    private String mEquipmentId;
    @SerializedName("userId")
    private String mUserId;
    @SerializedName("createTime")
    private String mCreateTime;
    @SerializedName("lng")
    private String mLng;
    @SerializedName("lat")
    private String mLat;

    public String getmPreNum() {
        return mPreNum;
    }

    public ReservationInfo setmPreNum(String mPreNum) {
        this.mPreNum = mPreNum;
        return this;
    }

    public Integer getmStatus() {
        return mStatus;
    }

    public ReservationInfo setmStatus(Integer mStatus) {
        this.mStatus = mStatus;
        return this;
    }

    public String getmOperatorId() {
        return mOperatorId;
    }

    public ReservationInfo setmOperatorId(String mOperatorId) {
        this.mOperatorId = mOperatorId;
        return this;
    }

    public String getmStationId() {
        return mStationId;
    }

    public ReservationInfo setmStationId(String mStationId) {
        this.mStationId = mStationId;
        return this;
    }

    public String getmEquipmentId() {
        return mEquipmentId;
    }

    public ReservationInfo setmEquipmentId(String mEquipmentId) {
        this.mEquipmentId = mEquipmentId;
        return this;
    }

    public String getmUserId() {
        return mUserId;
    }

    public ReservationInfo setmUserId(String mUserId) {
        this.mUserId = mUserId;
        return this;
    }

    public String getmCreateTime() {
        return mCreateTime;
    }

    public ReservationInfo setmCreateTime(String mCreateTime) {
        this.mCreateTime = mCreateTime;
        return this;
    }

    public String getmLng(){
        return mLng;
    }

    public ReservationInfo setmLng(String mLng){
        this.mLng = mLng;
        return this;
    }

    public String getmLat(){
        return mLat;
    }

    public ReservationInfo setmLat(String mLat){
        this.mLat = mLat;
        return this;
    }

    protected ReservationInfo(Parcel in) {
        mPreNum = in.readString();
        mStatus = in.readInt();
        mOperatorId = in.readString();
        mStationId = in.readString();
        mEquipmentId = in.readString();
        mUserId = in.readString();
        mCreateTime = in.readString();
        mLat = in.readString();
        mLng = in.readString();
    }

    public static final Creator<ReservationInfo> CREATOR = new Creator<ReservationInfo>() {
        @Override
        public ReservationInfo createFromParcel(Parcel in) {
            return new ReservationInfo(in);
        }

        @Override
        public ReservationInfo[] newArray(int size) {
            return new ReservationInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(mPreNum);
        parcel.writeInt(mStatus);
        parcel.writeString(mOperatorId);
        parcel.writeString(mStationId);
        parcel.writeString(mEquipmentId);
        parcel.writeString(mUserId);
        parcel.writeString(mCreateTime);
        parcel.writeString(mLat);
        parcel.writeString(mLng);
    }
}
