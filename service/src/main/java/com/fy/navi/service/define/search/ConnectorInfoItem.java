package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.SerializedName;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ConnectorInfoItem implements Parcelable {
    @SerializedName("connectorId")
    private String mConnectorId;
    @SerializedName("connectorName")
    private String mConnectorName;
    @SerializedName("chargeType")
    private String mChargeType;
    @SerializedName("voltageLowerLimits")
    private String mVoltageLowerLimits;
    @SerializedName("voltageUpperLimits")
    private String mVoltageUpperLimits;
    @SerializedName("ratedCurrent")
    private String mRatedCurrent;
    @SerializedName("power")
    private String mPower;
    @SerializedName("nationalStandard")
    private String mNationalStandard;
    @SerializedName("status")
    private String mStatus;
    @SerializedName("parkingLockFlag")
    private int mParkingLockFlag;
    @SerializedName("lockStatus")
    private int mLockStatus;
    @SerializedName("parkNo")
    private String mParkNo;

    protected ConnectorInfoItem(Parcel in) {
        mConnectorId = in.readString();
        mConnectorName = in.readString();
        mChargeType = in.readString();
        mVoltageLowerLimits = in.readString();
        mVoltageUpperLimits = in.readString();
        mRatedCurrent = in.readString();
        mPower = in.readString();
        mNationalStandard = in.readString();
        mStatus = in.readString();
        mParkingLockFlag = in.readInt();
        mLockStatus = in.readInt();
        mParkNo = in.readString();
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(mConnectorId);
        dest.writeString(mConnectorName);
        dest.writeString(mChargeType);
        dest.writeString(mVoltageLowerLimits);
        dest.writeString(mVoltageUpperLimits);
        dest.writeString(mRatedCurrent);
        dest.writeString(mPower);
        dest.writeString(mNationalStandard);
        dest.writeString(mStatus);
        dest.writeInt(mParkingLockFlag);
        dest.writeInt(mLockStatus);
        dest.writeString(mParkNo);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public static final Creator<ConnectorInfoItem> CREATOR = new Creator<ConnectorInfoItem>() {
        @Override
        public ConnectorInfoItem createFromParcel(Parcel in) {
            return new ConnectorInfoItem(in);
        }

        @Override
        public ConnectorInfoItem[] newArray(int size) {
            return new ConnectorInfoItem[size];
        }
    };

    public String getmConnectorId() {
        return mConnectorId;
    }

    public void setmConnectorId(String mConnectorId) {
        this.mConnectorId = mConnectorId;
    }

    public String getmConnectorName() {
        return mConnectorName;
    }

    public void setmConnectorName(String mConnectorName) {
        this.mConnectorName = mConnectorName;
    }

    public String getmChargeType() {
        return mChargeType;
    }

    public void setmChargeType(String mChargeType) {
        this.mChargeType = mChargeType;
    }

    public String getmVoltageLowerLimits() {
        return mVoltageLowerLimits;
    }

    public void setmVoltageLowerLimits(String mVoltageLowerLimits) {
        this.mVoltageLowerLimits = mVoltageLowerLimits;
    }

    public String getmVoltageUpperLimits() {
        return mVoltageUpperLimits;
    }

    public void setmVoltageUpperLimits(String mVoltageUpperLimits) {
        this.mVoltageUpperLimits = mVoltageUpperLimits;
    }

    public String getmRatedCurrent() {
        return mRatedCurrent;
    }

    public void setmRatedCurrent(String mRatedCurrent) {
        this.mRatedCurrent = mRatedCurrent;
    }

    public String getmPower() {
        return mPower;
    }

    public void setmPower(String mPower) {
        this.mPower = mPower;
    }

    public String getmNationalStandard() {
        return mNationalStandard;
    }

    public void setmNationalStandard(String mNationalStandard) {
        this.mNationalStandard = mNationalStandard;
    }

    public String getmStatus() {
        return mStatus;
    }

    public void setmStatus(String mStatus) {
        this.mStatus = mStatus;
    }

    public int getmParkingLockFlag() {
        return mParkingLockFlag;
    }

    public void setmParkingLockFlag(int mParkingLockFlag) {
        this.mParkingLockFlag = mParkingLockFlag;
    }

    public int getmLockStatus() {
        return mLockStatus;
    }

    public void setmLockStatus(int mLockStatus) {
        this.mLockStatus = mLockStatus;
    }

    public String getmParkNo() {
        return mParkNo;
    }

    public void setmParkNo(String mParkNo) {
        this.mParkNo = mParkNo;
    }
}
