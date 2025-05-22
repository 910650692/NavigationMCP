package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import com.android.utils.ConvertUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.R;
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
    private Object mParkingLockFlag;
    @SerializedName("lockStatus")
    private Object mLockStatus;
    @SerializedName("parkNo")
    private String mParkNo;
    @SerializedName("idpUserId")
    private String mIdpUserId;
    @SerializedName("preFlag")
    private Integer mPreFlag;

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
        mIdpUserId = in.readString();
        mPreFlag = in.readInt();
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
        dest.writeInt((int)mParkingLockFlag);
        dest.writeInt((int)mLockStatus);
        dest.writeString(mParkNo);
        dest.writeString(mIdpUserId);
        dest.writeInt(mPreFlag);
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
        if (mParkingLockFlag instanceof Integer) {
            return (Integer) mParkingLockFlag;
        }else if(mParkingLockFlag instanceof Double){
            return ConvertUtils.double2int((Double)mParkingLockFlag);
        }else if (mParkingLockFlag instanceof String) {
            try {
                return Integer.parseInt((String) mParkingLockFlag);
            } catch (NumberFormatException e) {
                return 0;
            }
        }
        return 0;
    }

    public void setmParkingLockFlag(int mParkingLockFlag) {
        this.mParkingLockFlag = mParkingLockFlag;
    }

    public int getmLockStatus() {
        if (mLockStatus instanceof Integer) {
            return (Integer) mLockStatus;
        }else if(mLockStatus instanceof Double){
            return ConvertUtils.double2int((Double)mLockStatus);
        } else if (mLockStatus instanceof String) {
            try {
                return Integer.parseInt((String) mLockStatus);
            } catch (NumberFormatException e) {
                return 0;
            }
        }
        return 0;
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
    public String getStatusName(String mStatus){
        return switch (mStatus) {
            case "0" ->
                    ResourceUtils.Companion.getInstance().getString(R.string.charge_equipment_offline);
            case "1" ->
                    ResourceUtils.Companion.getInstance().getString(R.string.charge_equipment_empty);
            case "2", "3" ->
                    ResourceUtils.Companion.getInstance().getString(R.string.charge_equipment_use);
            case "4" ->
                    ResourceUtils.Companion.getInstance().getString(R.string.charge_equipment_reversion);
            case "255" ->
                    ResourceUtils.Companion.getInstance().getString(R.string.charge_equipment_error);
            default -> "";
        };
    }

    public String getmIdpUserId() {
        return mIdpUserId;
    }

    public void setmIdpUserId(String mIdpUserId) {
        this.mIdpUserId = mIdpUserId;
    }

    public Integer getmPreFlag() {
        return mPreFlag;
    }

    public void setmPreFlag(Integer mPreFlag) {
        this.mPreFlag = mPreFlag;
    }
}
