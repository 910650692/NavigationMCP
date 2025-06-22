package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.google.gson.annotations.SerializedName;

import java.util.ArrayList;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class EquipmentInfo implements Parcelable {
    @SerializedName("equipmentName")
    private String mEquipmentName;
    @SerializedName("power")
    private String mPower;
    @SerializedName("equipmentId")
    private String mEquipmentId;
    @SerializedName("connectorInfoItem")
    private ArrayList<ConnectorInfoItem> mConnectorInfoItem;
    protected EquipmentInfo(Parcel in) {
        mEquipmentName = in.readString();
        mEquipmentId = in.readString();
        mPower = in.readString();
        mConnectorInfoItem = in.createTypedArrayList(ConnectorInfoItem.CREATOR);
    }

    public static final Creator<EquipmentInfo> CREATOR = new Creator<EquipmentInfo>() {
        @Override
        public EquipmentInfo createFromParcel(Parcel in) {
            return new EquipmentInfo(in);
        }

        @Override
        public EquipmentInfo[] newArray(int size) {
            return new EquipmentInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(mEquipmentName);
        parcel.writeString(mEquipmentId);
        parcel.writeString(mPower);
        parcel.writeTypedList(mConnectorInfoItem);
    }

    public String getmEquipmentName() {
        return mEquipmentName;
    }

    public void setmEquipmentName(String mEquipmentName) {
        this.mEquipmentName = mEquipmentName;
    }

    public String getmPower() {
        return mPower;
    }

    public void setmPower(String mPower) {
        this.mPower = mPower;
    }

    public String getmEquipmentId() {
        return mEquipmentId;
    }

    public void setmEquipmentId(String mEquipmentId) {
        this.mEquipmentId = mEquipmentId;
    }

    public ArrayList<ConnectorInfoItem> getmConnectorInfoItem() {
        return mConnectorInfoItem;
    }

    public void setmConnectorInfoItem(ArrayList<ConnectorInfoItem> mConnectorInfoItem) {
        this.mConnectorInfoItem = mConnectorInfoItem;
    }
}
