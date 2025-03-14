package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: GasStationInfo: 加油站信息
 * @CreateDate: 2025/2/12 11:31
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class GasStationInfo implements Parcelable {
    public int queryType;
    public String poiId;
    public String name;
    public String typeCode;
    private String type;
    private String price;
    private boolean discount = false;

    protected GasStationInfo(Parcel in) {
        queryType = in.readInt();
        poiId = in.readString();
        name = in.readString();
        typeCode = in.readString();
        type = in.readString();
        price = in.readString();
        discount = in.readByte() != 0;
    }

    public static final Creator<GasStationInfo> CREATOR = new Creator<GasStationInfo>() {
        @Override
        public GasStationInfo createFromParcel(Parcel in) {
            return new GasStationInfo(in);
        }

        @Override
        public GasStationInfo[] newArray(int size) {
            return new GasStationInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(queryType);
        parcel.writeString(poiId);
        parcel.writeString(name);
        parcel.writeString(typeCode);
        parcel.writeString(type);
        parcel.writeString(price);
        parcel.writeByte((byte) (discount ? 1 : 0));
    }
}
