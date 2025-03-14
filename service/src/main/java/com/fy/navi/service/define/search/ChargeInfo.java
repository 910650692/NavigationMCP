package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: ChargeInfo
 * @CreateDate: 2025/2/12 15:12
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChargeInfo implements Parcelable {
    private int childType = 0;
    private int slow_free = 0;
    private int slow_total = 0;
    private int fast_free = 0;
    private int fast_total = 0;
    private int superFree = -1;
    private int superTotal = -1;
    private String srcType = "";
    private String market = "";
    private String parkCategory = "";
    private String openTime = "";
    private boolean open24h = false;
    private float fMax;
    private float fMin;
    private String currentElePrice;
    private String currentServicePrice;
    private int maxPower;
    private int minPower;
    private int queryType;
    private String poiId;
    private String name;
    private String typeCode;
    private int slowVolt;
    private int slowPower;
    private int fastVolt;
    private int fastPower;

    protected ChargeInfo(Parcel in) {
        childType = in.readInt();
        slow_free = in.readInt();
        slow_total = in.readInt();
        fast_free = in.readInt();
        fast_total = in.readInt();
        superFree = in.readInt();
        superTotal = in.readInt();
        srcType = in.readString();
        market = in.readString();
        parkCategory = in.readString();
        openTime = in.readString();
        open24h = in.readByte() != 0;
        fMax = in.readFloat();
        fMin = in.readFloat();
        currentElePrice = in.readString();
        currentServicePrice = in.readString();
        maxPower = in.readInt();
        minPower = in.readInt();
        queryType = in.readInt();
        poiId = in.readString();
        name = in.readString();
        typeCode = in.readString();
        slowVolt = in.readInt();
        slowPower = in.readInt();
        fastVolt = in.readInt();
        fastPower = in.readInt();
    }

    public static final Creator<ChargeInfo> CREATOR = new Creator<ChargeInfo>() {
        @Override
        public ChargeInfo createFromParcel(Parcel in) {
            return new ChargeInfo(in);
        }

        @Override
        public ChargeInfo[] newArray(int size) {
            return new ChargeInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(childType);
        parcel.writeInt(slow_free);
        parcel.writeInt(slow_total);
        parcel.writeInt(fast_free);
        parcel.writeInt(fast_total);
        parcel.writeInt(superFree);
        parcel.writeInt(superTotal);
        parcel.writeString(srcType);
        parcel.writeString(market);
        parcel.writeString(parkCategory);
        parcel.writeString(openTime);
        parcel.writeByte((byte) (open24h ? 1 : 0));
        parcel.writeFloat(fMax);
        parcel.writeFloat(fMin);
        parcel.writeString(currentElePrice);
        parcel.writeString(currentServicePrice);
        parcel.writeInt(maxPower);
        parcel.writeInt(minPower);
        parcel.writeInt(queryType);
        parcel.writeString(poiId);
        parcel.writeString(name);
        parcel.writeString(typeCode);
        parcel.writeInt(slowVolt);
        parcel.writeInt(slowPower);
        parcel.writeInt(fastVolt);
        parcel.writeInt(fastPower);
    }
}
