package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChildChargingInfo implements Parcelable {
    //子点的父子关系类型
    public int childType;
    //价格
    public double price;
    //简称
    public String shortName;
    //名称
    public String name;
    //唯一标识
    public String poiId;
    //父节点的唯一标识
    public String parentPoiId;
    //类型编码
    public String typeCode;
    //慢充电桩数量
    public int numSlow;
    //快充电桩数量
    public int numFast;
    //坐标
    public GeoPoint location;

    protected ChildChargingInfo(Parcel in) {
        childType = in.readInt();
        price = in.readDouble();
        shortName = in.readString();
        name = in.readString();
        poiId = in.readString();
        parentPoiId = in.readString();
        typeCode = in.readString();
        numSlow = in.readInt();
        numFast = in.readInt();
        location = in.readParcelable(GeoPoint.class.getClassLoader());
    }

    public static final Creator<ChildChargingInfo> CREATOR = new Creator<ChildChargingInfo>() {
        @Override
        public ChildChargingInfo createFromParcel(Parcel in) {
            return new ChildChargingInfo(in);
        }

        @Override
        public ChildChargingInfo[] newArray(int size) {
            return new ChildChargingInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(childType);
        parcel.writeDouble(price);
        parcel.writeString(shortName);
        parcel.writeString(name);
        parcel.writeString(poiId);
        parcel.writeString(parentPoiId);
        parcel.writeString(typeCode);
        parcel.writeInt(numSlow);
        parcel.writeInt(numFast);
        parcel.writeParcelable(location, i);
    }
}
