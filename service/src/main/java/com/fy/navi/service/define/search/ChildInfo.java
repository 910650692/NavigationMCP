package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ChildInfo implements Parcelable {
    //唯一标识ID
    public String poiId;
    //名称
    public String name;
    //简称
    public String shortName;
    //子类型
    public int childType;
    //地址
    public String address;
    //标签
    public String label;
    //POI 的位置信息，包含经纬度。
    public GeoPoint location;
    //POI 的入口位置信息，包含经纬度。
    public GeoPoint pointEnter;
    //门点导航信息，用于行车场景下的导航标签细分。
    public int navigation;
    //子点推荐比例，单位为百分比。
    public double ratio;
    //是否被选中
    public int checked;
    //子点充电站信息列表,当POI是景区或者商场且子点是停车场时有该信息
    public ArrayList<ChildChargingInfo> chargingStationList;

    protected ChildInfo(Parcel in) {
        poiId = in.readString();
        name = in.readString();
        shortName = in.readString();
        childType = in.readInt();
        address = in.readString();
        label = in.readString();
        location = in.readParcelable(GeoPoint.class.getClassLoader());
        pointEnter = in.readParcelable(GeoPoint.class.getClassLoader());
        navigation = in.readInt();
        ratio = in.readDouble();
        checked = in.readInt();
        chargingStationList = in.createTypedArrayList(ChildChargingInfo.CREATOR);
    }

    public static final Creator<ChildInfo> CREATOR = new Creator<ChildInfo>() {
        @Override
        public ChildInfo createFromParcel(Parcel in) {
            return new ChildInfo(in);
        }

        @Override
        public ChildInfo[] newArray(int size) {
            return new ChildInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(poiId);
        parcel.writeString(name);
        parcel.writeString(shortName);
        parcel.writeInt(childType);
        parcel.writeString(address);
        parcel.writeString(label);
        parcel.writeParcelable(location, i);
        parcel.writeParcelable(pointEnter, i);
        parcel.writeInt(navigation);
        parcel.writeDouble(ratio);
        parcel.writeInt(checked);
        parcel.writeTypedList(chargingStationList);
    }
}
