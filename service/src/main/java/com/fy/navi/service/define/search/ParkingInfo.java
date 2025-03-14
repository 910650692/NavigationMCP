package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: ParkingInfo:停车场信息
 * @CreateDate: 2025/2/12 13:26
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ParkingInfo implements Parcelable {
    private int spaceTotal;
    private int spaceFree;
    private int busyStatus;
    private String srcType;
    private int space;
    private String fee;
    private String geometry;
    private String charge;
    private String dayCharge;
    private String nightCharge;
    private String category;
    private String parkingSrcType;
    private int queryType;
    private String poiId;
    private String name;
    private String typeCode;
    private String address = "";
    private List<SearchParkInOutInfo> searchParkInOutInfos;

    protected ParkingInfo(Parcel in) {
        spaceTotal = in.readInt();
        spaceFree = in.readInt();
        busyStatus = in.readInt();
        srcType = in.readString();
        space = in.readInt();
        fee = in.readString();
        geometry = in.readString();
        charge = in.readString();
        dayCharge = in.readString();
        nightCharge = in.readString();
        category = in.readString();
        parkingSrcType = in.readString();
        queryType = in.readInt();
        poiId = in.readString();
        name = in.readString();
        typeCode = in.readString();
        address = in.readString();
        searchParkInOutInfos = in.createTypedArrayList(SearchParkInOutInfo.CREATOR);
    }

    public static final Creator<ParkingInfo> CREATOR = new Creator<ParkingInfo>() {
        @Override
        public ParkingInfo createFromParcel(Parcel in) {
            return new ParkingInfo(in);
        }

        @Override
        public ParkingInfo[] newArray(int size) {
            return new ParkingInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeInt(spaceTotal);
        parcel.writeInt(spaceFree);
        parcel.writeInt(busyStatus);
        parcel.writeString(srcType);
        parcel.writeInt(space);
        parcel.writeString(fee);
        parcel.writeString(geometry);
        parcel.writeString(charge);
        parcel.writeString(dayCharge);
        parcel.writeString(nightCharge);
        parcel.writeString(category);
        parcel.writeString(parkingSrcType);
        parcel.writeInt(queryType);
        parcel.writeString(poiId);
        parcel.writeString(name);
        parcel.writeString(typeCode);
        parcel.writeString(address);
        parcel.writeTypedList(searchParkInOutInfos);
    }
}
