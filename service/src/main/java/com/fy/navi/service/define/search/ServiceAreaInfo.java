package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.List;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: ServiceAreaInfo
 * @CreateDate: 2025/2/12 15:21
 */

@Data
@NoArgsConstructor
@Accessors(chain = true)
public class ServiceAreaInfo implements Parcelable {
    private int queryType;
    private String poiId;
    private String name;
    private String typeCode;
    private int building = 0;
    private String serviceStar = "";
    private String brand = "";
    private String address = "";
    private List<ServiceAreaChild> serviceAreaChildList;

    protected ServiceAreaInfo(Parcel in) {
        queryType = in.readInt();
        poiId = in.readString();
        name = in.readString();
        typeCode = in.readString();
        building = in.readInt();
        serviceStar = in.readString();
        brand = in.readString();
        address = in.readString();
        serviceAreaChildList = in.createTypedArrayList(ServiceAreaChild.CREATOR);
    }

    public static final Creator<ServiceAreaInfo> CREATOR = new Creator<ServiceAreaInfo>() {
        @Override
        public ServiceAreaInfo createFromParcel(Parcel in) {
            return new ServiceAreaInfo(in);
        }

        @Override
        public ServiceAreaInfo[] newArray(int size) {
            return new ServiceAreaInfo[size];
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
        parcel.writeInt(building);
        parcel.writeString(serviceStar);
        parcel.writeString(brand);
        parcel.writeString(address);
        parcel.writeTypedList(serviceAreaChildList);
    }


    @Data
    @NoArgsConstructor
    @Accessors(chain = true)
    public static class ServiceAreaChild implements Parcelable {
        private boolean discount;
        private String gasType;
        private String id;
        private String name;
        private String minMame;
        private String parentId;
        private String typeCode;
        private String business;
        private String tag;
        private GeoPoint poi_loc;

        protected ServiceAreaChild(Parcel in) {
            discount = in.readByte() != 0;
            gasType = in.readString();
            id = in.readString();
            name = in.readString();
            minMame = in.readString();
            parentId = in.readString();
            typeCode = in.readString();
            business = in.readString();
            tag = in.readString();
        }

        public static final Creator<ServiceAreaChild> CREATOR = new Creator<ServiceAreaChild>() {
            @Override
            public ServiceAreaChild createFromParcel(Parcel in) {
                return new ServiceAreaChild(in);
            }

            @Override
            public ServiceAreaChild[] newArray(int size) {
                return new ServiceAreaChild[size];
            }
        };

        @Override
        public int describeContents() {
            return 0;
        }

        @Override
        public void writeToParcel(@NonNull Parcel parcel, int i) {
            parcel.writeByte((byte) (discount ? 1 : 0));
            parcel.writeString(gasType);
            parcel.writeString(id);
            parcel.writeString(name);
            parcel.writeString(minMame);
            parcel.writeString(parentId);
            parcel.writeString(typeCode);
            parcel.writeString(business);
            parcel.writeString(tag);
        }
    }
}
