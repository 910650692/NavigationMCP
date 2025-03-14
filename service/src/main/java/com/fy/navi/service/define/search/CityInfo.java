package com.fy.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @Author: baipeng0904
 * @Description: CityInfo
 * @CreateDate: 2025/2/13 13:13
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class CityInfo implements Parcelable {
    private String cityName;       // 城市名称
    private int cityCode;
    private String province;
    private int provinceAdCode;// 城市区号
    private String areaCode;
    private String desc;
    private String address;
    private int adCode;
    private String district;       // 区县名称
    private int districtAdCode;    // 区县行政区编号// 城市代码// 地址     // 地址
    private GeoPoint cityPoint;
    private String pos;

    protected CityInfo(Parcel in) {
        cityName = in.readString();
        cityCode = in.readInt();
        province = in.readString();
        provinceAdCode = in.readInt();
        areaCode = in.readString();
        desc = in.readString();
        address = in.readString();
        adCode = in.readInt();
        district = in.readString();
        districtAdCode = in.readInt();
        cityPoint = in.readParcelable(GeoPoint.class.getClassLoader());
        pos = in.readString();
    }

    public static final Creator<CityInfo> CREATOR = new Creator<CityInfo>() {
        @Override
        public CityInfo createFromParcel(Parcel in) {
            return new CityInfo(in);
        }

        @Override
        public CityInfo[] newArray(int size) {
            return new CityInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull Parcel parcel, int i) {
        parcel.writeString(cityName);
        parcel.writeInt(cityCode);
        parcel.writeString(province);
        parcel.writeInt(provinceAdCode);
        parcel.writeString(areaCode);
        parcel.writeString(desc);
        parcel.writeString(address);
        parcel.writeInt(adCode);
        parcel.writeString(district);
        parcel.writeInt(districtAdCode);
        parcel.writeParcelable(cityPoint, i);
        parcel.writeString(pos);
    }
}
