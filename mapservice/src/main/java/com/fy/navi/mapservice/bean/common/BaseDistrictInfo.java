package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseDistrictInfo implements Parcelable {

    private String province;
    private int provinceId;
    private String city;
    private int cityId;
    private String district;
    private int districtId;

    public BaseDistrictInfo() {}

    public static final Creator<BaseDistrictInfo> CREATOR = new Creator<BaseDistrictInfo>() {
        @Override
        public BaseDistrictInfo createFromParcel(Parcel source) {
            return new BaseDistrictInfo(source);
        }

        @Override
        public BaseDistrictInfo[] newArray(int size) {
            return new BaseDistrictInfo[size];
        }
    };

    public BaseDistrictInfo(Parcel in) {
        province = in.readString();
        provinceId = in.readInt();
        city = in.readString();
        cityId = in.readInt();
        district = in.readString();
        districtId = in.readInt();
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(province);
        dest.writeInt(provinceId);
        dest.writeString(city);
        dest.writeInt(cityId);
        dest.writeString(district);
        dest.writeInt(districtId);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public String getProvince() {
        return province;
    }

    public void setProvince(String province) {
        this.province = province;
    }

    public int getProvinceId() {
        return provinceId;
    }

    public void setProvinceId(int provinceId) {
        this.provinceId = provinceId;
    }

    public String getCity() {
        return city;
    }

    public void setCity(String city) {
        this.city = city;
    }

    public int getCityId() {
        return cityId;
    }

    public void setCityId(int cityId) {
        this.cityId = cityId;
    }

    public String getDistrict() {
        return district;
    }

    public void setDistrict(String district) {
        this.district = district;
    }

    public int getDistrictId() {
        return districtId;
    }

    public void setDistrictId(int districtId) {
        this.districtId = districtId;
    }

}
