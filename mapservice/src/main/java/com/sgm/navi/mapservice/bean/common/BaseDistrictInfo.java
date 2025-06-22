package com.sgm.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseDistrictInfo implements Parcelable {

    private String mProvince;
    private int mProvinceId;
    private String mCity;
    private int mCityId;
    private String mDistrict;
    private int mDistrictId;

    public BaseDistrictInfo() {

    }

    public static final Creator<BaseDistrictInfo> CREATOR = new Creator<BaseDistrictInfo>() {
        @Override
        public BaseDistrictInfo createFromParcel(final Parcel source) {
            return new BaseDistrictInfo(source);
        }

        @Override
        public BaseDistrictInfo[] newArray(final int size) {
            return new BaseDistrictInfo[size];
        }
    };

    public BaseDistrictInfo(final Parcel in) {
        mProvince = in.readString();
        mProvinceId = in.readInt();
        mCity = in.readString();
        mCityId = in.readInt();
        mDistrict = in.readString();
        mDistrictId = in.readInt();
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeString(mProvince);
        dest.writeInt(mProvinceId);
        dest.writeString(mCity);
        dest.writeInt(mCityId);
        dest.writeString(mDistrict);
        dest.writeInt(mDistrictId);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public String getProvince() {
        return mProvince;
    }

    public void setProvince(final String province) {
        mProvince = province;
    }

    public int getProvinceId() {
        return mProvinceId;
    }

    public void setProvinceId(final int provinceId) {
        mProvinceId = provinceId;
    }

    public String getCity() {
        return mCity;
    }

    public void setCity(final String city) {
        mCity = city;
    }

    public int getCityId() {
        return mCityId;
    }

    public void setCityId(final int cityId) {
        mCityId = cityId;
    }

    public String getDistrict() {
        return mDistrict;
    }

    public void setDistrict(final String district) {
        mDistrict = district;
    }

    public int getDistrictId() {
        return mDistrictId;
    }

    public void setDistrictId(final int districtId) {
        mDistrictId = districtId;
    }

}
