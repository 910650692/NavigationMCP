package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseCityInfo implements Parcelable {
    private String mCityName;       // 城市名称
    private int mCityCode;
    private String mProvince;
    private int mProvinceAdCode;// 城市区号
    private String mDistrict;       // 区县名称
    private int mDistrictAdCode;    // 区县行政区编号// 城市代码// 地址     // 地址

    public BaseCityInfo() {

    }

    public static final Parcelable.Creator<BaseCityInfo> CREATOR = new Parcelable.Creator<BaseCityInfo>() {
        @Override
        public BaseCityInfo createFromParcel(final Parcel source) {
            return new BaseCityInfo(source);
        }

        @Override
        public BaseCityInfo[] newArray(final int size) {
            return new BaseCityInfo[size];
        }
    };

    public BaseCityInfo(final Parcel in) {
        mCityName = in.readString();
        mCityCode = in.readInt();
        mProvince = in.readString();
        mProvinceAdCode = in.readInt();
        mDistrict = in.readString();
        mDistrictAdCode = in.readInt();
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeString(mCityName);
        dest.writeInt(mCityCode);
        dest.writeString(mProvince);
        dest.writeInt(mProvinceAdCode);
        dest.writeString(mDistrict);
        dest.writeInt(mDistrictAdCode);
    }


    public String getCityName() {
        return mCityName;
    }

    public void setCityName(final String cityName) {
        this.mCityName = cityName;
    }

    public int getCityCode() {
        return mCityCode;
    }

    public void setCityCode(final int cityCode) {
        this.mCityCode = cityCode;
    }

    public String getProvince() {
        return mProvince;
    }

    public void setProvince(final String province) {
        this.mProvince = province;
    }

    public int getProvinceAdCode() {
        return mProvinceAdCode;
    }

    public void setProvinceAdCode(final int provinceAdCode) {
        this.mProvinceAdCode = provinceAdCode;
    }

    public String getDistrict() {
        return mDistrict;
    }

    public void setDistrict(final String district) {
        this.mDistrict = district;
    }

    public int getDistrictAdCode() {
        return mDistrictAdCode;
    }

    public void setDistrictAdCode(final int districtAdCode) {
        this.mDistrictAdCode = districtAdCode;
    }

    @Override
    public String toString() {
        return "BaseCityInfo{" +
                "mCityName='" + mCityName + '\'' +
                ", mCityCode=" + mCityCode +
                ", mProvince='" + mProvince + '\'' +
                ", mProvinceAdCode=" + mProvinceAdCode +
                ", mDistrict='" + mDistrict + '\'' +
                ", mDistrictAdCode=" + mDistrictAdCode +
                '}';
    }

}
