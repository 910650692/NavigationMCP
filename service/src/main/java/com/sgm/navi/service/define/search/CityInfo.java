package com.sgm.navi.service.define.search;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.NonNull;

import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

/**
 * @author baipeng0904
 * @version \$Revision1.0\$
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
public class CityInfo implements Parcelable {
    private String mCityName;       // 城市名称
    private int mCityCode;
    private String mProvince;
    private int mProvinceAdCode;// 城市区号
    private String mAreaCode;
    private String mDesc;
    private String mAddress;
    private int mAdCode;
    private String mDistrict;       // 区县名称
    private int mDistrictAdCode;    // 区县行政区编号// 城市代码// 地址     // 地址
    private GeoPoint mCityPoint;
    private String mPos;

    public String getCityName() {
        return mCityName;
    }

    /**
     * 设置城市名称
     * @param cityName 城市名称
     * @return CityInfo
     */
    public CityInfo setCityName(final String cityName) {
        this.mCityName = cityName;
        return this;
    }

    public int getCityCode() {
        return mCityCode;
    }

    /**
     * 设置城市代码
     * @param cityCode 城市代码
     * @return CityInfo
     */
    public CityInfo setCityCode(final int cityCode) {
        this.mCityCode = cityCode;
        return this;
    }

    public String getProvince() {
        return mProvince;
    }

    /**
     * 设置省份
     * @param province 省份
     * @return CityInfo
     */
    public CityInfo setProvince(final String province) {
        this.mProvince = province;
        return this;
    }

    public String getAreaCode() {
        return mAreaCode;
    }

    /**
     * 设置区域代码
     * @param areaCode 区域代码
     * @return CityInfo
     */
    public CityInfo setAreaCode(final String areaCode) {
        this.mAreaCode = areaCode;
        return this;
    }

    public int getProvinceAdCode() {
        return mProvinceAdCode;
    }

    /**
     * 设置省份区号
     * @param provinceAdCode 省份区号
     * @return CityInfo
     */
    public CityInfo setProvinceAdCode(final int provinceAdCode) {
        this.mProvinceAdCode = provinceAdCode;
        return this;
    }

    public String getDesc() {
        return mDesc;
    }

    /**
     * 设置描述
     * @param desc 描述
     * @return CityInfo
     */
    public CityInfo setDesc(final String desc) {
        this.mDesc = desc;
        return this;
    }

    public String getAddress() {
        return mAddress;
    }

    /**
     * 设置地址
     * @param address 地址
     * @return CityInfo
     */
    public CityInfo setAddress(final String address) {
        this.mAddress = address;
        return this;
    }

    public int getAdCode() {
        return mAdCode;
    }

    /**
     * 设置城市代码
     * @param adCode 城市代码
     * @return CityInfo
     */
    public CityInfo setAdCode(final int adCode) {
        this.mAdCode = adCode;
        return this;
    }

    public int getDistrictAdCode() {
        return mDistrictAdCode;
    }

    /**
     * 设置区县行政区编号
     * @param districtAdCode 区县行政区编号
     * @return CityInfo
     */
    public CityInfo setDistrictAdCode(final int districtAdCode) {
        this.mDistrictAdCode = districtAdCode;
        return this;
    }

    public String getDistrict() {
        return mDistrict;
    }

    /**
     * 设置区县
     * @param district 区县
     * @return CityInfo
     */
    public CityInfo setDistrict(final String district) {
        this.mDistrict = district;
        return this;
    }

    public GeoPoint getCityPoint() {
        return mCityPoint;
    }

    /**
     * 设置城市坐标
     * @param cityPoint 城市坐标
     * @return CityInfo
     */
    public CityInfo setCityPoint(final GeoPoint cityPoint) {
        this.mCityPoint = cityPoint;
        return this;
    }

    public String getPos() {
        return mPos;
    }

    /**
     * 设置位置
     * @param pos 位置
     * @return CityInfo
     */
    public CityInfo setPos(final String pos) {
        this.mPos = pos;
        return this;
    }

    protected CityInfo(final Parcel in) {
        mCityName = in.readString();
        mCityCode = in.readInt();
        mProvince = in.readString();
        mProvinceAdCode = in.readInt();
        mAreaCode = in.readString();
        mDesc = in.readString();
        mAddress = in.readString();
        mAdCode = in.readInt();
        mDistrict = in.readString();
        mDistrictAdCode = in.readInt();
        mCityPoint = in.readParcelable(GeoPoint.class.getClassLoader());
        mPos = in.readString();
    }

    public static final Creator<CityInfo> CREATOR = new Creator<CityInfo>() {
        @Override
        public CityInfo createFromParcel(final Parcel in) {
            return new CityInfo(in);
        }

        @Override
        public CityInfo[] newArray(final int size) {
            return new CityInfo[size];
        }
    };

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(@NonNull final Parcel parcel, final int i) {
        parcel.writeString(mCityName);
        parcel.writeInt(mCityCode);
        parcel.writeString(mProvince);
        parcel.writeInt(mProvinceAdCode);
        parcel.writeString(mAreaCode);
        parcel.writeString(mDesc);
        parcel.writeString(mAddress);
        parcel.writeInt(mAdCode);
        parcel.writeString(mDistrict);
        parcel.writeInt(mDistrictAdCode);
        parcel.writeParcelable(mCityPoint, i);
        parcel.writeString(mPos);
    }
}
