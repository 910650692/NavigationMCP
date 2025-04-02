package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseSearchPoi implements Parcelable {

    private String mPid; //父POI的Id
    private String mName; //名称
    private String mAddress; //地址
    private int mAdCode; //城市代码
    private String mDistance; //距离，已格式化，带单位
    private BaseGeoPoint mPoint; //经纬度
    private BaseCityInfo mCityInfo;

    public BaseSearchPoi() {

    }

    public static final Creator<BaseSearchPoi> CREATOR = new Creator<BaseSearchPoi>() {
        @Override
        public BaseSearchPoi createFromParcel(Parcel source) {
            return new BaseSearchPoi(source);
        }

        @Override
        public BaseSearchPoi[] newArray(int size) {
            return new BaseSearchPoi[size];
        }
    };

    public BaseSearchPoi(Parcel in) {
        mPid = in.readString();
        mName = in.readString();
        mAddress = in.readString();
        mAdCode = in.readInt();
        mDistance = in.readString();
        mPoint = in.readTypedObject(BaseGeoPoint.CREATOR);
        mCityInfo = in.readTypedObject(BaseCityInfo.CREATOR);
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(mPid);
        dest.writeString(mName);
        dest.writeString(mAddress);
        dest.writeInt(mAdCode);
        dest.writeString(mDistance);
        dest.writeTypedObject(mPoint, 0);
        dest.writeTypedObject(mCityInfo, 0);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseSearchPoi{" +
                "pid='" + mPid + '\'' +
                ", name='" + mName + '\'' +
                ", address='" + mAddress + '\'' +
                ", cityCode='" + mAdCode + '\'' +
                ", distance='" + mDistance + '\'' +
                ", pointInfo=" + mPoint +
                '}';
    }

    public String getPid() {
        return mPid;
    }

    public void setPid(String pid) {
        this.mPid = pid;
    }

    public String getName() {
        return mName;
    }

    public void setName(String name) {
        this.mName = name;
    }

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(String address) {
        this.mAddress = address;
    }

    public int getAdCode() {
        return mAdCode;
    }

    public void setAdCode(int adCode) {
        this.mAdCode = adCode;
    }

    public String getDistance() {
        return mDistance;
    }

    public void setDistance(String distance) {
        this.mDistance = distance;
    }

    public BaseGeoPoint getPoint() {
        return mPoint;
    }

    public void setPoint(BaseGeoPoint point) {
        this.mPoint = point;
    }

    public BaseCityInfo getCityInfo() {
        return mCityInfo;
    }

    public void setCityInfo(final BaseCityInfo cityInfo) {
        mCityInfo = cityInfo;
    }
}
