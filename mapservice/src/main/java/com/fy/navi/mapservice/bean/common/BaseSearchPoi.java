package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseSearchPoi implements Parcelable {

    private String pid; //父POI的Id
    private String name; //名称
    private String address; //地址
    private int adCode; //城市代码
    private String distance; //距离，已格式化，带单位
    private BaseGeoPoint point; //经纬度

    public BaseSearchPoi() {}

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
        pid = in.readString();
        name = in.readString();
        address = in.readString();
        adCode = in.readInt();
        distance = in.readString();
        point = in.readTypedObject(BaseGeoPoint.CREATOR);
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(pid);
        dest.writeString(name);
        dest.writeString(address);
        dest.writeInt(adCode);
        dest.writeString(distance);
        dest.writeTypedObject(point, 0);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public String toString() {
        return "BaseSearchPoi{" +
                "pid='" + pid + '\'' +
                ", name='" + name + '\'' +
                ", address='" + address + '\'' +
                ", cityCode='" + adCode + '\'' +
                ", distance='" + distance + '\'' +
                ", pointInfo=" + point +
                '}';
    }

    public String getPid() {
        return pid;
    }

    public void setPid(String pid) {
        this.pid = pid;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getAddress() {
        return address;
    }

    public void setAddress(String address) {
        this.address = address;
    }

    public int getAdCode() {
        return adCode;
    }

    public void setAdCode(int adCode) {
        this.adCode = adCode;
    }

    public String getDistance() {
        return distance;
    }

    public void setDistance(String distance) {
        this.distance = distance;
    }

    public BaseGeoPoint getPoint() {
        return point;
    }

    public void setPoint(BaseGeoPoint point) {
        this.point = point;
    }
}
