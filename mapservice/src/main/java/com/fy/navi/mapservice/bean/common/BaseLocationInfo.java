package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseLocationInfo implements Parcelable {

    private double latitude;
    private double longitude;
    private double altitude;
    private String name;
    private String address;
    //原生定位provider
    private String provider;
    // 航向角度
    private float bearing;
    private float speed; //单位km/h
    // 定位精度
    private float accuracy;
    // 垂直方向精度
    private float vaccuracy;
    // gps时间戳
    private long gpsTickCount;
    // 系统时间戳
    private long sysTickCount;
    // 定位类型: -1无效定位，0卫星定位  1wifi定位 2基站定位 3 蓝牙定位 4地磁定位 5未知定位类型
    private int type;
    // 定位来源: 0:无效 1:GPS 2:北斗 3:GLONASS 4:GALILEO 8:DR 32.WIFI-corrected 64: Base-Station-corrected
    private int sourceFlag;

    public static final Creator<BaseLocationInfo> CREATOR = new Creator<BaseLocationInfo>() {

        @Override
        public BaseLocationInfo createFromParcel(Parcel source) {
            return new BaseLocationInfo(source);
        }

        @Override
        public BaseLocationInfo[] newArray(int size) {
            return new BaseLocationInfo[size];
        }
    };

    public BaseLocationInfo() {}

    public BaseLocationInfo(Parcel in) {
        latitude = in.readDouble();
        longitude = in.readDouble();
        altitude = in.readDouble();
        name = in.readString();
        address = in.readString();
        provider = in.readString();
        bearing = in.readFloat();
        speed = in.readFloat();
        accuracy = in.readFloat();
        vaccuracy = in.readFloat();
        gpsTickCount = in.readLong();
        sysTickCount = in.readLong();
        type = in.readInt();
        sourceFlag = in.readInt();
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeDouble(latitude);
        dest.writeDouble(longitude);
        dest.writeDouble(altitude);
        dest.writeString(name);
        dest.writeString(address);
        dest.writeString(provider);
        dest.writeFloat(bearing);
        dest.writeFloat(speed);
        dest.writeFloat(accuracy);
        dest.writeFloat(vaccuracy);
        dest.writeLong(gpsTickCount);
        dest.writeLong(sysTickCount);
        dest.writeInt(type);
        dest.writeInt(sourceFlag);
    }

    @Override
    public int describeContents() {
        return 0;
    }


    public double getLatitude() {
        return latitude;
    }

    public void setLatitude(double latitude) {
        this.latitude = latitude;
    }

    public double getLongitude() {
        return longitude;
    }

    public void setLongitude(double longitude) {
        this.longitude = longitude;
    }

    public double getAltitude() {
        return altitude;
    }

    public void setAltitude(double altitude) {
        this.altitude = altitude;
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

    public String getProvider() {
        return provider;
    }

    public void setProvider(String provider) {
        this.provider = provider;
    }

    public float getBearing() {
        return bearing;
    }

    public void setBearing(float bearing) {
        this.bearing = bearing;
    }

    public float getSpeed() {
        return speed;
    }

    public void setSpeed(float speed) {
        this.speed = speed;
    }

    public float getAccuracy() {
        return accuracy;
    }

    public void setAccuracy(float accuracy) {
        this.accuracy = accuracy;
    }

    public float getVaccuracy() {
        return vaccuracy;
    }

    public void setVaccuracy(float vaccuracy) {
        this.vaccuracy = vaccuracy;
    }

    public long getGpsTickCount() {
        return gpsTickCount;
    }

    public void setGpsTickCount(long gpsTickCount) {
        this.gpsTickCount = gpsTickCount;
    }

    public long getSysTickCount() {
        return sysTickCount;
    }

    public void setSysTickCount(long sysTickCount) {
        this.sysTickCount = sysTickCount;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getSourceFlag() {
        return sourceFlag;
    }

    public void setSourceFlag(int sourceFlag) {
        this.sourceFlag = sourceFlag;
    }

}
