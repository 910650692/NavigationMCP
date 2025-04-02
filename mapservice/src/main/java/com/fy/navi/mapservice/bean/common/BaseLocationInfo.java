package com.fy.navi.mapservice.bean.common;

import android.os.Parcel;
import android.os.Parcelable;

public class BaseLocationInfo implements Parcelable {

    private double mLatitude;
    private double mLongitude;
    private double mAltitude;
    private String mName;
    private String mAddress;
    //原生定位provider
    private String mProvider;
    // 航向角度
    private float mBearing;
    private float mSpeed; //单位km/h
    // 定位精度
    private float mAccuracy;
    // 垂直方向精度
    private float mVaccuracy;
    // gps时间戳
    private long mGpsTickCount;
    // 系统时间戳
    private long mSysTickCount;
    // 定位类型: -1无效定位，0卫星定位  1wifi定位 2基站定位 3 蓝牙定位 4地磁定位 5未知定位类型
    private int mType;
    // 定位来源: 0:无效 1:GPS 2:北斗 3:GLONASS 4:GALILEO 8:DR 32.WIFI-corrected 64: Base-Station-corrected
    private int mSourceFlag;

    public static final Creator<BaseLocationInfo> CREATOR = new Creator<BaseLocationInfo>() {

        @Override
        public BaseLocationInfo createFromParcel(final Parcel source) {
            return new BaseLocationInfo(source);
        }

        @Override
        public BaseLocationInfo[] newArray(final int size) {
            return new BaseLocationInfo[size];
        }
    };

    public BaseLocationInfo() {

    }

    public BaseLocationInfo(final Parcel in) {
        mLatitude = in.readDouble();
        mLongitude = in.readDouble();
        mAltitude = in.readDouble();
        mName = in.readString();
        mAddress = in.readString();
        mProvider = in.readString();
        mBearing = in.readFloat();
        mSpeed = in.readFloat();
        mAccuracy = in.readFloat();
        mVaccuracy = in.readFloat();
        mGpsTickCount = in.readLong();
        mSysTickCount = in.readLong();
        mType = in.readInt();
        mSourceFlag = in.readInt();
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeDouble(mLatitude);
        dest.writeDouble(mLongitude);
        dest.writeDouble(mAltitude);
        dest.writeString(mName);
        dest.writeString(mAddress);
        dest.writeString(mProvider);
        dest.writeFloat(mBearing);
        dest.writeFloat(mSpeed);
        dest.writeFloat(mAccuracy);
        dest.writeFloat(mVaccuracy);
        dest.writeLong(mGpsTickCount);
        dest.writeLong(mSysTickCount);
        dest.writeInt(mType);
        dest.writeInt(mSourceFlag);
    }

    @Override
    public int describeContents() {
        return 0;
    }


    public double getLatitude() {
        return mLatitude;
    }

    public void setLatitude(final double latitude) {
        mLatitude= latitude;
    }

    public double getLongitude() {
        return mLongitude;
    }

    public void setLongitude(final double longitude) {
        mLongitude = longitude;
    }

    public double getAltitude() {
        return mAltitude;
    }

    public void setAltitude(final double altitude) {
        mAltitude= altitude;
    }

    public String getName() {
        return mName;
    }

    public void setName(final String name) {
        mName = name;
    }

    public String getAddress() {
        return mAddress;
    }

    public void setAddress(final String address) {
        mAddress = address;
    }

    public String getProvider() {
        return mProvider;
    }

    public void setProvider(final String provider) {
        mProvider = provider;
    }

    public float getBearing() {
        return mBearing;
    }

    public void setBearing(final float bearing) {
        mBearing = bearing;
    }

    public float getSpeed() {
        return mSpeed;
    }

    public void setSpeed(final float speed) {
        mSpeed = speed;
    }

    public float getAccuracy() {
        return mAccuracy;
    }

    public void setAccuracy(final float accuracy) {
        mAccuracy = accuracy;
    }

    public float getVaccuracy() {
        return mVaccuracy;
    }

    public void setVaccuracy(final float vaccuracy) {
        mVaccuracy = vaccuracy;
    }

    public long getGpsTickCount() {
        return mGpsTickCount;
    }

    public void setGpsTickCount(final long gpsTickCount) {
        mGpsTickCount = gpsTickCount;
    }

    public long getSysTickCount() {
        return mSysTickCount;
    }

    public void setSysTickCount(final long sysTickCount) {
        mSysTickCount = sysTickCount;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        mType = type;
    }

    public int getSourceFlag() {
        return mSourceFlag;
    }

    public void setSourceFlag(final int sourceFlag) {
        mSourceFlag = sourceFlag;
    }

}
