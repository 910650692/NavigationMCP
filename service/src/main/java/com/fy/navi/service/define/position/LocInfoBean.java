package com.fy.navi.service.define.position;

import com.android.utils.ConvertUtils;

import java.util.Objects;

public class LocInfoBean {

    public static final int GPS_LOCATION = 0;
    public static final int NET_LOCATION = 1;

    private String mProvider;
    // 航向角度
    private float mBearing;
    //单位：公里/小时
    private float mSpeed;
    // 定位精度
    private float mAccuracy;
    // gps时间戳
    private long mGpsTickCount;
    // 系统时间戳
    private long mSysTickCount;
    // 垂直方向精度
    private float mVaccuracy;
    // 定位类型: -1无效定位，0卫星定位  1wifi定位 2基站定位 3 蓝牙定位 4地磁定位 5未知定位类型
    private int mType = GPS_LOCATION;
    // 定位来源: 0:无效 1:GPS 2:北斗 3:GLONASS 4:GALILEO 8:DR 32.WIFI-corrected 64: Base-Station-corrected
    private int mSourceFlag;
    //纬度
    private double mLatitude;
    //经度
    private double mLongitude;
    //海拔
    private double mAltitude;
    //地址
    private String mAddress;
    //航向，单位：度，北零顺时针。范围：[0-360)。0 => 北, 90 => 东, 180 => 南, 270 => 西
    private float mCourse;
    private long mTimeStamp;

    private long mRoadId;
    private int mOwnership;

    private int mLinkType;

    public long getTimeStamp() {
        return mTimeStamp;
    }

    public void setTimeStamp(final long timeStamp) {
        mTimeStamp = timeStamp;
    }

    public double getLatitude() {
        return mLatitude;
    }

    public void setLatitude(final double latitude) {
        mLatitude = latitude;
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
        mAltitude = altitude;
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

    public float getVaccuracy() {
        return mVaccuracy;
    }

    public void setVaccuracy(final float vaccuracy) {
        mVaccuracy = vaccuracy;
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

    public float getCourse() {
        return mCourse;
    }

    public void setCourse(final float course) {
        mCourse = course;
    }

    public void setRoadId(final long roadId) {
        mRoadId = roadId;
    }

    public long getRoadId() {
        return mRoadId;
    }

    public int getOwnership() {
        return mOwnership;
    }

    public void setOwnership(final int ownership) {
        mOwnership = ownership;
    }

    public int getLinkType() {
        return mLinkType;
    }

    public void setLinkType(final int linkType) {
        mLinkType = linkType;
    }

    @Override
    public String toString() {
        return "LocationInfo{" +
                "latitude=" + mLatitude +
                ", longitude=" + mLongitude +
                ", altitude=" + mAltitude +
                ", address='" + mAddress + '\'' +
                ", ownership='" + mOwnership + '\'' +
                ", linkType='" + mLinkType + '\'' +
                '}';
    }

    @Override
    public boolean equals(final Object o) {
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        if (this == o) {
            return true;
        }
        final LocInfoBean that = (LocInfoBean) o;
        return ConvertUtils.equals(Double.valueOf(mLatitude), Double.valueOf(that.mLatitude)) &&
                ConvertUtils.equals(Double.valueOf(mLongitude), Double.valueOf(that.mLongitude));
    }

    @Override
    public int hashCode() {
        return Objects.hash(mLatitude, mLongitude, mAltitude);
    }

}
