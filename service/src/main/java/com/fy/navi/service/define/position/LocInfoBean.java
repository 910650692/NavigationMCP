package com.fy.navi.service.define.position;

import com.android.utils.ConvertUtils;

import java.util.Objects;

/**
 * @Author: baipeng0904
 * @Description: 类作用描述
 * @CreateDate: $ $
 */
public class LocInfoBean {
    public static final int GPS_LOCATION = 0;
    public static final int NET_LOCATION = 1;

    private String provider;
    // 航向角度
    private float bearing;
    //单位：公里/小时
    private float speed;
    // 定位精度
    private float accuracy;
    // gps时间戳
    private long gpsTickCount;
    // 系统时间戳
    private long sysTickCount;
    // 垂直方向精度
    private float vaccuracy;
    // 定位类型: -1无效定位，0卫星定位  1wifi定位 2基站定位 3 蓝牙定位 4地磁定位 5未知定位类型
    private int type = GPS_LOCATION;
    // 定位来源: 0:无效 1:GPS 2:北斗 3:GLONASS 4:GALILEO 8:DR 32.WIFI-corrected 64: Base-Station-corrected
    private int sourceFlag;
    //纬度
    private double latitude;
    //经度
    private double longitude;
    //海拔
    private double altitude;
    //地址
    private String address;
    //航向，单位：度，北零顺时针。范围：[0-360)。0 => 北, 90 => 东, 180 => 南, 270 => 西
    private float course;
    private long timeStamp;

    private long roadId;
    private int ownership;

    public long getTimeStamp() {
        return timeStamp;
    }

    public void setTimeStamp(long timeStamp) {
        this.timeStamp = timeStamp;
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

    public float getVaccuracy() {
        return vaccuracy;
    }

    public void setVaccuracy(float vaccuracy) {
        this.vaccuracy = vaccuracy;
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

    public float getCourse() {
        return course;
    }

    public void setCourse(float course) {
        this.course = course;
    }

    public void setRoadId(long roadId) {
        this.roadId = roadId;
    }

    public long getRoadId() {
        return roadId;
    }

    public int getOwnership() {
        return ownership;
    }

    public void setOwnership(int ownership) {
        this.ownership = ownership;
    }

    @Override
    public String toString() {
        return "LocationInfo{" +
                "latitude=" + latitude +
                ", longitude=" + longitude +
                ", altitude=" + altitude +
                ", address='" + address + '\'' +
                ", ownership='" + ownership + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        LocInfoBean that = (LocInfoBean) o;
        return ConvertUtils.equals(Double.valueOf(latitude), Double.valueOf(that.latitude)) &&
                ConvertUtils.equals(Double.valueOf(longitude), Double.valueOf(that.longitude));
    }

    @Override
    public int hashCode() {
        return Objects.hash(latitude, longitude, altitude);
    }
}
