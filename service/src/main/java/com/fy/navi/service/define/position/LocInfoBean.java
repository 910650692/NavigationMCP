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
    //导航段
    private int mSegmCur;
    //道路段
    private int mLinkCur;
    //最后一个点索引
    private int mPostCur;
    private float mMatchRoadCourse;
    private float mCourseAcc;
    private int mStartDirType;
    private float mGpsDir;
    private float mCompassDir;
    private int mFormway;
    private int mStartPosType;
    private float mFittingCourse;
    private int mRoadDir;
    private float mFittingCourseAcc;
    private String mRequestRouteInfo;
    private int onGuideRoad;
    private int roadClass;

    public int getRoadClass() {
        return roadClass;
    }

    public void setRoadClass(int roadClass) {
        this.roadClass = roadClass;
    }

    public int getOnGuideRoad() {
        return onGuideRoad;
    }

    public void setOnGuideRoad(int onGuideRoad) {
        this.onGuideRoad = onGuideRoad;
    }

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

    public int getSegmCur() {
        return mSegmCur;
    }

    public void setSegmCur(int segmCur) {
        this.mSegmCur = segmCur;
    }

    public int getLinkCur() {
        return mLinkCur;
    }

    public void setLinkCur(int linkCur) {
        this.mLinkCur = linkCur;
    }

    public int getPostCur() {
        return mPostCur;
    }

    public void setPostCur(int postCur) {
        this.mPostCur = postCur;
    }

    public float getMatchRoadCourse() {
        return mMatchRoadCourse;
    }

    public void setMatchRoadCourse(float matchRoadCourse) {
        this.mMatchRoadCourse = matchRoadCourse;
    }

    public float getCourseAcc() {
        return mCourseAcc;
    }

    public void setCourseAcc(float courseAcc) {
        this.mCourseAcc = courseAcc;
    }

    public int getStartDirType() {
        return mStartDirType;
    }

    public void setStartDirType(int startDirType) {
        this.mStartDirType = startDirType;
    }

    public float getGpsDir() {
        return mGpsDir;
    }

    public void setGpsDir(float gpsDir) {
        this.mGpsDir = gpsDir;
    }

    public float getCompassDir() {
        return mCompassDir;
    }

    public void setCompassDir(float compassDir) {
        this.mCompassDir = compassDir;
    }

    public int getFormway() {
        return mFormway;
    }

    public void setFormway(int formway) {
        this.mFormway = formway;
    }

    public int getStartPosType() {
        return mStartPosType;
    }

    public void setStartPosType(int startPosType) {
        this.mStartPosType = startPosType;
    }

    public float getFittingCourse() {
        return mFittingCourse;
    }

    public void setFittingCourse(float fittingCourse) {
        this.mFittingCourse = fittingCourse;
    }

    public int getRoadDir() {
        return mRoadDir;
    }

    public void setRoadDir(int roadDir) {
        this.mRoadDir = roadDir;
    }

    public float getFittingCourseAcc() {
        return mFittingCourseAcc;
    }

    public void setFittingCourseAcc(float fittingCourseAcc) {
        this.mFittingCourseAcc = fittingCourseAcc;
    }

    public String getRequestRouteInfo() {
        return mRequestRouteInfo;
    }

    public void setRequestRouteInfo(String requestRouteInfo) {
        this.mRequestRouteInfo = requestRouteInfo;
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
                ", mSegmCur='" + mSegmCur + '\'' +
                ", mLinkCur='" + mLinkCur + '\'' +
                ", mPostCur='" + mPostCur + '\'' +
                ", mMatchRoadCourse='" + mMatchRoadCourse + '\'' +
                ", mCourseAcc='" + mCourseAcc + '\'' +
                ", mStartDirType='" + mStartDirType + '\'' +
                ", mGpsDir='" + mGpsDir + '\'' +
                ", mCompassDir='" + mCompassDir + '\'' +
                ", mFormway='" + mFormway + '\'' +
                ", mStartPosType='" + mStartPosType + '\'' +
                ", mFittingCourse='" + mFittingCourse + '\'' +
                ", mRoadDir='" + mRoadDir + '\'' +
                ", mFittingCourseAcc='" + mFittingCourseAcc + '\'' +
                ", mRequestRouteInfo='" + mRequestRouteInfo + '\'' +
                ", onGuideRoad='" + onGuideRoad + '\'' +
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
