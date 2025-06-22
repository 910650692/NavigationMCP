package com.sgm.navi.service.define.navi;

import androidx.annotation.NonNull;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;

import lombok.Getter;
import lombok.Setter;


public class NaviDriveReportEntity {
    //c车辆类型 默认0，0:小车，1:货车, 2:纯电动车，3:纯电动货车，4:插电式混动汽车，5:插电式混动货车
    @Getter
    @Setter
    private int mVehicleType;
    @Getter
    @Setter
    private ArrayList<NaviDriveEventEntity> mDriverEventList;
    private NaviStatisticsInfoEntity mBlNaviStatisticsInfo;

    @Setter
    @Getter
    public static class NaviDriveEventEntity {
        //未定义0，急左转2, 急右转3, 急左并道4, 急右并道5, 急加速6
        private int mType;
        //事件严重程度
        private int mLevel;
        //经度
        private float mLon;
        //纬度
        private float mLat;
        //事件开始时间
        private long mTime;

        public NaviDriveEventEntity(final float lat, final float lon, final int type,
                                    final long time, final int level) {
            this.mLat = lat;
            this.mLon = lon;
            this.mType = type;
            this.mLevel = level;
            this.mTime = time;
        }

        public int getType() {
            return mType;
        }

        public void setType(final int type) {
            this.mType = type;
        }

        public int getLevel() {
            return mLevel;
        }

        public void setLevel(final int level) {
            this.mLevel = level;
        }

        public float getLon() {
            return mLon;
        }

        public void setLon(final float lon) {
            this.mLon = lon;
        }

        public float getLat() {
            return mLat;
        }

        public void setLat(final float lat) {
            this.mLat = lat;
        }

        public long getTime() {
            return mTime;
        }

        public void setTime(final long time) {
            this.mTime = time;
        }

        @NonNull
        @Override
        public String toString() {
            return "DriveEventEntity{" +
                    "type=" + mType +
                    ", level=" + mLevel +
                    ", lon=" + mLon +
                    ", lat=" + mLat +
                    ", time=" + mTime +
                    '}';
        }
    }

    @Setter
    @Getter
    public static class NaviStatisticsInfoEntity {
        //导航开始时间UTC
        private BigInteger mStartUTC;
        //导航起始时间在当天的秒数
        private int mStartSecond;
        //同一起止点常规路线用时
        private int mNormalRouteTime;
        private int mSavedTime;
        //估算的行驶用时，不考虑偏航的影响，单位：秒
        private int mEstimateTime;
        //估算的行驶里程，不考虑偏航的影响，单位：米
        private int mEstimateDist;
        //实际的行驶用时，包括中途停车时间在内
        private int mDrivenTime;
        //实际的行驶里程
        private int mDrivenDist;
        //平均速度, 实际的行驶里程／实际的行驶用时, 单位：公里／小时
        private int mAverageSpeed;
        //最高速度, 单位：公里／小时
        private int mHighestSpeed;
        //电子眼播报的超速次数，关闭电子眼播报则没有计数
        private int mOverspeedCount;
        //高速路上的超速次数
        private int mOverspeedCountEx;
        //高速路上的超速低于20次数
        private int mHighwayOverSpeedLowCnt;
        //高速路上的超速20%-50次数
        private int mHighwayOverSpeedMidCnt;
        //高速路上的超速大于50次数
        private int mHighwayOverSpeedHighCnt;
        //普通路上的超速低于20次数
        private int mNormalOverSpeedLowCnt;
        //普通路上的超速20%-50次数
        private int mNormalOverSpeedMidCnt;
        //普通路上的超速大于50次数
        private int mNormalOverSpeedHighCnt;
        //途径事故高发地的次数
        private int mAccidentAreaCount;
        //偏航次数
        private int mRerouteCount;
        //急刹车次数
        private int mBrakesCount;
        //等待及拥堵时间 , 单位：秒
        private int mSlowTime;
        //各类路况的累积距离，0 - 4 未知– 极度拥堵
        private float[] mArrTrafficDist;
        //各等级道路的累积距离， 约定level 0 为高速，level 11 为未知
        private float[] mArrRoadDist;
        //各类瞬时速度值累积, 10km/h一个等级
        private int[] mArrSpeedClass;

        public NaviStatisticsInfoEntity() {
        }

        public NaviStatisticsInfoEntity(final BigInteger startUTC, final int startSecond,
                                        final int normalRouteTime,
                                        final int savedTime, final int estimateTime,
                                        final int estimateDist,
                                        final int drivenTime, final int drivenDist,
                                        final int averageSpeed,
                                        final int highestSpeed, final int overspeedCount,
                                        final int overspeedCountEx,
                                        final int highwayOverSpeedLowCnt,
                                        final int highwayOverSpeedMidCnt,
                                        final int highwayOverSpeedHighCnt,
                                        final int normalOverSpeedLowCnt,
                                        final int normalOverSpeedMidCnt,
                                        final int normalOverSpeedHighCnt,
                                        final int accidentAreaCount,
                                        final int rerouteCount, final int brakesCount,
                                        final int slowTime, final float[] arrTrafficDist,
                                        final float[] arrRoadDist,
                                        final int[] arrSpeedClass) {
            this.mStartUTC = startUTC;
            this.mStartSecond = startSecond;
            this.mNormalRouteTime = normalRouteTime;
            this.mSavedTime = savedTime;
            this.mEstimateTime = estimateTime;
            this.mEstimateDist = estimateDist;
            this.mDrivenTime = drivenTime;
            this.mDrivenDist = drivenDist;
            this.mAverageSpeed = averageSpeed;
            this.mHighestSpeed = highestSpeed;
            this.mOverspeedCount = overspeedCount;
            this.mOverspeedCountEx = overspeedCountEx;
            this.mHighwayOverSpeedLowCnt = highwayOverSpeedLowCnt;
            this.mHighwayOverSpeedMidCnt = highwayOverSpeedMidCnt;
            this.mHighwayOverSpeedHighCnt = highwayOverSpeedHighCnt;
            this.mNormalOverSpeedLowCnt = normalOverSpeedLowCnt;
            this.mNormalOverSpeedMidCnt = normalOverSpeedMidCnt;
            this.mNormalOverSpeedHighCnt = normalOverSpeedHighCnt;
            this.mAccidentAreaCount = accidentAreaCount;
            this.mRerouteCount = rerouteCount;
            this.mBrakesCount = brakesCount;
            this.mSlowTime = slowTime;
            this.mArrTrafficDist = arrTrafficDist;
            this.mArrRoadDist = arrRoadDist;
            this.mArrSpeedClass = arrSpeedClass;
        }

        public BigInteger getStartUTC() {
            return mStartUTC;
        }

        public void setStartUTC(final BigInteger startUTC) {
            this.mStartUTC = startUTC;
        }

        public int getStartSecond() {
            return mStartSecond;
        }

        public void setStartSecond(final int startSecond) {
            this.mStartSecond = startSecond;
        }

        public int getNormalRouteTime() {
            return mNormalRouteTime;
        }

        public void setNormalRouteTime(final int normalRouteTime) {
            this.mNormalRouteTime = normalRouteTime;
        }

        public int getSavedTime() {
            return mSavedTime;
        }

        public void setSavedTime(final int savedTime) {
            this.mSavedTime = savedTime;
        }

        public int getEstimateTime() {
            return mEstimateTime;
        }

        public void setEstimateTime(final int estimateTime) {
            this.mEstimateTime = estimateTime;
        }

        public int getEstimateDist() {
            return mEstimateDist;
        }

        public void setEstimateDist(final int estimateDist) {
            this.mEstimateDist = estimateDist;
        }

        public int getDrivenTime() {
            return mDrivenTime;
        }

        public void setDrivenTime(final int drivenTime) {
            this.mDrivenTime = drivenTime;
        }

        public int getDrivenDist() {
            return mDrivenDist;
        }

        public void setDrivenDist(final int drivenDist) {
            this.mDrivenDist = drivenDist;
        }

        public int getAverageSpeed() {
            return mAverageSpeed;
        }

        public void setAverageSpeed(final int averageSpeed) {
            this.mAverageSpeed = averageSpeed;
        }

        public int getHighestSpeed() {
            return mHighestSpeed;
        }

        public void setHighestSpeed(final int highestSpeed) {
            this.mHighestSpeed = highestSpeed;
        }

        public int getOverspeedCount() {
            return mOverspeedCount;
        }

        public void setOverspeedCount(final int overspeedCount) {
            this.mOverspeedCount = overspeedCount;
        }

        public int getOverspeedCountEx() {
            return mOverspeedCountEx;
        }

        public void setOverspeedCountEx(final int overspeedCountEx) {
            this.mOverspeedCountEx = overspeedCountEx;
        }

        public int getHighwayOverSpeedLowCnt() {
            return mHighwayOverSpeedLowCnt;
        }

        public void setHighwayOverSpeedLowCnt(final int highwayOverSpeedLowCnt) {
            this.mHighwayOverSpeedLowCnt = highwayOverSpeedLowCnt;
        }

        public int getHighwayOverSpeedMidCnt() {
            return mHighwayOverSpeedMidCnt;
        }

        public void setHighwayOverSpeedMidCnt(final int highwayOverSpeedMidCnt) {
            this.mHighwayOverSpeedMidCnt = highwayOverSpeedMidCnt;
        }

        public int getHighwayOverSpeedHighCnt() {
            return mHighwayOverSpeedHighCnt;
        }

        public void setHighwayOverSpeedHighCnt(final int highwayOverSpeedHighCnt) {
            this.mHighwayOverSpeedHighCnt = highwayOverSpeedHighCnt;
        }

        public int getNormalOverSpeedLowCnt() {
            return mNormalOverSpeedLowCnt;
        }

        public void setNormalOverSpeedLowCnt(final int normalOverSpeedLowCnt) {
            this.mNormalOverSpeedLowCnt = normalOverSpeedLowCnt;
        }

        public int getNormalOverSpeedMidCnt() {
            return mNormalOverSpeedMidCnt;
        }

        public void setNormalOverSpeedMidCnt(final int normalOverSpeedMidCnt) {
            this.mNormalOverSpeedMidCnt = normalOverSpeedMidCnt;
        }

        public int getNormalOverSpeedHighCnt() {
            return mNormalOverSpeedHighCnt;
        }

        public void setNormalOverSpeedHighCnt(final int normalOverSpeedHighCnt) {
            this.mNormalOverSpeedHighCnt = normalOverSpeedHighCnt;
        }

        public int getAccidentAreaCount() {
            return mAccidentAreaCount;
        }

        public void setAccidentAreaCount(final int accidentAreaCount) {
            this.mAccidentAreaCount = accidentAreaCount;
        }

        public int getRerouteCount() {
            return mRerouteCount;
        }

        public void setRerouteCount(final int rerouteCount) {
            this.mRerouteCount = rerouteCount;
        }

        public int getBrakesCount() {
            return mBrakesCount;
        }

        public void setBrakesCount(final int brakesCount) {
            this.mBrakesCount = brakesCount;
        }

        public int getSlowTime() {
            return mSlowTime;
        }

        public void setSlowTime(final int slowTime) {
            this.mSlowTime = slowTime;
        }

        public float[] getArrTrafficDist() {
            return mArrTrafficDist;
        }

        public void setArrTrafficDist(final float[] arrTrafficDist) {
            this.mArrTrafficDist = arrTrafficDist;
        }

        public float[] getArrRoadDist() {
            return mArrRoadDist;
        }

        public void setArrRoadDist(final float[] arrRoadDist) {
            this.mArrRoadDist = arrRoadDist;
        }

        public int[] getArrSpeedClass() {
            return mArrSpeedClass;
        }

        public void setArrSpeedClass(final int[] arrSpeedClass) {
            this.mArrSpeedClass = arrSpeedClass;
        }

        @NonNull
        @Override
        public String toString() {
            return "NaviStatisticsInfo{" +
                    "startUTC=" + mStartUTC +
                    ", startSecond=" + mStartSecond +
                    ", normalRouteTime=" + mNormalRouteTime +
                    ", savedTime=" + mSavedTime +
                    ", estimateTime=" + mEstimateTime +
                    ", estimateDist=" + mEstimateDist +
                    ", drivenTime=" + mDrivenTime +
                    ", drivenDist=" + mDrivenDist +
                    ", averageSpeed=" + mAverageSpeed +
                    ", highestSpeed=" + mHighestSpeed +
                    ", overspeedCount=" + mOverspeedCount +
                    ", overspeedCountEx=" + mOverspeedCountEx +
                    ", highwayOverSpeedLowCnt=" + mHighwayOverSpeedLowCnt +
                    ", highwayOverSpeedMidCnt=" + mHighwayOverSpeedMidCnt +
                    ", highwayOverSpeedHighCnt=" + mHighwayOverSpeedHighCnt +
                    ", normalOverSpeedLowCnt=" + mNormalOverSpeedLowCnt +
                    ", normalOverSpeedMidCnt=" + mNormalOverSpeedMidCnt +
                    ", normalOverSpeedHighCnt=" + mNormalOverSpeedHighCnt +
                    ", accidentAreaCount=" + mAccidentAreaCount +
                    ", rerouteCount=" + mRerouteCount +
                    ", brakesCount=" + mBrakesCount +
                    ", slowTime=" + mSlowTime +
                    ", arrTrafficDist=" + Arrays.toString(mArrTrafficDist) +
                    ", arrRoadDist=" + Arrays.toString(mArrRoadDist) +
                    ", arrSpeedClass=" + Arrays.toString(mArrSpeedClass) +
                    '}';
        }

    }

    public NaviStatisticsInfoEntity getNaviStatisticsInfoEntity() {
        return mBlNaviStatisticsInfo;
    }

    public void setNaviStatisticsInfoEntity(final NaviStatisticsInfoEntity blNaviStatisticsInfo) {
        this.mBlNaviStatisticsInfo = blNaviStatisticsInfo;
    }

    public int getVehicleType() {
        return mVehicleType;
    }

    public void setVehicleType(final int vehicleType) {
        this.mVehicleType = vehicleType;
    }

    public ArrayList<NaviDriveEventEntity> getDriverEventList() {
        return mDriverEventList;
    }

    public void setDriverEventList(final ArrayList<NaviDriveEventEntity> driverEventList) {
        this.mDriverEventList = driverEventList;
    }

    public NaviStatisticsInfoEntity getBlNaviStatisticsInfo() {
        return mBlNaviStatisticsInfo;
    }

    public void setBlNaviStatisticsInfo(final NaviStatisticsInfoEntity blNaviStatisticsInfo) {
        this.mBlNaviStatisticsInfo = blNaviStatisticsInfo;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviDriveReportEntity{" +
                "vehicleType=" + mVehicleType +
                ", driverEventList=" + mDriverEventList +
                ", blNaviStatisticsInfo=" + mBlNaviStatisticsInfo +
                '}';
    }
}
