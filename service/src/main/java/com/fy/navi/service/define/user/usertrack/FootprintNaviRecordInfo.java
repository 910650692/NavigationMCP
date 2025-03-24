package com.fy.navi.service.define.user.usertrack;


public class FootprintNaviRecordInfo {
    private boolean mArrived;
    private int mMonthActTimes;
    private int mMonthDistance;
    private String mMonth;
    private String mOrigin;
    private String mDestination;
    private String mActualDest;
    private String mNaviId;

    private int mDuration;
    private int mDistance;
    private int mAvgSpeed;
    private int mMaxSpeed;
    private String mAvgSpeedDesc;
    private String mAvgSpeedUnit;
    private String mMaxSpeedDesc;
    private String mMaxSpeedUnit;

    private int mMaxCount;
    private int mMinNaviDist;
    private int mMaxNaviDist;
    private String mNaviTime;
    private String mSrcAdcode;
    private String mDstAdcode;

    public int getMaxCount() {
        return mMaxCount;
    }

    public void setMaxCount(final int maxCount) {
        this.mMaxCount = maxCount;
    }

    public int getMinNaviDist() {
        return mMinNaviDist;
    }

    public void setMinNaviDist(final int minNaviDist) {
        this.mMinNaviDist = minNaviDist;
    }

    public int getMaxNaviDist() {
        return mMaxNaviDist;
    }

    public void setMaxNaviDist(final int maxNaviDist) {
        this.mMaxNaviDist = maxNaviDist;
    }

    public String getNaviTime() {
        return mNaviTime;
    }

    public void setNaviTime(final String naviTime) {
        this.mNaviTime = naviTime;
    }

    public String getSrcAdcode() {
        return mSrcAdcode;
    }

    public void setSrcAdcode(final String srcAdcode) {
        this.mSrcAdcode = srcAdcode;
    }

    public String getDstAdcode() {
        return mDstAdcode;
    }

    public void setDstAdcode(final String dstAdcode) {
        this.mDstAdcode = dstAdcode;
    }

    public boolean isArrived() {
        return mArrived;
    }

    public void setArrived(final boolean arrived) {
        this.mArrived = arrived;
    }

    public int getMonthActTimes() {
        return mMonthActTimes;
    }

    public void setMonthActTimes(final int monthActTimes) {
        this.mMonthActTimes = monthActTimes;
    }

    public int getMonthDistance() {
        return mMonthDistance;
    }

    public void setMonthDistance(final int monthDistance) {
        this.mMonthDistance = monthDistance;
    }

    public String getMonth() {
        return mMonth;
    }

    public void setMonth(final String month) {
        this.mMonth = month;
    }

    public String getOrigin() {
        return mOrigin;
    }

    public void setOrigin(final String origin) {
        this.mOrigin = origin;
    }

    public String getDestination() {
        return mDestination;
    }

    public void setDestination(final String destination) {
        this.mDestination = destination;
    }

    public String getActualDest() {
        return mActualDest;
    }

    public void setActualDest(final String actualDest) {
        this.mActualDest = actualDest;
    }

    public String getNaviId() {
        return mNaviId;
    }

    public void setNaviId(final String naviId) {
        this.mNaviId = naviId;
    }


    public int getDuration() {
        return mDuration;
    }

    public void setDuration(final int duration) {
        this.mDuration = duration;
    }

    public int getDistance() {
        return mDistance;
    }

    public void setDistance(final int distance) {
        this.mDistance = distance;
    }

    public int getAvgSpeed() {
        return mAvgSpeed;
    }

    public void setAvgSpeed(final int avgSpeed) {
        this.mAvgSpeed = avgSpeed;
    }

    public int getMaxSpeed() {
        return mMaxSpeed;
    }

    public void setMaxSpeed(final int maxSpeed) {
        this.mMaxSpeed = maxSpeed;
    }

    public String getAvgSpeedDesc() {
        return mAvgSpeedDesc;
    }

    public void setAvgSpeedDesc(final String avgSpeedDesc) {
        this.mAvgSpeedDesc = avgSpeedDesc;
    }

    public String getAvgSpeedUnit() {
        return mAvgSpeedUnit;
    }

    public void setAvgSpeedUnit(final String avgSpeedUnit) {
        this.mAvgSpeedUnit = avgSpeedUnit;
    }

    public String getMaxSpeedDesc() {
        return mMaxSpeedDesc;
    }

    public void setMaxSpeedDesc(final String maxSpeedDesc) {
        this.mMaxSpeedDesc = maxSpeedDesc;
    }

    public String getMaxSpeedUnit() {
        return mMaxSpeedUnit;
    }

    public void setMaxSpeedUnit(final String maxSpeedUnit) {
        this.mMaxSpeedUnit = maxSpeedUnit;
    }
}
