package com.fy.navi.service.greendao.history;

import org.greenrobot.greendao.annotation.Entity;
import org.greenrobot.greendao.annotation.Id;
import org.greenrobot.greendao.annotation.Keep;
import org.greenrobot.greendao.annotation.Property;
import org.greenrobot.greendao.annotation.Unique;

import java.util.Date;
import org.greenrobot.greendao.annotation.Generated;

@Entity
public class History {

    @Id(autoincrement = true)
    private Long mId;

    @Unique
    @Property(nameInDb = "keyWord")
    private String mKeyWord;

    @Property(nameInDb = "poiId")
    private String mPoiId;

    @Property(nameInDb = "startPoint")
    private String mStartPoint;

    @Property(nameInDb = "endPoint")
    private String mEndPoint;

    @Property(nameInDb = "startPoiName")
    private String mStartPoiName;

    @Property(nameInDb = "endPoiName")
    private String mEndPoiName;

    @Property(nameInDb = "type")
    private int mType;

    @Property(nameInDb = "updateTime")
    private Date mUpdateTime;

    @Property(nameInDb = "isCompleted")
    private boolean mIsCompleted; // 是否存在继续导航信息

    @Property(nameInDb = "rideRunType")
    private int mRideRunType = -1; //行程类型（导航/巡航）

    @Property(nameInDb = "timeInterval")
    private int mTimeInterval; // 时长  单位:秒

    @Property(nameInDb = "runDistance")
    private int mRunDistance; // 距离  单位:米

    @Property(nameInDb = "maxSpeed")
    private int mMaxSpeed; // 最快速度  单位:公里/小时

    @Property(nameInDb = "endTime")
    private String mEndTime; // 该行程完成时间

    @Property(nameInDb = "averageSpeed")
    private int mAverageSpeed; // 平均速度  单位:米/秒

    @Keep
    public History(final Long id, final String keyWord, final String poiId, final String startPoint,
            final String endPoint, final String startPoiName, final String endPoiName, final int type,
            final Date updateTime, final boolean isCompleted, final int rideRunType, final int timeInterval,
            final int runDistance, final int maxSpeed, final String endTime, final int averageSpeed) {
        this.mId = id;
        this.mKeyWord = keyWord;
        this.mPoiId = poiId;
        this.mStartPoint = startPoint;
        this.mEndPoint = endPoint;
        this.mStartPoiName = startPoiName;
        this.mEndPoiName = endPoiName;
        this.mType = type;
        this.mUpdateTime = updateTime;
        this.mIsCompleted = isCompleted;
        this.mRideRunType = rideRunType;
        this.mTimeInterval = timeInterval;
        this.mRunDistance = runDistance;
        this.mMaxSpeed = maxSpeed;
        this.mEndTime = endTime;
        this.mAverageSpeed = averageSpeed;
    }

    @Generated(hash = 869423138)
    public History() {
    }


    public Long getMId() {
        return this.mId;
    }

    public void setMId(final Long id) {
        this.mId = id;
    }

    public String getMKeyWord() {
        return this.mKeyWord;
    }

    public void setMKeyWord(final String keyWord) {
        this.mKeyWord = keyWord;
    }

    public String getMPoiId() {
        return this.mPoiId;
    }

    public void setMPoiId(final String poiId) {
        this.mPoiId = poiId;
    }

    public String getMStartPoint() {
        return this.mStartPoint;
    }

    public void setMStartPoint(final String startPoint) {
        this.mStartPoint = startPoint;
    }

    public String getMEndPoint() {
        return this.mEndPoint;
    }

    public void setMEndPoint(final String endPoint) {
        this.mEndPoint = endPoint;
    }

    public String getMStartPoiName() {
        return this.mStartPoiName;
    }

    public void setMStartPoiName(final String startPoiName) {
        this.mStartPoiName = startPoiName;
    }

    public String getMEndPoiName() {
        return this.mEndPoiName;
    }

    public void setMEndPoiName(final String endPoiName) {
        this.mEndPoiName = endPoiName;
    }

    public int getMType() {
        return this.mType;
    }

    public void setMType(final int type) {
        this.mType = type;
    }

    public Date getMUpdateTime() {
        return this.mUpdateTime;
    }

    public void setMUpdateTime(final Date updateTime) {
        this.mUpdateTime = updateTime;
    }

    public boolean getMIsCompleted() {
        return this.mIsCompleted;
    }

    public void setMIsCompleted(final boolean isCompleted) {
        this.mIsCompleted = isCompleted;
    }

    public int getMRideRunType() {
        return this.mRideRunType;
    }

    public void setMRideRunType(final int rideRunType) {
        this.mRideRunType = rideRunType;
    }

    public int getMTimeInterval() {
        return this.mTimeInterval;
    }

    public void setMTimeInterval(final int timeInterval) {
        this.mTimeInterval = timeInterval;
    }

    public int getMRunDistance() {
        return this.mRunDistance;
    }

    public void setMRunDistance(final int runDistance) {
        this.mRunDistance = runDistance;
    }

    public int getMMaxSpeed() {
        return this.mMaxSpeed;
    }

    public void setMMaxSpeed(final int maxSpeed) {
        this.mMaxSpeed = maxSpeed;
    }

    public String getMEndTime() {
        return this.mEndTime;
    }

    public void setMEndTime(final String endTime) {
        this.mEndTime = endTime;
    }

    public int getMAverageSpeed() {
        return this.mAverageSpeed;
    }

    public void setMAverageSpeed(final int averageSpeed) {
        this.mAverageSpeed = averageSpeed;
    }
}
