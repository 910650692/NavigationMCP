package com.sgm.navi.service.greendao.history;

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

    @Property(nameInDb = "naviHistoryId")
    private String mNaviHistoryId;

    @Property(nameInDb = "address")
    private String mAddress;

    @Property(nameInDb = "startPoint")
    private String mStartPoint;

    @Property(nameInDb = "endPoint")
    private String mEndPoint;

    @Property(nameInDb = "startPoiName")
    private String mStartPoiName;

    @Property(nameInDb = "endPoiName")
    private String mEndPoiName;

    @Property(nameInDb = "type")
    private int mType; //0:关键字记录(搜索记录) 1:导航记录 2:行程历史记录

    @Property(nameInDb = "updateTime")
    private Date mUpdateTime;

    @Property(nameInDb = "isCompleted")
    private boolean mIsCompleted; // 是否存在继续导航信息

    @Property(nameInDb = "rideRunType")
    private int mRideRunType = -1; //行程类型（1:导航/0:巡航）

    @Property(nameInDb = "timeInterval")
    private int mTimeInterval; // 时长  单位:秒

    @Property(nameInDb = "runDistance")
    private int mRunDistance; // 距离  单位:米

    @Property(nameInDb = "maxSpeed")
    private int mMaxSpeed; // 最快速度  单位:公里/小时

    @Property(nameInDb = "viaPoint")
    private String mViaPoint; //存储的是RouteSpeechRequestParam信息

    @Property(nameInDb = "startTime")
    private String mStartTime;

    @Property(nameInDb = "averageSpeed")
    private int mAverageSpeed; // 平均速度  单位:米/秒

    @Property(nameInDb = "trackFileName")
    private String mTrackFileName;

    @Property(nameInDb = "filePath")
    private String mFilePath;


    @Generated(hash = 869423138)
    public History() {
    }

    @Keep
    public History(final Long id, final String keyWord, final String poiId, final String naviHistoryId,
                   final String address, final String startPoint, final String endPoint,
                   final String startPoiName, final String endPoiName, final int type, final Date updateTime,
                   final boolean isCompleted, final int rideRunType, final int timeInterval,
                   final int runDistance, final int maxSpeed, final String viaPoint, final String startName,
                   final int averageSpeed, final String trackFileName, final String filePath) {
        this.mId = id;
        this.mKeyWord = keyWord;
        this.mPoiId = poiId;
        this.mNaviHistoryId = naviHistoryId;
        this.mAddress = address;
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
        this.mViaPoint = viaPoint;
        this.mStartTime = startName;
        this.mAverageSpeed = averageSpeed;
        this.mTrackFileName = trackFileName;
        this.mFilePath = filePath;
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

    public String getMStartTime() {
        return this.mStartTime;
    }

    public void setMStartTime(final String startTime) {
        this.mStartTime = startTime;
    }

    public int getMAverageSpeed() {
        return this.mAverageSpeed;
    }

    public void setMAverageSpeed(final int averageSpeed) {
        this.mAverageSpeed = averageSpeed;
    }

    public String getMTrackFileName() {
        return this.mTrackFileName;
    }

    public void setMTrackFileName(final String trackFileName) {
        this.mTrackFileName = trackFileName;
    }

    public String getMFilePath() {
        return this.mFilePath;
    }

    public void setMFilePath(final String filePath) {
        this.mFilePath = filePath;
    }

    public String getMNaviHistoryId() {
        return this.mNaviHistoryId;
    }

    public void setMNaviHistoryId(final String naviHistoryId) {
        this.mNaviHistoryId = naviHistoryId;
    }

    public String getMAddress() {
        return mAddress;
    }

    public void setMAddress(String address) {
        this.mAddress = address;
    }

    public String getMViaPoint() {
        return this.mViaPoint;
    }

    public void setMViaPoint(String viaPoint) {
        this.mViaPoint = viaPoint;
    }
    
}
