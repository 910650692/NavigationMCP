package com.fy.navi.service.greendao.history;

import org.greenrobot.greendao.annotation.Entity;
import org.greenrobot.greendao.annotation.Id;
import org.greenrobot.greendao.annotation.Property;
import org.greenrobot.greendao.annotation.Unique;

import java.util.Date;
import org.greenrobot.greendao.annotation.Generated;

@Entity
public class History {

    @Id(autoincrement = true)
    public Long id;

    @Unique
    @Property(nameInDb = "keyWord")
    public String keyWord;

    @Property(nameInDb = "poiId")
    public String poiId;

    @Property(nameInDb = "startPoint")
    public String startPoint;

    @Property(nameInDb = "endPoint")
    public String endPoint;

    @Property(nameInDb = "startPoiName")
    public String startPoiName;

    @Property(nameInDb = "endPoiName")
    public String endPoiName;

    @Property(nameInDb = "type")
    public int type;

    @Property(nameInDb = "updateTime")
    public Date updateTime;

    @Property(nameInDb = "isCompleted")
    public boolean isCompleted; // 是否存在继续导航信息

    @Property(nameInDb = "rideRunType")
    public int rideRunType = -1; //行程类型（导航/巡航）

    @Property(nameInDb = "timeInterval")
    public int timeInterval; // 时长  单位:秒

    @Property(nameInDb = "runDistance")
    public int runDistance; // 距离  单位:米

    @Property(nameInDb = "maxSpeed")
    public int maxSpeed; // 最快速度  单位:公里/小时

    @Property(nameInDb = "endTime")
    public String endTime; // 该行程完成时间

    @Property(nameInDb = "averageSpeed")
    public int averageSpeed; // 平均速度  单位:米/秒

    @Generated(hash = 1355772610)
    public History(Long id, String keyWord, String poiId, String startPoint,
            String endPoint, String startPoiName, String endPoiName, int type,
            Date updateTime, boolean isCompleted, int rideRunType, int timeInterval,
            int runDistance, int maxSpeed, String endTime, int averageSpeed) {
        this.id = id;
        this.keyWord = keyWord;
        this.poiId = poiId;
        this.startPoint = startPoint;
        this.endPoint = endPoint;
        this.startPoiName = startPoiName;
        this.endPoiName = endPoiName;
        this.type = type;
        this.updateTime = updateTime;
        this.isCompleted = isCompleted;
        this.rideRunType = rideRunType;
        this.timeInterval = timeInterval;
        this.runDistance = runDistance;
        this.maxSpeed = maxSpeed;
        this.endTime = endTime;
        this.averageSpeed = averageSpeed;
    }

    @Generated(hash = 869423138)
    public History() {
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getKeyWord() {
        return keyWord;
    }

    public void setKeyWord(String keyWord) {
        this.keyWord = keyWord;
    }

    public String getPoiId() {
        return poiId;
    }

    public void setPoiId(String poiId) {
        this.poiId = poiId;
    }

    public String getStartPoint() {
        return startPoint;
    }

    public void setStartPoint(String startPoint) {
        this.startPoint = startPoint;
    }

    public String getEndPoint() {
        return endPoint;
    }

    public void setEndPoint(String endPoint) {
        this.endPoint = endPoint;
    }

    public String getStartPoiName() {
        return startPoiName;
    }

    public void setStartPoiName(String startPoiName) {
        this.startPoiName = startPoiName;
    }

    public String getEndPoiName() {
        return endPoiName;
    }

    public void setEndPoiName(String endPoiName) {
        this.endPoiName = endPoiName;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public Date getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(Date updateTime) {
        this.updateTime = updateTime;
    }

    public boolean isCompleted() {
        return isCompleted;
    }

    public void setCompleted(boolean completed) {
        isCompleted = completed;
    }

    public boolean getIsCompleted() {
        return this.isCompleted;
    }

    public void setIsCompleted(boolean isCompleted) {
        this.isCompleted = isCompleted;
    }

    public int getRideRunType() {
        return rideRunType;
    }

    public void setRideRunType(int rideRunType) {
        this.rideRunType = rideRunType;
    }

    public int getTimeInterval() {
        return timeInterval;
    }

    public void setTimeInterval(int timeInterval) {
        this.timeInterval = timeInterval;
    }

    public int getRunDistance() {
        return runDistance;
    }

    public void setRunDistance(int runDistance) {
        this.runDistance = runDistance;
    }

    public int getMaxSpeed() {
        return maxSpeed;
    }

    public void setMaxSpeed(int maxSpeed) {
        this.maxSpeed = maxSpeed;
    }

    public String getEndTime() {
        return endTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    public int getAverageSpeed() {
        return averageSpeed;
    }

    public void setAverageSpeed(int averageSpeed) {
        this.averageSpeed = averageSpeed;
    }
}
