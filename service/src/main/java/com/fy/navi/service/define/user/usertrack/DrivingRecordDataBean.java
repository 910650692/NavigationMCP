package com.fy.navi.service.define.user.usertrack;

import android.os.Parcel;
import android.os.Parcelable;

public class DrivingRecordDataBean implements Parcelable {
    private String id; // 数据ID
    private int type; // 数据类型  固定 403
    private int rideRunType = -1; //
    private int timeInterval; // 时长  单位:秒
    private int runDistance; // 距离  单位:米
    private int maxSpeed; // 最快速度  单位:公里/小时
    private String startTime; //
    private String endTime; //
    private String trackFileName; // 轨迹文件名  必传
    private String startPoiName; //
    private String endPoiName; //
    private String startLocation; // 起点位置 Json串  "startLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    private String endLocation; // 终点位置 Json串
    private String maxSpeedTime;
    private String maxSpeedLocation; // 起点位置 Json串  "maxSpeedLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    private String maxSpeedPoiName; // 起点位置 Json串  "maxSpeedLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    private int updateTime; // 数据更新时间  时间戳 单位:秒
    private String trackPointsURL; // 轨迹文件url  SDK内部赋值
    private int averageSpeed; // 平均速度  单位:米/秒
    private String trackFileMd5; // 轨迹文件Md5  SDK内部赋值
    private String data; // 自定义数据 Json串
    private String filePath;

    public DrivingRecordDataBean() {
    }

    protected DrivingRecordDataBean(Parcel in) {
        id = in.readString();
        type = in.readInt();
        rideRunType = in.readInt();
        timeInterval = in.readInt();
        runDistance = in.readInt();
        maxSpeed = in.readInt();
        startTime = in.readString();
        endTime = in.readString();
        trackFileName = in.readString();
        startPoiName = in.readString();
        endPoiName = in.readString();
        startLocation = in.readString();
        endLocation = in.readString();
        maxSpeedTime = in.readString();
        maxSpeedLocation = in.readString();
        maxSpeedPoiName = in.readString();
        updateTime = in.readInt();
        trackPointsURL = in.readString();
        averageSpeed = in.readInt();
        trackFileMd5 = in.readString();
        data = in.readString();
        filePath = in.readString();
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(id);
        dest.writeInt(type);
        dest.writeInt(rideRunType);
        dest.writeInt(timeInterval);
        dest.writeInt(runDistance);
        dest.writeInt(maxSpeed);
        dest.writeString(startTime);
        dest.writeString(endTime);
        dest.writeString(trackFileName);
        dest.writeString(startPoiName);
        dest.writeString(endPoiName);
        dest.writeString(startLocation);
        dest.writeString(endLocation);
        dest.writeString(maxSpeedTime);
        dest.writeString(maxSpeedLocation);
        dest.writeString(maxSpeedPoiName);
        dest.writeInt(updateTime);
        dest.writeString(trackPointsURL);
        dest.writeInt(averageSpeed);
        dest.writeString(trackFileMd5);
        dest.writeString(data);
        dest.writeString(filePath);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public static final Creator<DrivingRecordDataBean> CREATOR = new Creator<DrivingRecordDataBean>() {
        @Override
        public DrivingRecordDataBean createFromParcel(Parcel in) {
            return new DrivingRecordDataBean(in);
        }

        @Override
        public DrivingRecordDataBean[] newArray(int size) {
            return new DrivingRecordDataBean[size];
        }
    };

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
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

    public String getStartTime() {
        return startTime;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    public String getEndTime() {
        return endTime;
    }

    public void setEndTime(String endTime) {
        this.endTime = endTime;
    }

    public String getTrackFileName() {
        return trackFileName;
    }

    public void setTrackFileName(String trackFileName) {
        this.trackFileName = trackFileName;
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

    public String getStartLocation() {
        return startLocation;
    }

    public void setStartLocation(String startLocation) {
        this.startLocation = startLocation;
    }

    public String getEndLocation() {
        return endLocation;
    }

    public void setEndLocation(String endLocation) {
        this.endLocation = endLocation;
    }

    public String getMaxSpeedTime() {
        return maxSpeedTime;
    }

    public void setMaxSpeedTime(String maxSpeedTime) {
        this.maxSpeedTime = maxSpeedTime;
    }

    public String getMaxSpeedLocation() {
        return maxSpeedLocation;
    }

    public void setMaxSpeedLocation(String maxSpeedLocation) {
        this.maxSpeedLocation = maxSpeedLocation;
    }

    public String getMaxSpeedPoiName() {
        return maxSpeedPoiName;
    }

    public void setMaxSpeedPoiName(String maxSpeedPoiName) {
        this.maxSpeedPoiName = maxSpeedPoiName;
    }

    public int getUpdateTime() {
        return updateTime;
    }

    public void setUpdateTime(int updateTime) {
        this.updateTime = updateTime;
    }

    public String getTrackPointsURL() {
        return trackPointsURL;
    }

    public void setTrackPointsURL(String trackPointsURL) {
        this.trackPointsURL = trackPointsURL;
    }

    public int getAverageSpeed() {
        return averageSpeed;
    }

    public String getData() {
        return data;
    }

    public void setData(String data) {
        this.data = data;
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

    public void setAverageSpeed(int averageSpeed) {
        this.averageSpeed = averageSpeed;
    }

    public String getTrackFileMd5() {
        return trackFileMd5;
    }

    public void setTrackFileMd5(String trackFileMd5) {
        this.trackFileMd5 = trackFileMd5;
    }

    public String getFilePath() {
        return filePath;
    }

    public void setFilePath(String filePath) {
        this.filePath = filePath;
    }
}

