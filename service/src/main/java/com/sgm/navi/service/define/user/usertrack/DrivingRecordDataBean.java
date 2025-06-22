package com.sgm.navi.service.define.user.usertrack;

import android.os.Parcel;
import android.os.Parcelable;

import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;

@JsonAdapter(DrivingRecordDataBeanAdapter.class)
public class DrivingRecordDataBean implements Parcelable {
    @SerializedName("id")
    private String mId; // 数据ID
    @SerializedName("type")
    private int mType; // 数据类型  固定 403
    @SerializedName("rideRunType")
    private int mRideRunType = -1; //
    @SerializedName("timeInterval")
    private int mTimeInterval; // 时长  单位:秒
    @SerializedName("runDistance")
    private int mRunDistance; // 距离  单位:米
    @SerializedName("maxSpeed")
    private int mMaxSpeed; // 最快速度  单位:公里/小时
    @SerializedName("startTime")
    private String mStartTime; //
    @SerializedName("endTime")
    private String mEndTime; //
    @SerializedName("trackFileName")
    private String mTrackFileName; // 轨迹文件名  必传
    @SerializedName("startPoiName")
    private String mStartPoiName; //
    @SerializedName("endPoiName")
    private String mEndPoiName; //
    @SerializedName("startLocation")
    private String mStartLocation; // 起点位置 Json串  "startLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    @SerializedName("endLocation")
    private String mEndLocation; // 终点位置 Json串
    @SerializedName("maxSpeedTime")
    private String mMaxSpeedTime;
    @SerializedName("maxSpeedLocation")
    private String mMaxSpeedLocation; // 起点位置 Json串  "maxSpeedLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    @SerializedName("maxSpeedPoiName")
    private String mMaxSpeedPoiName; // 起点位置 Json串  "maxSpeedLocation":"{\"x\":\"118.184593\",\"y\":\"24.48529\"}"
    @SerializedName("updateTime")
    private int mUpdateTime; // 数据更新时间  时间戳 单位:秒
    @SerializedName("trackPointsURL")
    private String mTrackPointsURL; // 轨迹文件url  SDK内部赋值
    @SerializedName("averageSpeed")
    private int mAverageSpeed; // 平均速度  单位:米/秒
    @SerializedName("trackFileMd5")
    private String mTrackFileMd5; // 轨迹文件Md5  SDK内部赋值
    @SerializedName("data")
    private String mData; // 自定义数据 Json串
    @SerializedName("filePath")
    private String mFilePath;

    public DrivingRecordDataBean() {
    }

    protected DrivingRecordDataBean(final Parcel in) {
        mId = in.readString();
        mType = in.readInt();
        mRideRunType = in.readInt();
        mTimeInterval = in.readInt();
        mRunDistance = in.readInt();
        mMaxSpeed = in.readInt();
        mStartTime = in.readString();
        mEndTime = in.readString();
        mTrackFileName = in.readString();
        mStartPoiName = in.readString();
        mEndPoiName = in.readString();
        mStartLocation = in.readString();
        mEndLocation = in.readString();
        mMaxSpeedTime = in.readString();
        mMaxSpeedLocation = in.readString();
        mMaxSpeedPoiName = in.readString();
        mUpdateTime = in.readInt();
        mTrackPointsURL = in.readString();
        mAverageSpeed = in.readInt();
        mTrackFileMd5 = in.readString();
        mData = in.readString();
        mFilePath = in.readString();
    }

    @Override
    public void writeToParcel(final Parcel dest, final int flags) {
        dest.writeString(mId);
        dest.writeInt(mType);
        dest.writeInt(mRideRunType);
        dest.writeInt(mTimeInterval);
        dest.writeInt(mRunDistance);
        dest.writeInt(mMaxSpeed);
        dest.writeString(mStartTime);
        dest.writeString(mEndTime);
        dest.writeString(mTrackFileName);
        dest.writeString(mStartPoiName);
        dest.writeString(mEndPoiName);
        dest.writeString(mStartLocation);
        dest.writeString(mEndLocation);
        dest.writeString(mMaxSpeedTime);
        dest.writeString(mMaxSpeedLocation);
        dest.writeString(mMaxSpeedPoiName);
        dest.writeInt(mUpdateTime);
        dest.writeString(mTrackPointsURL);
        dest.writeInt(mAverageSpeed);
        dest.writeString(mTrackFileMd5);
        dest.writeString(mData);
        dest.writeString(mFilePath);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    public static final Creator<DrivingRecordDataBean> CREATOR = new Creator<DrivingRecordDataBean>() {
        @Override
        public DrivingRecordDataBean createFromParcel(final Parcel in) {
            return new DrivingRecordDataBean(in);
        }

        @Override
        public DrivingRecordDataBean[] newArray(final int size) {
            return new DrivingRecordDataBean[size];
        }
    };

    public String getId() {
        return mId;
    }

    public void setId(final String id) {
        this.mId = id;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public int getRideRunType() {
        return mRideRunType;
    }

    public void setRideRunType(final int rideRunType) {
        this.mRideRunType = rideRunType;
    }

    public int getTimeInterval() {
        return mTimeInterval;
    }

    public String getStartTime() {
        return mStartTime;
    }

    public void setStartTime(final String startTime) {
        this.mStartTime = startTime;
    }

    public String getEndTime() {
        return mEndTime;
    }

    public void setEndTime(final String endTime) {
        this.mEndTime = endTime;
    }

    public String getTrackFileName() {
        return mTrackFileName;
    }

    public void setTrackFileName(final String trackFileName) {
        this.mTrackFileName = trackFileName;
    }

    public String getStartPoiName() {
        return mStartPoiName;
    }

    public void setStartPoiName(final String startPoiName) {
        this.mStartPoiName = startPoiName;
    }

    public String getEndPoiName() {
        return mEndPoiName;
    }

    public void setEndPoiName(final String endPoiName) {
        this.mEndPoiName = endPoiName;
    }

    public String getStartLocation() {
        return mStartLocation;
    }

    public void setStartLocation(final String startLocation) {
        this.mStartLocation = startLocation;
    }

    public String getEndLocation() {
        return mEndLocation;
    }

    public void setEndLocation(final String endLocation) {
        this.mEndLocation = endLocation;
    }

    public String getMaxSpeedTime() {
        return mMaxSpeedTime;
    }

    public void setMaxSpeedTime(final String maxSpeedTime) {
        this.mMaxSpeedTime = maxSpeedTime;
    }

    public String getMaxSpeedLocation() {
        return mMaxSpeedLocation;
    }

    public void setMaxSpeedLocation(final String maxSpeedLocation) {
        this.mMaxSpeedLocation = maxSpeedLocation;
    }

    public String getMaxSpeedPoiName() {
        return mMaxSpeedPoiName;
    }

    public void setMaxSpeedPoiName(final String maxSpeedPoiName) {
        this.mMaxSpeedPoiName = maxSpeedPoiName;
    }

    public int getUpdateTime() {
        return mUpdateTime;
    }

    public void setUpdateTime(final int updateTime) {
        this.mUpdateTime = updateTime;
    }

    public String getTrackPointsURL() {
        return mTrackPointsURL;
    }

    public void setTrackPointsURL(final String trackPointsURL) {
        this.mTrackPointsURL = trackPointsURL;
    }

    public int getAverageSpeed() {
        return mAverageSpeed;
    }

    public String getData() {
        return mData;
    }

    public void setData(final String data) {
        this.mData = data;
    }

    public void setTimeInterval(final int timeInterval) {
        this.mTimeInterval = timeInterval;
    }

    public int getRunDistance() {
        return mRunDistance;
    }

    public void setRunDistance(final int runDistance) {
        this.mRunDistance = runDistance;
    }

    public int getMaxSpeed() {
        return mMaxSpeed;
    }

    public void setMaxSpeed(final int maxSpeed) {
        this.mMaxSpeed = maxSpeed;
    }

    public void setAverageSpeed(final int averageSpeed) {
        this.mAverageSpeed = averageSpeed;
    }

    public String getTrackFileMd5() {
        return mTrackFileMd5;
    }

    public void setTrackFileMd5(final String trackFileMd5) {
        this.mTrackFileMd5 = trackFileMd5;
    }

    public String getFilePath() {
        return mFilePath;
    }

    public void setFilePath(final String filePath) {
        this.mFilePath = filePath;
    }
}

