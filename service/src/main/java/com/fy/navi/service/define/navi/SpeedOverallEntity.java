package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.fy.navi.service.adapter.navi.NaviConstant;

import java.util.ArrayList;

/**
 * 高速限速实体类
 * @author fy
 * @version $Revision.*$
 */
public class SpeedOverallEntity {

    /**
     * 摄像头唯一标志，预留字段，当前一直为 0
     */
    private int mId;
    /**
     * 获取摄像头类型、绿灯数量类型（0无效类型；1多灯类型；2单灯类型）
     */
    private int mType;

    @NaviConstant.SpeedType
    private int mSpeedType;

    /**
     * 进入区间测速路段后实时的区间路段剩余距离
     */
    private int mRemainDistance = 0;
    /**
     * 限速信息的限速值，单位 km/s
     */
    private int mSpeedLimit;
    /**
     * 平均速度
     */
    private int mAverageSpeed = 0;
    private ArrayList<Short> mLimitSpeedList = new ArrayList<>();

    /**
     * 区间测速电子眼长度
     */
    private int mDistance = 0;
    /**
     * 最大建议车速，单位：km/h
     */
    private long mMaxSpeed;
    /**
     * 最小建议车速，单位：km/h
     */
    private long mMinSpeed;
    /**
     * 剩余绿灯数量，单位：个
     */
    private long mLightCount;

    public int getId() {
        return mId;
    }

    public void setId(final int id) {
        this.mId = id;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public int getSpeedType() {
        return mSpeedType;
    }

    public void setSpeedType(final int speedType) {
        this.mSpeedType = speedType;
    }

    public int getRemainDistance() {
        return mRemainDistance;
    }

    public void setRemainDistance(final int remainDistance) {
        this.mRemainDistance = remainDistance;
    }

    public int getSpeedLimit() {
        return mSpeedLimit;
    }

    public void setSpeedLimit(final int speedLimit) {
        this.mSpeedLimit = speedLimit;
    }

    public int getAverageSpeed() {
        return mAverageSpeed;
    }

    public void setAverageSpeed(final int averageSpeed) {
        this.mAverageSpeed = averageSpeed;
    }

    public int getDistance() {
        return mDistance;
    }

    public void setDistance(final int distance) {
        this.mDistance = distance;
    }

    public ArrayList<Short> getLimitSpeedList() {
        return mLimitSpeedList;
    }

    public void setLimitSpeedList(final ArrayList<Short> speed) {
        this.mLimitSpeedList = speed;
    }

    public long getMaxSpeed() {
        return mMaxSpeed;
    }

    public void setMaxSpeed(final long maxSpeed) {
        this.mMaxSpeed = maxSpeed;
    }

    public long getMinSpeed() {
        return mMinSpeed;
    }

    public void setMinSpeed(final long minSpeed) {
        this.mMinSpeed = minSpeed;
    }

    public long getLightCount() {
        return mLightCount;
    }

    public void setLightCount(final long lightCount) {
        this.mLightCount = lightCount;
    }

    @NonNull
    @Override
    public String toString() {
        return "SpeedOverallEntity{" +
                "id=" + mId +
                ", type=" + mType +
                ", speedType=" + mSpeedType +
                ", remainDistance=" + mRemainDistance +
                ", speedLimit=" + mSpeedLimit +
                ", averageSpeed=" + mAverageSpeed +
                ", limitSpeedList=" + mLimitSpeedList +
                ", distance=" + mDistance +
                ", maxSpeed=" + mMaxSpeed +
                ", minSpeed=" + mMinSpeed +
                ", lightCount=" + mLightCount +
                '}';
    }
}
