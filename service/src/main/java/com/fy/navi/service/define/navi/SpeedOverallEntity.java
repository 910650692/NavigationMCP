package com.fy.navi.service.define.navi;

import com.fy.navi.service.adapter.navi.NaviConstant;

import java.util.ArrayList;

/**
 * 高速限速实体类
 */
public class SpeedOverallEntity {

    /**
     * 摄像头唯一标志，预留字段，当前一直为 0
     */
    private int id;
    /**
     * 获取摄像头类型、绿灯数量类型（0无效类型；1多灯类型；2单灯类型）
     */
    private int type;

    @NaviConstant.SpeedType
    private int speedType;

    /**
     * 进入区间测速路段后实时的区间路段剩余距离
     */
    private int remainDistance = 0;
    /**
     * 限速信息的限速值，单位 km/s
     */
    private int speedLimit;
    /**
     * 平均速度
     */
    private int averageSpeed = 0;
    private ArrayList<Short> limitSpeedList = new ArrayList<>();

    /**
     * 区间测速电子眼长度
     */
    private int distance = 0;
    /**
     * 最大建议车速，单位：km/h
     */
    private long maxSpeed;
    /**
     * 最小建议车速，单位：km/h
     */
    private long minSpeed;
    /**
     * 剩余绿灯数量，单位：个
     */
    private long lightCount;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getSpeedType() {
        return speedType;
    }

    public void setSpeedType(int speedType) {
        this.speedType = speedType;
    }

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getSpeedLimit() {
        return speedLimit;
    }

    public void setSpeedLimit(int speedLimit) {
        this.speedLimit = speedLimit;
    }

    public int getAverageSpeed() {
        return averageSpeed;
    }

    public void setAverageSpeed(int averageSpeed) {
        this.averageSpeed = averageSpeed;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public ArrayList<Short> getLimitSpeedList() {
        return limitSpeedList;
    }

    public void setLimitSpeedList(ArrayList<Short> speed) {
        this.limitSpeedList = speed;
    }

    public long getMaxSpeed() {
        return maxSpeed;
    }

    public void setMaxSpeed(long maxSpeed) {
        this.maxSpeed = maxSpeed;
    }

    public long getMinSpeed() {
        return minSpeed;
    }

    public void setMinSpeed(long minSpeed) {
        this.minSpeed = minSpeed;
    }

    public long getLightCount() {
        return lightCount;
    }

    public void setLightCount(long lightCount) {
        this.lightCount = lightCount;
    }
}
