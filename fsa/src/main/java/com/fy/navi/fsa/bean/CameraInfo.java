package com.fy.navi.fsa.bean;

public class CameraInfo {
    private int id;
    private int type;
    private int remainDistance;
    private int speedLimit;
    private int averageSpeed;

    public CameraInfo() {
    }

    public CameraInfo(int id, int type, int remainDistance, int speedLimit, int averageSpeed) {
        this.id = id;
        this.type = type;
        this.remainDistance = remainDistance;
        this.speedLimit = speedLimit;
        this.averageSpeed = averageSpeed;
    }

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

    @Override
    public String toString() {
        return "CameraInfo{" +
                "id=" + id +
                ", type=" + type +
                ", remainDistance=" + remainDistance +
                ", speedLimit=" + speedLimit +
                ", averageSpeed=" + averageSpeed +
                '}';
    }
}
