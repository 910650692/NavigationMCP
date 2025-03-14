package com.fy.navi.fsa.bean;

public class ForwardCameraInfo {
    private int type;
    private int remainDistance;
    private int speedLimit;
    private boolean isOverSpeed;
    private GeoPoint position;

    public ForwardCameraInfo() {
    }

    public ForwardCameraInfo(int type, int remainDistance, int speedLimit, boolean isOverSpeed, GeoPoint position) {
        this.type = type;
        this.remainDistance = remainDistance;
        this.speedLimit = speedLimit;
        this.isOverSpeed = isOverSpeed;
        this.position = position;
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

    public boolean isOverSpeed() {
        return isOverSpeed;
    }

    public void setOverSpeed(boolean overSpeed) {
        isOverSpeed = overSpeed;
    }

    public GeoPoint getPosition() {
        return position;
    }

    public void setPosition(GeoPoint position) {
        this.position = position;
    }

    @Override
    public String toString() {
        return "ForwardCameraInfo{" +
                "type=" + type +
                ", remainDistance=" + remainDistance +
                ", speedLimit=" + speedLimit +
                ", isOverSpeed=" + isOverSpeed +
                ", position=" + position +
                '}';
    }
}
