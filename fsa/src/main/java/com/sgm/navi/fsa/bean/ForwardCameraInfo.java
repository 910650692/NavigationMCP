package com.sgm.navi.fsa.bean;

/**
 * 1.1.13、获取导航态限速信息（非区间限速）
 */
public class ForwardCameraInfo {
    /**
     * type	int	摄像头类型，当前只支持限速、红绿灯、违章摄像头，取值参考CameraInfo类的type字段
     */
    private int type;
    /**
     * remainDistance	int	当前为止距离摄像头的距离
     */
    private int remainDistance;
    /**
     * speedLimit	int	限速摄像头的限速值，单位：米/秒。限速摄像头才有效
     */
    private int speedLimit;
    /**
     * isOverSpeed	boolean	是否超速，限速摄像头才有效
     */
    private boolean isOverSpeed;
    /**
     * position	GeoPoint	摄像头的位置信息
     */
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
