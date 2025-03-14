package com.fy.navi.fsa.bean;

public class NaviIntervalSpeedInfo {
    private int status;
    private int showType;
    private CameraInfo cameraInfo;
    private int currentSpeed;
    private boolean isOverSpeed;

    public int getStatus() {
        return status;
    }

    public void setStatus(int status) {
        this.status = status;
    }

    public int getShowType() {
        return showType;
    }

    public void setShowType(int showType) {
        this.showType = showType;
    }

    public CameraInfo getCameraInfo() {
        return cameraInfo;
    }

    public void setCameraInfo(CameraInfo cameraInfo) {
        this.cameraInfo = cameraInfo;
    }

    public int getCurrentSpeed() {
        return currentSpeed;
    }

    public void setCurrentSpeed(int currentSpeed) {
        this.currentSpeed = currentSpeed;
    }

    public boolean isOverSpeed() {
        return isOverSpeed;
    }

    public void setOverSpeed(boolean overSpeed) {
        isOverSpeed = overSpeed;
    }

    @Override
    public String toString() {
        return "NaviIntervalSpeedInfo{" +
                "status=" + status +
                ", showType=" + showType +
                ", cameraInfo=" + cameraInfo +
                ", currentSpeed=" + currentSpeed +
                ", isOverSpeed=" + isOverSpeed +
                '}';
    }
}
