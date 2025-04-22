package com.fy.navi.fsa.bean;

/**
 * 获取导航态区间限速信息
 */
public class NaviIntervalSpeedInfo {
    /**
     * 区间测速状态
     * 0 – 区间测速进入
     * 1 – 区间测速脱出
     */
    private int status;
    /**
     * 导航信息显示状态
     * -1 – 无效值
     * 0 – 展示
     * 1 – 更新
     * 2 – 隐藏
     */
    private int showType;
    /**
     * 电子眼信息
     */
    private CameraInfo cameraInfo;
    /**
     * 当前速度
     */
    private int currentSpeed;
    /**
     * 当前速度是否超速
     */
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
