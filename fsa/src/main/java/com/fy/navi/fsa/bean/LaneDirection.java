package com.fy.navi.fsa.bean;

/**
 * 车道线方向信息
 */
public class LaneDirection {
    /**
     * 车道线方向
     * 0 – 无效值
     * 1 – 直行
     * 2 – 左转
     * 3 – 右转
     * 4 – 掉头
     */
    private int direction;
    /**
     * 车道方向是否可通行
     * 0 – 无相关信息
     * 1 – 可行
     * 2 – 不可行
     */
    private int validType;

    public LaneDirection() {
    }

    public LaneDirection(int direction, int validType) {
        this.direction = direction;
        this.validType = validType;
    }

    public int getDirection() {
        return direction;
    }

    public void setDirection(int direction) {
        this.direction = direction;
    }

    public int getValidType() {
        return validType;
    }

    public void setValidType(int validType) {
        this.validType = validType;
    }

    @Override
    public String toString() {
        return "LaneDirection{" +
                "direction=" + direction +
                ", validType=" + validType +
                '}';
    }
}
