package com.fy.navi.fsa.bean;

public class LaneDirection {
    private int direction;
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
