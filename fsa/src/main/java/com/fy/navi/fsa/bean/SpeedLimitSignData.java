package com.fy.navi.fsa.bean;

public class SpeedLimitSignData {
    private boolean isMapMatch;
    private boolean isAssured;
    private int speedLimit;

    public SpeedLimitSignData() {
    }

    public SpeedLimitSignData(boolean isMapMatch, boolean isAssured, int speedLimit) {
        this.isMapMatch = isMapMatch;
        this.isAssured = isAssured;
        this.speedLimit = speedLimit;
    }

    public boolean isMapMatch() {
        return isMapMatch;
    }

    public void setMapMatch(boolean mapMatch) {
        isMapMatch = mapMatch;
    }

    public boolean isAssured() {
        return isAssured;
    }

    public void setAssured(boolean assured) {
        isAssured = assured;
    }

    public int getSpeedLimit() {
        return speedLimit;
    }

    public void setSpeedLimit(int speedLimit) {
        this.speedLimit = speedLimit;
    }

    @Override
    public String toString() {
        return "SpeedLimitSignData{" +
                "isMapMatch=" + isMapMatch +
                ", isAssured=" + isAssured +
                ", speedLimit=" + speedLimit +
                '}';
    }
}
