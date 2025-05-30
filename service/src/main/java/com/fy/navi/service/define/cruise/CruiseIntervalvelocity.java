package com.fy.navi.service.define.cruise;

public class CruiseIntervalvelocity {
    private int startPointDist;
    private int endPointDist;
    private int speedValue;

    public int getStartPointDist() {
        return startPointDist;
    }

    public void setStartPointDist(int startPointDist) {
        this.startPointDist = startPointDist;
    }

    public int getEndPointDist() {
        return endPointDist;
    }

    public void setEndPointDist(int endPointDist) {
        this.endPointDist = endPointDist;
    }

    public int getSpeedValue() {
        return speedValue;
    }

    public void setSpeedValue(int speedValue) {
        this.speedValue = speedValue;
    }

    @Override
    public String toString() {
        return "CruiseIntervalvelocity{" +
                "startPointDist=" + startPointDist +
                ", endPointDist=" + endPointDist +
                ", speedValue=" + speedValue +
                '}';
    }
}
