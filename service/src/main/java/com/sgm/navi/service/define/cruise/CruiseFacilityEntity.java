package com.sgm.navi.service.define.cruise;

public class CruiseFacilityEntity {
    public int type;
    public int distance;
    public int limitSpeed;

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public int getLimitSpeed() {
        return limitSpeed;
    }

    public void setLimitSpeed(int limitSpeed) {
        this.limitSpeed = limitSpeed;
    }

    @Override
    public String toString() {
        return "CruiseFacilityEntity{" +
                "type=" + type +
                ", distance=" + distance +
                ", limitSpeed=" + limitSpeed +
                '}';
    }
}
