package com.fy.navi.fsa.bean;

public class EvRangeOnRouteInfo {
    private boolean canArrived;
    private GeoPoint pos;
    private int remainRangeDistance;

    public EvRangeOnRouteInfo() {
    }

    public EvRangeOnRouteInfo(boolean canArrived, GeoPoint pos, int remainRangeDistance) {
        this.canArrived = canArrived;
        this.pos = pos;
        this.remainRangeDistance = remainRangeDistance;
    }

    public boolean isCanArrived() {
        return canArrived;
    }

    public void setCanArrived(boolean canArrived) {
        this.canArrived = canArrived;
    }

    public GeoPoint getPos() {
        return pos;
    }

    public void setPos(GeoPoint pos) {
        this.pos = pos;
    }

    public int getRemainRangeDistance() {
        return remainRangeDistance;
    }

    public void setRemainRangeDistance(int remainRangeDistance) {
        this.remainRangeDistance = remainRangeDistance;
    }

    @Override
    public String toString() {
        return "EvRangeOnRouteInfo{" +
                "canArrived=" + canArrived +
                ", pos=" + pos +
                ", remainRangeDistance=" + remainRangeDistance +
                '}';
    }
}
