package com.fy.navi.service.define.route;

import com.fy.navi.service.define.bean.GeoPoint;

public class EvRangeOnRouteInfo {
    private boolean canArrived;
    private GeoPoint pos;
    private long remainRangeDistance;

    public EvRangeOnRouteInfo() {
    }

    public EvRangeOnRouteInfo(int remainRangeDistance, GeoPoint pos, boolean canArrived) {
        this.remainRangeDistance = remainRangeDistance;
        this.pos = pos;
        this.canArrived = canArrived;
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

    public long getRemainRangeDistance() {
        return remainRangeDistance;
    }

    public void setRemainRangeDistance(long remainRangeDistance) {
        this.remainRangeDistance = remainRangeDistance;
    }

    @Override
    public String toString() {
        return "RouteRange{" +
                "canArrived=" + canArrived +
                ", pos=" + pos +
                ", remainRangeDistance=" + remainRangeDistance +
                '}';
    }
}
