package com.fy.navi.fsa.bean;

/**
 * 1.1.12、获取续航里程信息
 */
public class EvRangeOnRouteInfo {
    /**
     * canArrived	boolean	是否可以到达终点
     */
    private boolean canArrived;
    /**
     * pos	GeoPoint	如果可以抵达终点，返回终点坐标，如果不可以抵达终点，返回可以到达的坐标
     */
    private GeoPoint pos;
    /**
     * remainRangeDistance	int	到达终点后剩余续航里程（km），只有在可以抵达终点的时候才有效
     */
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
