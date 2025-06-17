package com.fy.navi.service.define.route;

import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EvRangeOnRouteInfo {
    private boolean mCanArrived;//能否到达
    private GeoPoint mPos;//能量耗尽点距离，如能达到为终点坐标
    private long mRemainRangeDistance;//能耗点距终点距离
    private int mRemainCapacity;//剩余电量，-1表示不能到达

    @Override
    public String toString() {
        return "RouteRange{" +
                "canArrived=" + mCanArrived +
                ", pos=" + mPos +
                ", remainRangeDistance=" + mRemainRangeDistance +
                ", mRemainCapacity=" + mRemainCapacity +
                '}';
    }
}
