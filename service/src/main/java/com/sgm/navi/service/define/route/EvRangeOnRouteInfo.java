package com.sgm.navi.service.define.route;

import com.google.gson.annotations.SerializedName;
import com.sgm.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class EvRangeOnRouteInfo {
    @SerializedName("canArrived")
    private boolean mCanArrived;//能否到达
    @SerializedName("pos")
    private GeoPoint mPos;//能量耗尽点距离，如能达到为终点坐标
    @SerializedName("remainRangeDistance")
    private long mRemainRangeDistance;//能耗点距终点距离
    @SerializedName("remainCapacity")
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
