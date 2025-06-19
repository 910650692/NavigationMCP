package com.fy.navi.service.define.route;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RoutePoint implements Serializable {
    public boolean mIsDraw = true;
    public long mPathId = 0;
    public int mType;
    public GeoPoint mPos;
    public int mAddressType; // 地点类型: 0默认 , 1 替换补能点, 2 充电站（非补能规划）

    @NonNull
    @Override
    public String toString() {
        return "RoutePoint{" +
                "mIsDraw=" + mIsDraw +
                ", mPathId=" + mPathId +
                ", mType=" + mType +
                ", mPos=" + mPos +
                '}';
    }
}
