package com.fy.navi.service.define.route;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RoutePoint {
    private boolean mIsDraw = true;
    private long mPathId = 0;
    private int mType;
    private GeoPoint mPos;


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
