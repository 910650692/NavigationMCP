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
