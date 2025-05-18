package com.fy.navi.service.define.navi;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

public class NaviRoadFacilityEntity {
    // 设施坐标
    private GeoPoint mGeoPoint;
    // 设施类型 NaviFacilityType枚举，0:服务区，1:收费站，2:检查站
    private int mType;
    // 距离最近设施距离（单位米），若为-1则说明距离下一个设施还远，或是路上没有电子眼
    private int mDistance;

    public GeoPoint getGeoPoint() {
        return mGeoPoint;
    }

    public void setGeoPoint(final GeoPoint geoPoint) {
        this.mGeoPoint = geoPoint;
    }

    public int getType() {
        return mType;
    }

    public void setType(final int type) {
        this.mType = type;
    }

    public int getDistance() {
        return mDistance;
    }

    public void setDistance(final int distance) {
        this.mDistance = distance;
    }

    @NonNull
    @Override
    public String toString() {
        return "NaviRoadFacilityEntity{" +
                "geoPoint=" + mGeoPoint +
                ", type=" + mType +
                ", distance=" + mDistance +
                '}';
    }
}
