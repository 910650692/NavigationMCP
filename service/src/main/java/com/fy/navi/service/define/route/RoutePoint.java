package com.fy.navi.service.define.route;

import androidx.annotation.NonNull;

import com.fy.navi.service.define.bean.GeoPoint;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/10
 */
public class RoutePoint {
    private boolean mIsDraw = true;
    private long mPathId = 0;
    private int mType;
    private GeoPoint mPos;

    public boolean isIsDraw() {
        return mIsDraw;
    }

    public void setIsDraw(boolean mIsDraw) {
        this.mIsDraw = mIsDraw;
    }

    public long getPathId() {
        return mPathId;
    }

    public void setPathId(long mPathId) {
        this.mPathId = mPathId;
    }

    public int getType() {
        return mType;
    }

    public void setType(int mType) {
        this.mType = mType;
    }

    public GeoPoint getPos() {
        return mPos;
    }

    public void setPos(GeoPoint mPos) {
        this.mPos = mPos;
    }

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
