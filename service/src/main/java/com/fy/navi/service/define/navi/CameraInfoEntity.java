package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;

public class CameraInfoEntity {
    private long mCameraId;//大类电子眼id
    private GeoPoint mCoord2D;//电子眼坐标2d
    private int mDistance;//车距电子眼距离 单位:米
    private int mRoadClass;//电子眼所在道路的道路等级

    private long mSubCameraId;//细类电子眼id
    private int mSubType;//电子眼的详细违章类型:SubCameraExtType
    private boolean mIsMatch;//区间测速是否配对:仅对区间测速电子眼生效
    private int mSpeed;//电子眼限速 size = 0没有限速值 0车道禁行 0xff车道无数据 单位km/h

    public long getCameraId() {
        return mCameraId;
    }

    public void setCameraId(final long cameraId) {
        this.mCameraId = cameraId;
    }

    public GeoPoint getCoord2D() {
        return mCoord2D;
    }

    public void setCoord2D(final GeoPoint coord2D) {
        this.mCoord2D = coord2D;
    }

    public int getDistance() {
        return mDistance;
    }

    public void setDistance(final int distance) {
        this.mDistance = distance;
    }

    public int getRoadClass() {
        return mRoadClass;
    }

    public void setRoadClass(final int roadClass) {
        this.mRoadClass = roadClass;
    }

    public long getSubCameraId() {
        return mSubCameraId;
    }

    public void setSubCameraId(final long subCameraId) {
        this.mSubCameraId = subCameraId;
    }

    public int getSubType() {
        return mSubType;
    }

    public void setSubType(final int subType) {
        this.mSubType = subType;
    }

    public boolean isMatch() {
        return mIsMatch;
    }

    public void setMatch(final boolean match) {
        mIsMatch = match;
    }

    public int getSpeed() {
        return mSpeed;
    }

    public void setSpeed(final int speed) {
        this.mSpeed = speed;
    }
}
