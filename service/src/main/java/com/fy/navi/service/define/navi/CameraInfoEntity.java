package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;

public class CameraInfoEntity {
    private long cameraId;//大类电子眼id
    private GeoPoint coord2D;//电子眼坐标2d
    private int distance;//车距电子眼距离 单位:米
    private int roadClass;//电子眼所在道路的道路等级

    private long subCameraId;//细类电子眼id
    private int subType;//电子眼的详细违章类型:SubCameraExtType
    private boolean isMatch;//区间测速是否配对:仅对区间测速电子眼生效
    private int speed;//电子眼限速 size = 0没有限速值 0车道禁行 0xff车道无数据 单位km/h

    public long getCameraId() {
        return cameraId;
    }

    public void setCameraId(long cameraId) {
        this.cameraId = cameraId;
    }

    public GeoPoint getCoord2D() {
        return coord2D;
    }

    public void setCoord2D(GeoPoint coord2D) {
        this.coord2D = coord2D;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public int getRoadClass() {
        return roadClass;
    }

    public void setRoadClass(int roadClass) {
        this.roadClass = roadClass;
    }

    public long getSubCameraId() {
        return subCameraId;
    }

    public void setSubCameraId(long subCameraId) {
        this.subCameraId = subCameraId;
    }

    public int getSubType() {
        return subType;
    }

    public void setSubType(int subType) {
        this.subType = subType;
    }

    public boolean isMatch() {
        return isMatch;
    }

    public void setMatch(boolean match) {
        isMatch = match;
    }

    public int getSpeed() {
        return speed;
    }

    public void setSpeed(int speed) {
        this.speed = speed;
    }
}
