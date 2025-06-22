package com.sgm.navi.service.define.position;

public class LocParkingSlope {

    /*** 甬道独有，按照行驶方向，终点连接的楼层，0为无效,1为地上1层，-1为地下1层,*/
    private short 	parkingSlopeFromFloor;

    /***甬道独有，按照行驶方向，起点连接的楼层*/
    private short 	parkingSlopeToFloor;

    /***甬道当前所在link的类型*/
    private short 	parkingSlopeType;

    /***甬道link坡长,单位米*/
    private float 	parkingSlopeLength;

    /***甬道link坡高,单位米*/
    private float 	parkingSlopeHeight;

    /***甬道link平均坡度,弧度*/
    private float 	parkingSlopeAngle;

    public LocParkingSlope() {
    }

    public short getParkingSlopeFromFloor() {
        return parkingSlopeFromFloor;
    }

    public void setParkingSlopeFromFloor(short parkingSlopeFromFloor) {
        this.parkingSlopeFromFloor = parkingSlopeFromFloor;
    }

    public short getParkingSlopeToFloor() {
        return parkingSlopeToFloor;
    }

    public void setParkingSlopeToFloor(short parkingSlopeToFloor) {
        this.parkingSlopeToFloor = parkingSlopeToFloor;
    }

    public short getParkingSlopeType() {
        return parkingSlopeType;
    }

    public void setParkingSlopeType(short parkingSlopeType) {
        this.parkingSlopeType = parkingSlopeType;
    }

    public float getParkingSlopeLength() {
        return parkingSlopeLength;
    }

    public void setParkingSlopeLength(float parkingSlopeLength) {
        this.parkingSlopeLength = parkingSlopeLength;
    }

    public float getParkingSlopeHeight() {
        return parkingSlopeHeight;
    }

    public void setParkingSlopeHeight(float parkingSlopeHeight) {
        this.parkingSlopeHeight = parkingSlopeHeight;
    }

    public float getParkingSlopeAngle() {
        return parkingSlopeAngle;
    }

    public void setParkingSlopeAngle(float parkingSlopeAngle) {
        this.parkingSlopeAngle = parkingSlopeAngle;
    }
}
