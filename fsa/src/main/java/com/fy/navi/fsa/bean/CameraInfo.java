package com.fy.navi.fsa.bean;

/**
 * 电子眼信息
 */
public class CameraInfo {
    //唯一id
    private int id;
    /**
     * 电子眼类型
     *
     *
     * CAMERA_TYPE_NONE	0	非法值
     * CAMERA_TYPE_SECURITY_MONITORING	1	摄像头-治安监控
     * CAMERA_TYPE_NO_PARKING	2	摄像头-禁止停车
     * CAMERA_TYPE_NO_ADMITTANCE	3	摄像头-禁行路段
     * CAMERA_TYPE_U_TURN_FORBIDDEN	4	摄像头-禁止掉头
     * CAMERA_TYPE_U_RIGHT_FORBIDDEN	5	摄像头-禁止右转
     * CAMERA_TYPE_U_LEFT_FORBIDDEN	6	摄像头-禁止左转
     * CAMERA_TYPE_U_TRAFFIC_LIGHT	7	摄像头-红绿灯
     * CAMERA_TYPE_PECCANCY	8	摄像头-违章
     * CAMERA_TYPE_VEHICLE_LIMITED	9	摄像头-车辆限行拍照、外地牌进京拍照
     * CAMERA_TYPE_NO_AUTO_LANE	10	摄像头-非机动车道
     * CAMERA_TYPE_ONE_WAY_ROAD	11	摄像头-单行道
     * CAMERA_TYPE_EMERGENCY_LANE	12	摄像头-应急车道
     * CAMERA_TYPE_BUS_LANE	13	摄像头-公交车道
     * CAMERA_TYPE_PRESS_PHOTO	14	摄像头-压线
     * CAMERA_TYPE_HOV	15	摄像头-HOV
     * CAMERA_TYPE_SPEED_LIMIT	16	摄像头-测速
     * CAMERA_TYPE_INTEVAL_OUT	17	摄像头-区间测速脱出
     * CAMERA_TYPE_INTEVAL_IN	18	摄像头-区间测速进入
     * CAMERA_TYPE _INTEVAL_NO_PASSG_GREEN_LIGHT
     * 	19	摄像头-闯绿灯
     */
    private int type;
    //当前车辆位置距离电子眼的距离
    private int remainDistance;
    //限速值
    private int speedLimit;
    //区间测速脱出时的平均速度
    private int averageSpeed;

    public CameraInfo() {
    }

    public CameraInfo(int id, int type, int remainDistance, int speedLimit, int averageSpeed) {
        this.id = id;
        this.type = type;
        this.remainDistance = remainDistance;
        this.speedLimit = speedLimit;
        this.averageSpeed = averageSpeed;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getRemainDistance() {
        return remainDistance;
    }

    public void setRemainDistance(int remainDistance) {
        this.remainDistance = remainDistance;
    }

    public int getSpeedLimit() {
        return speedLimit;
    }

    public void setSpeedLimit(int speedLimit) {
        this.speedLimit = speedLimit;
    }

    public int getAverageSpeed() {
        return averageSpeed;
    }

    public void setAverageSpeed(int averageSpeed) {
        this.averageSpeed = averageSpeed;
    }

    @Override
    public String toString() {
        return "CameraInfo{" +
                "id=" + id +
                ", type=" + type +
                ", remainDistance=" + remainDistance +
                ", speedLimit=" + speedLimit +
                ", averageSpeed=" + averageSpeed +
                '}';
    }
}
