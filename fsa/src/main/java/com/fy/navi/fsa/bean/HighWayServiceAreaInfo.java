package com.fy.navi.fsa.bean;

/**
 * 高速服务区
 */
public class HighWayServiceAreaInfo {
    /**
     * 服务区名称
     */
    private String name;
    /**
     * 到达服务区的时长
     */
    private int duration;
    /**
     * 到服务区的距离
     */
    private int distance;
    /**
     * 服务区所在位置
     */
    private GeoPoint position;
    /**
     * 服务区支持的服务类型
     * 0 – 无效值
     * 1 – 加油
     * 2 – 充电
     * 4 – 加气
     * 8 – 停车
     * 16 – 汽修
     * 32 – 餐饮
     * 64 – 厕所
     * 128 – 超市
     * 256 – 休闲
     */
    private int serviceTypes;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getDuration() {
        return duration;
    }

    public void setDuration(int duration) {
        this.duration = duration;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }

    public GeoPoint getPosition() {
        return position;
    }

    public void setPosition(GeoPoint position) {
        this.position = position;
    }

    public int getServiceTypes() {
        return serviceTypes;
    }

    public void setServiceTypes(int serviceTypes) {
        this.serviceTypes = serviceTypes;
    }

    @Override
    public String toString() {
        return "HighWayServiceAreaInfo{" +
                "name='" + name + '\'' +
                ", duration=" + duration +
                ", distance=" + distance +
                ", position=" + position +
                ", serviceTypes=" + serviceTypes +
                '}';
    }
}
