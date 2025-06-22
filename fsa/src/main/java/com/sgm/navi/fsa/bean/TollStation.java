package com.sgm.navi.fsa.bean;

/**
 * 高速收费站
 */
public class TollStation {
    /**
     * 收费站名称
     */
    private String name;
    /**
     * 到收费站的距离
     */
    private int distance;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getDistance() {
        return distance;
    }

    public void setDistance(int distance) {
        this.distance = distance;
    }
}
