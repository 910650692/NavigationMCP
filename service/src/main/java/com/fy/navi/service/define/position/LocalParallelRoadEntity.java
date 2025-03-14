package com.fy.navi.service.define.position;


import java.math.BigInteger;

/**
 * 本地并行道路实体
 */
public class LocalParallelRoadEntity {
    // 道路的唯一标识符
    private BigInteger roadID;
    /**
     * 道路属性
     * 0：表示主路
     * 1：表示辅路
     * -1：表示其他
     */
    private int type;

    public static final int MAIN_ROAD = 0;
    public static final int SIDE_ROAD = 0;

    private int formWay;

    private int linkType;

    public BigInteger getRoadID() {
        return roadID;
    }

    public void setRoadID(BigInteger roadID) {
        this.roadID = roadID;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getFormWay() {
        return formWay;
    }

    public void setFormWay(int formWay) {
        this.formWay = formWay;
    }

    public int getLinkType() {
        return linkType;
    }

    public void setLinkType(int linkType) {
        this.linkType = linkType;
    }

    @Override
    public String toString() {
        return "LocalParallelRoad{" +
                "roadID=" + roadID +
                ", type=" + type +
                ", formWay=" + formWay +
                ", linkType=" + linkType +
                '}';
    }
}
