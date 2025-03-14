package com.fy.navi.service.define.navi;

import com.autonavi.gbl.common.model.Coord2DDouble;
import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * 车道线实体类
 */
public class LaneInfoEntity {
    //背景车道，巡航、导航都有效 LaneAction
    private ArrayList<Integer> backLane;
    //背景扩展车道信息，巡航、导航都有效 ExtenLaneAction
    private ArrayList<Integer> backExtenLane;
    //车道坐标点，仅巡航有效
    private GeoPoint point;

    //前景车道，仅导航有效 LaneAction
    private ArrayList<Integer> frontLane;
    //建议车道，仅在线导航有效 LaneAction
    private ArrayList<Integer> optimalLane;


    //扩展车道信息，仅在线导航有效 ExtenLaneAction
    private ArrayList<Integer> extensionLane;
    //前景分时车道类型，仅在线导航有效 LaneCategoryType
    private ArrayList<Integer> frontLaneType;
    //背景分时车道类型，仅在线导航有效 LaneCategoryType
    private ArrayList<Integer> backLaneType;
    public int segmentIdx;
    public int linkIdx;

    public ArrayList<Integer> getBackLane() {
        return backLane;
    }

    public void setBackLane(ArrayList<Integer> backLane) {
        this.backLane = backLane;
    }

    public ArrayList<Integer> getBackExtenLane() {
        return backExtenLane;
    }

    public void setBackExtenLane(ArrayList<Integer> backExtenLane) {
        this.backExtenLane = backExtenLane;
    }

    public GeoPoint getPoint() {
        return point;
    }

    public void setPoint(GeoPoint point) {
        this.point = point;
    }

    public ArrayList<Integer> getFrontLane() {
        return frontLane;
    }

    public void setFrontLane(ArrayList<Integer> frontLane) {
        this.frontLane = frontLane;
    }

    public ArrayList<Integer> getOptimalLane() {
        return optimalLane;
    }

    public void setOptimalLane(ArrayList<Integer> optimalLane) {
        this.optimalLane = optimalLane;
    }

    public ArrayList<Integer> getExtensionLane() {
        return extensionLane;
    }

    public void setExtensionLane(ArrayList<Integer> extensionLane) {
        this.extensionLane = extensionLane;
    }

    public ArrayList<Integer> getFrontLaneType() {
        return frontLaneType;
    }

    public void setFrontLaneType(ArrayList<Integer> frontLaneType) {
        this.frontLaneType = frontLaneType;
    }

    public ArrayList<Integer> getBackLaneType() {
        return backLaneType;
    }

    public void setBackLaneType(ArrayList<Integer> backLaneType) {
        this.backLaneType = backLaneType;
    }

    public int getSegmentIdx() {
        return segmentIdx;
    }

    public void setSegmentIdx(int segmentIdx) {
        this.segmentIdx = segmentIdx;
    }

    public int getLinkIdx() {
        return linkIdx;
    }

    public void setLinkIdx(int linkIdx) {
        this.linkIdx = linkIdx;
    }
}
