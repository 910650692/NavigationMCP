package com.fy.navi.service.define.navi;

import com.fy.navi.service.define.bean.GeoPoint;

import java.util.ArrayList;

/**
 * 车道线实体类
 * @author fy
 * @version $Revision.*$
 */
public class LaneInfoEntity {
    //背景车道，巡航、导航都有效 LaneAction
    private ArrayList<Integer> mBackLane;
    //背景扩展车道信息，巡航、导航都有效 ExtenLaneAction
    private ArrayList<Integer> mBackExtenLane;
    //车道坐标点，仅巡航有效
    private GeoPoint mPoint;

    //前景车道，仅导航有效 LaneAction
    private ArrayList<Integer> mFrontLane;
    //建议车道，仅在线导航有效 LaneAction
    private ArrayList<Integer> mOptimalLane;


    //扩展车道信息，仅在线导航有效 ExtenLaneAction
    private ArrayList<Integer> mExtensionLane;
    //前景分时车道类型，仅在线导航有效 LaneCategoryType
    private ArrayList<Integer> mFrontLaneType;
    //背景分时车道类型，仅在线导航有效 LaneCategoryType
    private ArrayList<Integer> mBackLaneType;
    private int mSegmentIdx;
    private int mLinkIdx;

    public ArrayList<Integer> getBackLane() {
        return mBackLane;
    }

    public void setBackLane(final ArrayList<Integer> backLane) {
        this.mBackLane = backLane;
    }

    public ArrayList<Integer> getBackExtenLane() {
        return mBackExtenLane;
    }

    public void setBackExtenLane(final ArrayList<Integer> backExtenLane) {
        this.mBackExtenLane = backExtenLane;
    }

    public GeoPoint getPoint() {
        return mPoint;
    }

    public void setPoint(final GeoPoint point) {
        this.mPoint = point;
    }

    public ArrayList<Integer> getFrontLane() {
        return mFrontLane;
    }

    public void setFrontLane(final ArrayList<Integer> frontLane) {
        this.mFrontLane = frontLane;
    }

    public ArrayList<Integer> getOptimalLane() {
        return mOptimalLane;
    }

    public void setOptimalLane(final ArrayList<Integer> optimalLane) {
        this.mOptimalLane = optimalLane;
    }

    public ArrayList<Integer> getExtensionLane() {
        return mExtensionLane;
    }

    public void setExtensionLane(final ArrayList<Integer> extensionLane) {
        this.mExtensionLane = extensionLane;
    }

    public ArrayList<Integer> getFrontLaneType() {
        return mFrontLaneType;
    }

    public void setFrontLaneType(final ArrayList<Integer> frontLaneType) {
        this.mFrontLaneType = frontLaneType;
    }

    public ArrayList<Integer> getBackLaneType() {
        return mBackLaneType;
    }

    public void setBackLaneType(final ArrayList<Integer> backLaneType) {
        this.mBackLaneType = backLaneType;
    }

    public int getSegmentIdx() {
        return mSegmentIdx;
    }

    public void setSegmentIdx(final int segmentIdx) {
        this.mSegmentIdx = segmentIdx;
    }

    public int getLinkIdx() {
        return mLinkIdx;
    }

    public void setLinkIdx(final int linkIdx) {
        this.mLinkIdx = linkIdx;
    }
}
