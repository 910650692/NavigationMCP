package com.fy.navi.service.define.position;

import com.fy.navi.service.define.bean.GeoPoint;

import java.math.BigInteger;

public class LocMMNode {

    /*** 匹配点坐标(GCJ02) @unit 度 @range [ lon == 0 && lat == 0: 无效 ]*/
    private GeoPoint matchPoint;
    /*** GCJ02下的偏移量，偏移量=原始位置点坐标(GCJ02) - 匹配点坐标(GCJ02) @unit 度 @range [ lon == 0 && lat == 0: 无效 ]*/
    private GeoPoint deltaPoint;
    /*** 道路角度，即航向。北零顺时针(0：正北；90：正东；180：正南；270：正西)*/
    private float roadAzi;
    /*** 匹配在这条道路上的概率。范围：[0-1]。所有道路的概率值累加起来为1*/
    private float probability;
    /***地图匹配反馈类型*/
    private int type;
    /***道路宽度。单位：米*/
    private int roadWidth;
    /***道路航向偏移量，含义是北零顺时针坐标系下:道路航向-DR信号航向。*/
    private float aziDiffRoad2DR;
    /***当前行驶方向与数字化方向（LINK上型点的索引方向）相同则顺行即true，与数字化方向相反则为false*/
    private boolean positiveLinkDirection;
    /***道路ID，0表示无效*/
    private BigInteger linkID;
    /***是否是GeoLine @range [ true: 是GeoLine false: 不是GeoLine ]*/
    private boolean isGeoLine;
    /***GeoLine在RouteLink的顺序 @range [uint16_t] @unit 无*/
    private int ordinalNum;
    /***到LINK的起始节点距离，单位：米*/
    private float toLinkStartDist;
    /***建筑ID。LocFeedbackType为停车场时，以下信息有效*/
    private long buildingID;
    /***当前Link类型，0-普通道路，1甬道，2出入口*/
    private byte buildingLinkType;
    /***当前所在楼层*/
    private short buildingFloor;
    /***甬道信息，buildingLinkType为1时有效*/
    private LocParkingSlope buildingSlope;

    public LocMMNode() {
    }

    public GeoPoint getMatchPoint() {
        return matchPoint;
    }

    public void setMatchPoint(GeoPoint matchPoint) {
        this.matchPoint = matchPoint;
    }

    public GeoPoint getDeltaPoint() {
        return deltaPoint;
    }

    public void setDeltaPoint(GeoPoint deltaPoint) {
        this.deltaPoint = deltaPoint;
    }

    public float getRoadAzi() {
        return roadAzi;
    }

    public void setRoadAzi(float roadAzi) {
        this.roadAzi = roadAzi;
    }

    public float getProbability() {
        return probability;
    }

    public void setProbability(float probability) {
        this.probability = probability;
    }

    public int getType() {
        return type;
    }

    public void setType(int type) {
        this.type = type;
    }

    public int getRoadWidth() {
        return roadWidth;
    }

    public void setRoadWidth(int roadWidth) {
        this.roadWidth = roadWidth;
    }

    public float getAziDiffRoad2DR() {
        return aziDiffRoad2DR;
    }

    public void setAziDiffRoad2DR(float aziDiffRoad2DR) {
        this.aziDiffRoad2DR = aziDiffRoad2DR;
    }

    public boolean isPositiveLinkDirection() {
        return positiveLinkDirection;
    }

    public void setPositiveLinkDirection(boolean positiveLinkDirection) {
        this.positiveLinkDirection = positiveLinkDirection;
    }

    public BigInteger getLinkID() {
        return linkID;
    }

    public void setLinkID(BigInteger linkID) {
        this.linkID = linkID;
    }

    public boolean isGeoLine() {
        return isGeoLine;
    }

    public void setGeoLine(boolean geoLine) {
        isGeoLine = geoLine;
    }

    public int getOrdinalNum() {
        return ordinalNum;
    }

    public void setOrdinalNum(int ordinalNum) {
        this.ordinalNum = ordinalNum;
    }

    public float getToLinkStartDist() {
        return toLinkStartDist;
    }

    public void setToLinkStartDist(float toLinkStartDist) {
        this.toLinkStartDist = toLinkStartDist;
    }

    public long getBuildingID() {
        return buildingID;
    }

    public void setBuildingID(long buildingID) {
        this.buildingID = buildingID;
    }

    public byte getBuildingLinkType() {
        return buildingLinkType;
    }

    public void setBuildingLinkType(byte buildingLinkType) {
        this.buildingLinkType = buildingLinkType;
    }

    public short getBuildingFloor() {
        return buildingFloor;
    }

    public void setBuildingFloor(short buildingFloor) {
        this.buildingFloor = buildingFloor;
    }

    public LocParkingSlope getBuildingSlope() {
        return buildingSlope;
    }

    public void setBuildingSlope(LocParkingSlope buildingSlope) {
        this.buildingSlope = buildingSlope;
    }
}
