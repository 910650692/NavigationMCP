package com.fy.navi.service.define.route;


import java.io.Serializable;

public class RouteChargeStationDetailInfo implements Serializable {
    public int segmentIdx;
    public short direction;
    public Coord2DDouble show;
    public Coord2DDouble projective;
    public String poiID;
    public String name;
    public String brandName;
    public int maxPower;
    public short chargePercent;
    public int chargeTime;
    public int remainingCapacity;
    public double remainingPercent;
    public int index;

    public RouteChargeStationDetailInfo() {
        this.segmentIdx = 0;
        this.direction = 0;
        this.show = new Coord2DDouble();
        this.projective = new Coord2DDouble();
        this.poiID = "";
        this.name = "";
        this.brandName = "";
        this.maxPower = 0;
        this.chargePercent = 0;
        this.chargeTime = 0;
        this.remainingCapacity = 0;
        this.remainingPercent = 0.0;
        this.index = -1;
    }

    public RouteChargeStationDetailInfo(int segmentIdxLiteObj, short directionLiteObj, Coord2DDouble showLiteObj, Coord2DDouble projectiveLiteObj, String poiIDLiteObj, String nameLiteObj, String brandNameLiteObj, int maxPowerLiteObj, short chargePercentLiteObj, int chargeTimeLiteObj, int remainingCapacityLiteObj, double remainingPercentLiteObj, int indexLiteObj) {
        this.segmentIdx = segmentIdxLiteObj;
        this.direction = directionLiteObj;
        this.show = showLiteObj;
        this.projective = projectiveLiteObj;
        this.poiID = poiIDLiteObj;
        this.name = nameLiteObj;
        this.brandName = brandNameLiteObj;
        this.maxPower = maxPowerLiteObj;
        this.chargePercent = chargePercentLiteObj;
        this.chargeTime = chargeTimeLiteObj;
        this.remainingCapacity = remainingCapacityLiteObj;
        this.remainingPercent = remainingPercentLiteObj;
        this.index = indexLiteObj;
    }
}