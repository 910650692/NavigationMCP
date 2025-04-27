package com.fy.navi.service.define.route;


import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteChargeStationDetailInfo implements Serializable {
    private int mSegmentIdx;
    private short mDirection;
    private Coord2DDouble mShow;
    private Coord2DDouble mProjective;
    private String mPoiID;
    private String mName;
    private String mBrandName;
    private int mMaxPower;
    private short mChargePercent;
    private int mChargeTime;
    private int mRemainingCapacity;
    private double mRemainingPercent;
    private int mIndex;
    private int mInterval = -1;

    public RouteChargeStationDetailInfo() {
        this.mSegmentIdx = 0;
        this.mDirection = 0;
        this.mShow = new Coord2DDouble();
        this.mProjective = new Coord2DDouble();
        this.mPoiID = "";
        this.mName = "";
        this.mBrandName = "";
        this.mMaxPower = 0;
        this.mChargePercent = 0;
        this.mChargeTime = 0;
        this.mRemainingCapacity = 0;
        this.mRemainingPercent = 0.0;
        this.mIndex = -1;
    }

    public RouteChargeStationDetailInfo(final int segmentIdxLiteObj, final short directionLiteObj, final Coord2DDouble showLiteObj
            , final Coord2DDouble projectiveLiteObj, final String poiIDLiteObj, final String nameLiteObj, final String brandNameLiteObj
            , final int maxPowerLiteObj, final short chargePercentLiteObj, final int chargeTimeLiteObj, final int remainingCapacityLiteObj
            , final double remainingPercentLiteObj, final int indexLiteObj) {
        this.mSegmentIdx = segmentIdxLiteObj;
        this.mDirection = directionLiteObj;
        this.mShow = showLiteObj;
        this.mProjective = projectiveLiteObj;
        this.mPoiID = poiIDLiteObj;
        this.mName = nameLiteObj;
        this.mBrandName = brandNameLiteObj;
        this.mMaxPower = maxPowerLiteObj;
        this.mChargePercent = chargePercentLiteObj;
        this.mChargeTime = chargeTimeLiteObj;
        this.mRemainingCapacity = remainingCapacityLiteObj;
        this.mRemainingPercent = remainingPercentLiteObj;
        this.mIndex = indexLiteObj;
    }
}