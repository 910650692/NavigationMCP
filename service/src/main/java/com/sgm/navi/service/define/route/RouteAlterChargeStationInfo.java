package com.sgm.navi.service.define.route;


import java.io.Serializable;
import java.util.ArrayList;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteAlterChargeStationInfo implements Serializable {
    private String mPoiId;
    private String mName;
    private Coord3DDouble mPos;
    private int mRemainingCapacity;
    private double mRemainingPercent;
    private int mChargeTime;
    private int mChildType;
    private ArrayList<String> mTagInfos;
    private RouteChargeStationNumberInfo mSuperPlugInfo;
    private RouteChargeStationNumberInfo mFastPlugInfo;
    private RouteChargeStationNumberInfo mSlowPlugInfo;
    private RouteAlterChargePriceInfo mPriceInfo;
    private RouteAlternativeChargeDetourInfo mDetourInfo;
    //替换补能界面-距离参数
    private int mDistance;

    public RouteAlterChargeStationInfo() {
        this.mPoiId = "";
        this.mName = "";
        this.mPos = new com.sgm.navi.service.define.route.Coord3DDouble();
        this.mRemainingCapacity = 0;
        this.mRemainingPercent = 0.0;
        this.mChargeTime = 0;
        this.mChildType = 0;
        this.mTagInfos = new ArrayList();
        this.mSuperPlugInfo = new RouteChargeStationNumberInfo();
        this.mFastPlugInfo = new RouteChargeStationNumberInfo();
        this.mSlowPlugInfo = new RouteChargeStationNumberInfo();
        this.mPriceInfo = new RouteAlterChargePriceInfo();
        this.mDetourInfo = new RouteAlternativeChargeDetourInfo();
    }

    public RouteAlterChargeStationInfo(final String poiIdLiteObj, final String nameLiteObj, final Coord3DDouble posLiteObj
            , final int remainingCapacityLiteObj, final double remainingPercentLiteObj, final int chargeTimeLiteObj
            , final int childTypeLiteObj, final ArrayList<String> tagInfosLiteObj
            , final RouteChargeStationNumberInfo superPlugInfoLiteObj, final RouteChargeStationNumberInfo fastPlugInfoLiteObj
            , final RouteChargeStationNumberInfo slowPlugInfoLiteObj, final RouteAlterChargePriceInfo priceInfoLiteObj
            , final RouteAlternativeChargeDetourInfo detourInfoLiteObj) {
        this.mPoiId = poiIdLiteObj;
        this.mName = nameLiteObj;
        this.mPos = posLiteObj;
        this.mRemainingCapacity = remainingCapacityLiteObj;
        this.mRemainingPercent = remainingPercentLiteObj;
        this.mChargeTime = chargeTimeLiteObj;
        this.mChildType = childTypeLiteObj;
        this.mTagInfos = tagInfosLiteObj;
        this.mSuperPlugInfo = superPlugInfoLiteObj;
        this.mFastPlugInfo = fastPlugInfoLiteObj;
        this.mSlowPlugInfo = slowPlugInfoLiteObj;
        this.mPriceInfo = priceInfoLiteObj;
        this.mDetourInfo = detourInfoLiteObj;
    }
}
