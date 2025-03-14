package com.fy.navi.service.define.route;


import java.io.Serializable;
import java.util.ArrayList;

public class RouteAlterChargeStationInfo implements Serializable {
    public String poiId;
    public String name;
    public com.fy.navi.service.define.route.Coord3DDouble pos;
    public int remainingCapacity;
    public double remainingPercent;
    public int chargeTime;
    public int childType;
    public ArrayList<String> tagInfos;
    public RouteChargeStationNumberInfo superPlugInfo;
    public RouteChargeStationNumberInfo fastPlugInfo;
    public RouteChargeStationNumberInfo slowPlugInfo;
    public RouteAlterChargePriceInfo priceInfo;
    public RouteAlterChargePriceInfo detourInfo;

    public RouteAlterChargeStationInfo() {
        this.poiId = "";
        this.name = "";
        this.pos = new com.fy.navi.service.define.route.Coord3DDouble();
        this.remainingCapacity = 0;
        this.remainingPercent = 0.0;
        this.chargeTime = 0;
        this.childType = 0;
        this.tagInfos = new ArrayList();
        this.superPlugInfo = new RouteChargeStationNumberInfo();
        this.fastPlugInfo = new RouteChargeStationNumberInfo();
        this.slowPlugInfo = new RouteChargeStationNumberInfo();
        this.priceInfo = new RouteAlterChargePriceInfo();
        this.detourInfo = new RouteAlterChargePriceInfo();
    }

    public RouteAlterChargeStationInfo(String poiIdLiteObj, String nameLiteObj, Coord3DDouble posLiteObj, int remainingCapacityLiteObj, double remainingPercentLiteObj, int chargeTimeLiteObj, int childTypeLiteObj, ArrayList<String> tagInfosLiteObj, RouteChargeStationNumberInfo superPlugInfoLiteObj, RouteChargeStationNumberInfo fastPlugInfoLiteObj, RouteChargeStationNumberInfo slowPlugInfoLiteObj, RouteAlterChargePriceInfo priceInfoLiteObj, RouteAlterChargePriceInfo detourInfoLiteObj) {
        this.poiId = poiIdLiteObj;
        this.name = nameLiteObj;
        this.pos = posLiteObj;
        this.remainingCapacity = remainingCapacityLiteObj;
        this.remainingPercent = remainingPercentLiteObj;
        this.chargeTime = chargeTimeLiteObj;
        this.childType = childTypeLiteObj;
        this.tagInfos = tagInfosLiteObj;
        this.superPlugInfo = superPlugInfoLiteObj;
        this.fastPlugInfo = fastPlugInfoLiteObj;
        this.slowPlugInfo = slowPlugInfoLiteObj;
        this.priceInfo = priceInfoLiteObj;
        this.detourInfo = detourInfoLiteObj;
    }
}
