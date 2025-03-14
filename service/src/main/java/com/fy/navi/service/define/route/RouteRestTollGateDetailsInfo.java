package com.fy.navi.service.define.route;



public class RouteRestTollGateDetailsInfo {
    public long remainDist;
    public long remainTime;
    public String TollGateName;
    public Coord2DDouble pos;

    public RouteRestTollGateDetailsInfo() {
        this.remainDist = 0L;
        this.remainTime = 0L;
        this.TollGateName = "";
        this.pos = new Coord2DDouble();
    }

    public RouteRestTollGateDetailsInfo(long remainDistLiteObj, long remainTimeLiteObj, String TollGateNameLiteObj, Coord2DDouble posLiteObj) {
        this.remainDist = remainDistLiteObj;
        this.remainTime = remainTimeLiteObj;
        this.TollGateName = TollGateNameLiteObj;
        this.pos = posLiteObj;
    }
}
