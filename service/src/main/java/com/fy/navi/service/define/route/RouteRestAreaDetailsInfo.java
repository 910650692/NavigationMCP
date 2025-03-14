package com.fy.navi.service.define.route;


import java.io.Serializable;

public class RouteRestAreaDetailsInfo  implements Serializable {
    public long remainDist;
    public long remainTime;
    public String serviceName;
    public String servicePOIID;
    public Coord2DDouble pos;
    public long sapaDetail;

    public boolean isAdded;

    public RouteRestAreaDetailsInfo() {
        this.remainDist = 0L;
        this.remainTime = 0L;
        this.serviceName = "";
        this.servicePOIID = "";
        this.pos = new Coord2DDouble();
        this.sapaDetail = 0L;
        this.isAdded = false;
    }

    public RouteRestAreaDetailsInfo(long remainDistLiteObj, long remainTimeLiteObj, String serviceNameLiteObj, String servicePOIIDLiteObj, Coord2DDouble posLiteObj, long sapaDetailLiteObj, boolean isAddedObj) {
        this.remainDist = remainDistLiteObj;
        this.remainTime = remainTimeLiteObj;
        this.serviceName = serviceNameLiteObj;
        this.servicePOIID = servicePOIIDLiteObj;
        this.pos = posLiteObj;
        this.sapaDetail = sapaDetailLiteObj;
        this.isAdded = isAddedObj;
    }
}
