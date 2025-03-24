package com.fy.navi.service.define.route;


import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestTollGateDetailsInfo {
    private long mRemainDist;
    private long mRemainTime;
    private String mTollGateName;
    private Coord2DDouble mPos;

    public RouteRestTollGateDetailsInfo() {
        this.mRemainDist = 0L;
        this.mRemainTime = 0L;
        this.mTollGateName = "";
        this.mPos = new Coord2DDouble();
    }

    public RouteRestTollGateDetailsInfo(final long remainDistLiteObj, final long remainTimeLiteObj
            , final String tollGateNameLiteObj, final Coord2DDouble posLiteObj) {
        this.mRemainDist = remainDistLiteObj;
        this.mRemainTime = remainTimeLiteObj;
        this.mTollGateName = tollGateNameLiteObj;
        this.mPos = posLiteObj;
    }
}
