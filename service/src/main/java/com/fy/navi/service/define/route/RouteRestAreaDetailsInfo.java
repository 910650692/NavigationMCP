package com.fy.navi.service.define.route;


import java.io.Serializable;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class RouteRestAreaDetailsInfo  implements Serializable {
    private long mRemainDist;
    private long mRemainTime;
    private String mServiceName;
    private String mServicePOIID;
    private Coord2DDouble mPos;
    private long mSapaDetail;
    private boolean mIsAdded;

    public RouteRestAreaDetailsInfo() {
        this.mRemainDist = 0L;
        this.mRemainTime = 0L;
        this.mServiceName = "";
        this.mServicePOIID = "";
        this.mPos = new Coord2DDouble();
        this.mSapaDetail = 0L;
        this.mIsAdded = false;
    }

    public RouteRestAreaDetailsInfo(final long remainDistLiteObj, final long remainTimeLiteObj, final String serviceNameLiteObj
            , final String servicePOIIDLiteObj, final Coord2DDouble posLiteObj, final long sapaDetailLiteObj
            , final boolean isAddedObj) {
        this.mRemainDist = remainDistLiteObj;
        this.mRemainTime = remainTimeLiteObj;
        this.mServiceName = serviceNameLiteObj;
        this.mServicePOIID = servicePOIIDLiteObj;
        this.mPos = posLiteObj;
        this.mSapaDetail = sapaDetailLiteObj;
        this.mIsAdded = isAddedObj;
    }
}
