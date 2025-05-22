package com.fy.navi.patacnetlib.request.navibean.activate;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class QueryOrderRequest {
    private String mApiVersion;
    private String mAppId;
    private String mUuid;
    private String mCusOrderId;

    public QueryOrderRequest(final String apiVersion, final String appId, final String uuid, final String cusOrderId) {
        this.mApiVersion = apiVersion;
        this.mAppId = appId;
        this.mUuid = uuid;
        this.mCusOrderId = cusOrderId;
    }
}
