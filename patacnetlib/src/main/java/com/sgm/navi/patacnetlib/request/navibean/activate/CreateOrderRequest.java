package com.sgm.navi.patacnetlib.request.navibean.activate;

import lombok.Getter;
import lombok.Setter;
import lombok.ToString;

@Getter
@Setter
@ToString
public class CreateOrderRequest {
    private String mApiVersion;
    private String mAppId;
    private String mUuid;
    private String mSidTag;

    public CreateOrderRequest(final String apiVersion, final String appId, final String uuid, final String sidTag) {
        this.mApiVersion = apiVersion;
        this.mAppId = appId;
        this.mUuid = uuid;
        this.mSidTag = sidTag;
    }
}
