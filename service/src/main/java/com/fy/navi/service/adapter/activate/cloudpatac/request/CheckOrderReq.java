package com.fy.navi.service.adapter.activate.cloudpatac.request;

public class CheckOrderReq extends BaseRequest {

    public CheckOrderReq(final String apiVersion) {
        super(apiVersion);
    }

    public CheckOrderReq(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
    }
}
