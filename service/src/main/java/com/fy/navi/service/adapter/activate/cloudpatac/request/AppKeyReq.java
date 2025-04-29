package com.fy.navi.service.adapter.activate.cloudpatac.request;

public class AppKeyReq extends BaseRequest {


    public AppKeyReq(final String apiVersion) {
        super(apiVersion);
    }

    public AppKeyReq(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
    }

}
