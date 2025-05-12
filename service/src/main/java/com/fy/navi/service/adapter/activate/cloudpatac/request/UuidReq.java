package com.fy.navi.service.adapter.activate.cloudpatac.request;


public class UuidReq extends BaseRequest {


    public UuidReq(final String apiVersion) {
        super(apiVersion);
    }

    public UuidReq(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
    }
}
