package com.fy.navi.service.adapter.activate.cloudpatac.request;

public class QueryOrderReq extends BaseRequest {

    public QueryOrderReq(final String apiVersion) {
        super(apiVersion);
    }

    public QueryOrderReq(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
    }
}
