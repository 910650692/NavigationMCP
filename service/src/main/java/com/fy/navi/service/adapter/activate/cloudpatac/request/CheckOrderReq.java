package com.fy.navi.service.adapter.activate.cloudpatac.request;

import com.patac.netlib.convert.Header;

public class CheckOrderReq extends BaseRequest {

    @Header
    private String appId;

    private String uuid;

    private String cusOrderId;

    public CheckOrderReq(final String apiVersion, final String appId, final String uuid, final String cusOrderId) {
        super(apiVersion);
        this.appId = appId;
        this.uuid = uuid;
        this.cusOrderId = cusOrderId;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public CheckOrderReq(final String apiVersion, final String idpUserId, final String appId, final String uuid, final String cusOrderId) {
        super(apiVersion, idpUserId);
        this.appId = appId;
        this.uuid = uuid;
        this.cusOrderId = cusOrderId;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }
}
