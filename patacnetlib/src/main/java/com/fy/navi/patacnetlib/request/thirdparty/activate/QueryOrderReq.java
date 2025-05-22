package com.fy.navi.patacnetlib.request.thirdparty.activate;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class QueryOrderReq extends SuperReq {

    @Header
    private String appId;

    private String uuid;

    private String cusOrderId;

    public QueryOrderReq(final String apiVersion, final String appId, final String uuid, final String cusOrderId) {
        super(apiVersion);
        this.appId = appId;
        this.uuid = uuid;
        this.cusOrderId = cusOrderId;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public QueryOrderReq(final String apiVersion, final String idpUserId, final String appId, final String uuid, final String cusOrderId) {
        super(apiVersion, idpUserId);
        this.appId = appId;
        this.uuid = uuid;
        this.cusOrderId = cusOrderId;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }
}
