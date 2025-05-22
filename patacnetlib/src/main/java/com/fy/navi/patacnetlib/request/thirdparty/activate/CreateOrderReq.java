package com.fy.navi.patacnetlib.request.thirdparty.activate;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

import lombok.Getter;
import lombok.Setter;

@Getter
@Setter
public class CreateOrderReq extends SuperReq {
    @Header
    private String appId;

    private String uuid;

    private String sidTag;

    public CreateOrderReq(final String apiVersion, final String appId, final String uuid, final String sidTag) {
        super(apiVersion);
        this.appId = appId;
        this.uuid = uuid;
        this.sidTag = sidTag;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public CreateOrderReq(final String apiVersion, final String idpUserId, final String appId, final String uuid, final String sidTag) {
        super(apiVersion, idpUserId);
        this.appId = appId;
        this.uuid = uuid;
        this.sidTag = sidTag;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

}
