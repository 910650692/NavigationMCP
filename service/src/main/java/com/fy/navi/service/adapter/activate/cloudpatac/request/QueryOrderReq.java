package com.fy.navi.service.adapter.activate.cloudpatac.request;

import com.patac.netlib.convert.Header;

public class QueryOrderReq extends BaseRequest {
    @Header
    private String appId;

    private String uuid;

    private String sidTag;

    public QueryOrderReq(final String apiVersion, final String appId, final String uuid, final String sidTag) {
        super(apiVersion);
        this.appId = appId;
        this.uuid = uuid;
        this.sidTag = sidTag;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public QueryOrderReq(final String apiVersion, final String idpUserId, final String appId, final String uuid, final String sidTag) {
        super(apiVersion, idpUserId);
        this.appId = appId;
        this.uuid = uuid;
        this.sidTag = sidTag;
        setCheckAppKey(true);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public String getAppId() {
        return appId;
    }

    public void setAppId(final String appId) {
        this.appId = appId;
    }

    public String getUuid() {
        return uuid;
    }

    public void setUuid(final String uuid) {
        this.uuid = uuid;
    }

    public String getSidTag() {
        return sidTag;
    }

    public void setSidTag(final String sidTag) {
        this.sidTag = sidTag;
    }


}
