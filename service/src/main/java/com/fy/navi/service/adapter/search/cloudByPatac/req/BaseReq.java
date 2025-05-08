package com.fy.navi.service.adapter.search.cloudByPatac.req;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

public class BaseReq extends SuperReq {
    @Header
    private String access_token = "";
    public BaseReq(String apiVersion) {
        super(apiVersion);
    }

    public BaseReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }

    public String getAccess_token() {
        return access_token;
    }

    public void setAccess_token(String access_token) {
        this.access_token = access_token;
    }
}
