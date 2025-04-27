package com.fy.navi.service.adapter.search.cloudByPatac.req;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

public class BaseReq extends SuperReq {
    @Header
    private String client_id = "";
    @Header
    private String access_token = "";
    @Header
    private String IDP_USER_ID = "";
    public BaseReq(String apiVersion) {
        super(apiVersion);
    }

    public BaseReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }

    @Override
    public boolean isCheckAppKey() {
        return false;
    }
}
