package com.sgm.navi.service.adapter.search.cloudByPatac.req;

import com.patac.netlib.bean.SuperReq;

public class BaseReq extends SuperReq {
    public BaseReq(String apiVersion) {
        super(apiVersion);
    }

    public BaseReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }
}
