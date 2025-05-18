package com.fy.navi.service.adapter.search.cloudByPatac.req;

import com.patac.netlib.bean.SuperReq;
import com.patac.netlib.convert.Header;

public class BaseReq extends SuperReq {
    public BaseReq(String apiVersion) {
        super(apiVersion);
    }

    public BaseReq(String apiVersion, String idpUserId) {
        super(apiVersion, idpUserId);
    }
}
