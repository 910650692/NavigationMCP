package com.fy.navi.service.adapter.activate.cloudpatac.request;

import com.patac.netlib.bean.SuperReq;

public class BaseRequest extends SuperReq {

    public BaseRequest(final String apiVersion) {
        super(apiVersion);
    }

    public BaseRequest(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
    }

}
