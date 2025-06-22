package com.sgm.navi.patacnetlib.request.thirdparty.activate;

import com.patac.netlib.bean.SuperReq;

public class AppKeyReq extends SuperReq {

    public AppKeyReq(final String apiVersion) {
        super(apiVersion);
        setCheckAppKey(false);
        setHeaderJson(true);
        setAddContentType(true);
    }

    public AppKeyReq(final String apiVersion, final String idpUserId) {
        super(apiVersion, idpUserId);
        setCheckAppKey(false);
        setHeaderJson(true);
        setAddContentType(true);
    }

}
